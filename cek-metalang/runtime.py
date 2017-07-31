import time
from rpython.rlib import jit

class CEKError(Exception):
  def __init__(self, message):
    self.message = message
  def __str__(self):
    return self.message
class CEKMatchFailure(CEKError):
  pass
class CEKUnlessFailure(CEKError):
  def __init__(self):
    pass
class CEKDone(Exception):
  def __init__(self, result):
    self.result = result

def mkvariable(name):
  return PrimVariable(name)
class PrimVariable(cl_variable):
  def __init__(self, name):
    self.literal = name
  def pprint(self, indent):
    return self.literal

def mkint(n):
  return Integer(n)
class Integer(cl_integer):
  def __init__(self, n):
    self.value = n
  def eq(self, other):
    return isinstance(other, Integer) and self.value == other.value
  def ne(self, other):
    return not self.eq(other)
  def pprint(self, indent):
    return ' ' * indent + '%s' % self.value
def guardint(v):
  if not isinstance(v, Integer):
    raise CEKError("Expected an integer")
  return v
def zeropimpl(n):
  return UnaryPrim(n, 'zerop', lambda n: cl_true() if guardint(n).value == 0 else cl_false())
def succimpl(n):
  return UnaryPrim(n, 'succ', lambda n: Integer(guardint(n).value + 1))
def predimpl(n):
  return UnaryPrim(n, 'pred', lambda n: Integer(guardint(n).value - 1))
def addimpl(n1, n2):
  return BinaryPrim(n1, n2, '+', lambda n1, n2: Integer(guardint(n1).value + guardint(n2).value))
def subimpl(n1, n2):
  return BinaryPrim(n1, n2, '-', lambda n1, n2: Integer(guardint(n1).value - guardint(n2).value))
def multimpl(n1, n2):
  return BinaryPrim(n1, n2, '*', lambda n1, n2: Integer(guardint(n1).value * guardint(n2).value))
def ltimpl(n1, n2):
  return BinaryPrim(n1, n2, '<', lambda n1, n2: cl_true() if guardint(n1).value < guardint(n2).value else cl_false())
def eqlimpl(v1, v2):
  return BinaryPrim(v1, v2, 'equal?', lambda v1, v2: cl_true() if v1.eq(v2) else cl_false())
def numequalimpl(v1, v2):
  return BinaryPrim(v1, v2, '=', lambda v1, v2: cl_true() if v1.eq(v2) else cl_false())

def mkbox(v):
  return Box(v)
class Box(cl_v):
  def __init__(self, v):
    self.v = v
  def set(self, new_v):
    self.v = new_v
    return self.v
  def get(self):
    return self.v
  def pprint(self, indent):
    return ' ' * indent + '<box %s>' % self.v.pprint(0)
def boximpl(v):
  return UnaryPrim(v, 'box', lambda v: mkbox(v))
def unboximpl(b):
  return UnaryPrim(b, 'unbox', lambda b: b.get())
def setboximpl(b, v):
  return BinaryPrim(b, v, 'box-set!', lambda b, v: b.set(v))

class NullaryPrim(cl_e):
  def __init__(self, opname, op):
    self.opname = opname
    self.op = op
  def interpret(self, env, k):
    return _ignore_sing, env, cl_ret(self.op(), k)
  def pprint(self, indent):
    return ' ' * indent + '(p#%s)' % self.opname

class UnaryPrim(cl_e):
  def __init__(self, arg, opname, op):
    self.arg = arg
    self.opname = opname
    self.op = op
  def interpret(self, env, k):
    v = lookup(env, self.arg)
    return _ignore_sing, env, cl_ret(self.op(v), k)
  def pprint(self, indent):
    return ' ' * indent + '(p#%s %s)' % (self.opname, self.arg.pprint(0))

class BinaryPrim(cl_e):
  def __init__(self, arg1, arg2, opname, op):
    self.arg1 = arg1
    self.arg2 = arg2
    self.opname = opname
    self.op = op
  def interpret(self, env, k):
    v1 = lookup(env, self.arg1)
    v2 = lookup(env, self.arg2)
    return _ignore_sing, env, cl_ret(self.op(v1, v2), k)
  def pprint(self, indent):
    return ' ' * indent + '(p#%s %s %s)' % (self.opname, self.arg1.pprint(0), self.arg2.pprint(0))

class TernaryPrim(cl_e):
  def __init__(self, arg1, arg2, arg3, opname, op):
    self.arg1 = arg1
    self.arg2 = arg2
    self.arg3 = arg3
    self.opname = opname
    self.op = op
  def interpret(self, env, k):
    v1 = lookup(env, self.arg1)
    v2 = lookup(env, self.arg2)
    v3 = lookup(env, self.arg3)
    return _ignore_sing, env, cl_ret(self.op(v1, v2, v3), k)
  def pprint(self, indent):
    return ' ' * indent + '(p#%s %s %s %s)' % (self.opname, self.arg1.pprint(0), self.arg2.pprint(0),
                                               self.arg3.pprint(0))

class Env(cl_env):
  def __init__(self):
    pass
  def lookup(self, x):
    raise Exception("subclass responsibility")
class EmptyEnv(Env):
  def __init__(self):
    pass
  def lookup(self, y):
    raise CEKError("Variable %s not found" % y)
  def pprint(self, indent):
    return ' ' * indent + 'emptyenv'
class ExtendedEnv(Env):
  def __init__(self, x, v, e):
    self.x = x
    self.v = v
    self.e = e
  def lookup(self, y):
    if (isinstance(self.x, PrimVariable) and
        isinstance(y, PrimVariable) and
        self.x.literal == y.literal):
      return self.v
    else:
      return self.e.lookup(y)
  def pprint(self, indent):
    return ' ' * indent + '([%s -> %s], %s)' % (self.x.pprint(0), self.v.pprint(0), self.e.pprint(0))

def emptyenv():
  return EmptyEnv()
def lookup(e, x):
  result = e.lookup(x)
  if isinstance(result, Cell):
    result = result.get()
    if isinstance(result, cl_undefinedv):
      raise CEKError("%s: undefined; cannot use before initialization" % x.pprint(0))
  return result
def extend(e, xs, vs):
  result = e
  while isinstance(xs, cl_varl) and isinstance(vs, cl_vl):
    x, xs = xs.var0, xs.vars1
    v, vs = vs.v0, vs.vs1
    result = ExtendedEnv(x, v, result)
  if isinstance(xs, cl_varl) or isinstance(vs, cl_vl):
    raise CEKError("Function called with the wrong number of arguments")
  return result
def extend1(e, x, v):
  return ExtendedEnv(x, v, e)
def extendrest(e, xs, x_rest, vs):
  result = e
  while isinstance(xs, cl_varl) and isinstance(vs, cl_vl):
    x, xs = xs.var0, xs.vars1
    v, vs = vs.v0, vs.vs1
    result = ExtendedEnv(x, v, result)
  if isinstance(xs, cl_varl):
    raise CEKError("Function called with too few arguments")

  restvs_reversed = vsreverse(vs)
  rest_list = _nil_sing
  while isinstance(restvs_reversed, cl_vl):
    rest_list = cl_cons(restvs_reversed.v0, rest_list)
    restvs_reversed = restvs_reversed.vs1
  return ExtendedEnv(x_rest, rest_list, result)

def resulttovlist(result):
  if isinstance(result, cl_v):
    return cl_cons(result, _nil_sing)

  vs_reversed = vsreverse(result)
  resultlist = _nil_sing
  while isinstance(vs_reversed, cl_vl):
    resultlist = cl_cons(vs_reversed.v0, resultlist)
    vs_reversed = vs_reversed.vs1
  return resultlist

def ret(v):
  raise CEKDone(v)

def modformsreverse(mfs):
  result = _mfnil_sing

  while isinstance(mfs, cl_mf):
    result = cl_mf(mfs.modform0, result)
    mfs = mfs.modforms1
  return result

def varsreverse(vars):
  result = _varsnil_sing

  while isinstance(vars, cl_varl):
    result = cl_varl(vars.var0, result)
    vars = vars.vars1
  return result

def vsreverse(vs):
  result = _vsnil_sing
  while isinstance(vs, cl_vl):
    result = cl_vl(vs.v0, result)
    vs = vs.vs1
  return result

def printimpl(x):
  return UnaryPrim(x, 'print', lambda v: pprint(v))
def pprint(v):
  if isinstance(v, cl_clo):
    print v.l0.pprint(0)
  elif isinstance(v, cl_voidv):
    pass
  else:
    print v.pprint(0)
  return v

class Cell(cl_v):
  def __init__(self, init):
    self.val = init
  def set(self, v):
    self.val = v
    return v
  def get(self):
    return self.val
def mkcell(v):
  return Cell(v)
def setcell(var, env, v):
  cell = env.lookup(var)
  return cell.set(v)
def setcells(vars, env, vs):
  while isinstance(vars, cl_varl) and isinstance(vs, cl_vl):
    setcell(vars.var0, env, vs.v0)
    vars = vars.vars1
    vs = vs.vs1
  if not isinstance(vars, cl_varsnil) and not isinstance(vs, cl_vsnil):
    raise CEKError("Number of variables and values did not agree")
  return _voidv_sing

class String(cl_string):
  _attrs_ = ['str']
  def __init__(self, str):
    self.str = str
  def pprint(self, indent):
    return ' ' * indent + "\"%s\"" % self.str
  def eq(self, other):
    return isinstance(other, String) and self.str == other.str
  def ne(self, other):
    return not self.eq(other)
def mkstr(str):
  return String(str)
def guardstr(v):
  if not isinstance(v, String):
    raise CEKError("Expected a string")
  return v

class Vector(cl_v):
  def __init__(self, vs):
    self.vs = vs
  def ref(self, pos):
    current = self.vs
    remaining = pos

    while remaining > 0 and isinstance(current, cl_vl):
      current = current.vs1
      remaining = remaining - 1
    if isinstance(current, cl_vl):
      return current.v0
    else:
      raise CEKError("vector-ref: index %s is out of range" % pos)
  def length(self):
    result = 0
    current = self.vs
    while isinstance(current, cl_vl):
      current = current.vs1
      result = result + 1
    return result
def guardvector(v):
  if not isinstance(v, Vector):
    raise CEKError("Expected a vector")
  return v
def vectorimpl(vlist):
  return UnaryPrim(vlist, 'vector', lambda vlist: Vector(vlisttovs(vlist)))
def vecrefimpl(vec, pos):
  return BinaryPrim(vec, pos, 'vector-ref', lambda v, p: guardvector(v).ref(guardint(p).value))
def veclengthimpl(vec):
  return UnaryPrim(vec, 'vector-length', lambda v: mkint(guardvector(v).length()))

def listtovs(lst):
  lst.reverse()
  result = _vsnil_sing
  while not lst == []:
    v = lst[0]
    lst = lst[1:]
    result = cl_vl(v, result)
  return result

def vlisttovs(vlist):
  result_reversed = _vsnil_sing
  while isinstance(vlist, cl_cons):
    result_reversed = cl_vl(vlist.v0, result_reversed)
    vlist = vlist.v1
  if not isinstance(vlist, cl_nil):
    raise CEKError("apply only accepts proper lists")
  return vsreverse(result_reversed)

# no interning behavior yet
class Symbol(cl_v):
  _attrs_ = ['contents']
  def __init__(self, contents):
    self.contents = contents
  def eq(self, other):
    return isinstance(other, Symbol) and self.contents == other.contents
  def ne(self, other):
    return not self.eq(other)
  def pprint(self, indent):
    return ' '* indent + '\'%s' % self.contents
def mksymbol(var):
  return Symbol(var.literal)
def issymbolimpl(s):
  return UnaryPrim(s, "symbol?", lambda s: cl_true() if isinstance(s, Symbol) else cl_false())

from rpython.rlib import streamio as sio
# Heavily inspired by Pycket's representations, not that it's anything
# too special. I couldn't find too good of docs on sio so I checked
# their implementation for reference.
class FileOutputPort(cl_v):
  def __init__(self, file):
    self.file = file
  def write(self, string):
    self.file.write(string)
def guardfileoutputport(v):
  if not isinstance(v, FileOutputPort):
    raise CEKError("Expected a FileOutputPort but got something else")
  return v
stdout = FileOutputPort(sio.fdopen_as_stream(1, "w", buffering = 1))
def currentoutputportimpl():
  return NullaryPrim('current-output-port', lambda: stdout)
stderr = FileOutputPort(sio.fdopen_as_stream(2, "w", buffering = 1))
def currenterrorportimpl():
  return NullaryPrim('current-error-port', lambda: stderr)

def fprintfimpl(out, form, vals):
  return TernaryPrim(out, form, vals, "fprintf", fprintf)
def fprintf(out, form, vals):
  guardfileoutputport(out)
  out.write(guardstr(form).str) # XXX need to actually implement formatting
  while isinstance(vals, cl_cons):
    out.write(vals.v0.pprint(0))
    out.write("\n")
    vals = vals.v1
  return _voidv_sing

def currentsecondsimpl():
  return NullaryPrim('current-seconds', lambda: mkint(int(time.clock())))

def apply(f, args, k):
  vs = cl_vl(f, vlisttovs(args))
  return _ignore_sing, emptyenv(), cl_fn(vsreverse(vs), _esnil_sing, emptyenv(), k)

class TimeApply(cl_e):
  def __init__(self, proc, lst):
    self.proc = proc
    self.lst = lst
  def pprint(self, indent):
    return ' ' * indent + '(p#time-apply %s %s)' % (self.proc.pprint(0), self.lst.pprint(0))

  def interpret(self, env, k):
    procv = env.lookup(self.proc)
    lstv = env.lookup(self.lst)
    return apply(procv, lstv, TimeApplyK(time.clock(), k))

class ExtensionK(cl_extensionk):
  def interpretspecial(self, result):
    raise CEKError("Subclass responsibility")

class TimeApplyK(ExtensionK):
  def __init__(self, start, k):
    self.start = start
    self.k = k
  def interpretspecial(self, result):
    end = time.clock()
    ms = mkint(int((end - self.start) * 1000))
    resultlist = resulttovlist(result)

    timing_results = listtovs([resultlist, ms, ms, mkint(0)])
    return _ignore_sing, emptyenv(), cl_ret(timing_results, self.k)
  def pprint(self, indent):
    return ' ' * indent + '(timeapplyk %s %s)' % (self.start, self.k.pprint(0))

def docontinuation(extensionk, result):
  assert isinstance(extensionk, ExtensionK)
  c, e, k = extensionk.interpretspecial(result)
  return cl_conf(c, e, k)

def timeapplyimpl(proc, init):
  return TimeApply(proc, init)

driver = jit.JitDriver(reds = ['e', 'k'],
                       greens = ['c'],
                       get_printable_location=lambda c: c.pprint(0))
def run(p):
  c, e, k = init(p)
  while True:
    driver.jit_merge_point(c = c, e = e, k = k)
    # print "c: %s, e: %s, k: %s" % (c.pprint(0), e.pprint(0), k.pprint(0))
    try:
      c, e, k = c.interpret(e, k)
      if isinstance(c, cl_app):
        driver.can_enter_jit(c = c, e = e, k = k)
    except CEKDone as d:
      return d.result
    except CEKError as err:
      print "c: %s, e: %s, k: %s" % (c.pprint(0), e.pprint(0), k.pprint(0))
      print err.__str__()
      print c.pprint(0)
      return None
