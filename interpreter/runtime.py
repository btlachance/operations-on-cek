import time
import machine as m
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
class PrimVariable(m.cl_variable):
  _immutable_fields_ = ['literal']
  def __init__(self, name):
    self.literal = name
  def pprint(self, indent):
    return self.literal

def guardnum(v):
  if not isinstance(v, Number):
    raise CEKError("Expected a number")
  return v
class Number(m.cl_number):
  def _value(self):
    raise CEKError("subclass responsibility")
  def eq(self, other):
    raise CEKError("subclass responsibility")
  def ne(self, other):
    return not self.eq(other)
  def pprint(self, indent):
    if isinstance(self, Integer):
      valstr = '%s' % self._value()
    else:
      valstr = '%s' % self._value()

    return ' ' * indent + valstr
  def is_zero(self):
    return m.cl_true() if self._value() == 0 else m.cl_false()
  def add(self, v):
    raise CEKError("subclass responsibility")
  def addint(self, i):
    raise CEKError("subclass responsibility")
  def sub(self, v):
    raise CEKError("subclass responsibility")
  def subint(self, i):
    raise CEKError("subclass responibility")
  def mult(self, v):
    raise CEKError("subclass responsibility")
  def multint(self, i):
    raise CEKError("subclass responibility")
  def div(self, v):
    dividend = self._value()
    divisor = v._value()
    quotient = dividend / divisor
    return mkfloat(quotient) # ugh
  def lt(self, v):
    return m.cl_true() if self._value() < v._value() else m.cl_false()
  def gt(self, v):
    return m.cl_true() if self._value() > v._value() else m.cl_false()
  def lteq(self, v):
    return m.cl_true() if self._value() <= v._value() else m.cl_false()
  def gteq(self, v):
    return m.cl_true() if self._value() >= v._value() else m.cl_false()

def mkint(n):
  return Integer(n)
def guardint(v):
  if not isinstance(v, Integer):
    raise CEKError("Expected an exact integer")
  return v
class Integer(Number):
  _immutable_fields_ = ['value']
  def __init__(self, n):
    assert isinstance(n, int) or isinstance(n, long)
    self.value = n
  def _value(self):
    return self.value
  def eq(self, other):
    if isinstance(other, Integer):
      return self.value == other.value
    elif isinstance(other, Float):
      return self.value == other.value
    else:
      return False
  def add(self, v):
    if isinstance(v, Integer):
      return mkint(self.value + v.value)
    else:
      return v.addint(self.value)
  def addint(self, i):
    assert isinstance(i, int) or isinstance(i, long)
    return mkint(self.value + i)
  def sub(self, v):
    if isinstance(v, Integer):
      return mkint(self.value - v.value)
    else:
      return v.subint(self.value)
  def subint(self, i):
    assert isinstance(i, int) or isinstance(i, long)
    return mkint(i - self.value)
  def mult(self, v):
    if isinstance(v, Integer):
      return mkint(self.value * v.value)
    else:
      return v.multint(self.value)
  def multint(self, i):
    assert isinstance(i, int) or isinstance(i, long)
    return mkint(self.value * i)
  def toinexact(self):
    return mkfloat(float(self.value))

def mkfloat(n):
  return Float(n)
class Float(Number):
  _immutable_fields_ = ['value']
  def __init__(self, n):
    assert isinstance(n, float)
    self.value = n
  def _value(self):
    return self.value
  def eq(self, other):
    if isinstance(other, Float):
      return self.value == other.value
    elif isinstance(other, Integer):
      return self.value == other.value
    else:
      return False
  def add(self, v):
    if isinstance(v, Float):
      return mkfloat(self.value + v.value)
    else:
      return mkfloat(self.value + v._value())
  def addint(self, i):
    assert isinstance(i, int) or isinstance(i, long)
    return mkfloat(self.value + i)
  def sub(self, v):
    if isinstance(v, Float):
      return mkfloat(self.value - v.value)
    else:
      return mkfloat(self.value - v._value())
  def subint(self, i):
    assert isinstance(i, int) or isinstance(i, long)
    return mkfloat(i - self.value)
  def mult(self, v):
    if isinstance(v, Float):
      return mkfloat(self.value * v.value)
    else:
      return mkfloat(self.value * v._value())
  def multint(self, i):
    assert isinstance(i, int) or isinstance(i, long)
    return mkfloat(self.value * i)

def zeropimpl(n):
  return UnaryPrim(n, 'zerop', lambda n: guardnum(n).is_zero())
def succimpl(n):
  return UnaryPrim(n, 'succ', lambda n: guardnum(n).addint(1))
def predimpl(n):
  return UnaryPrim(n, 'pred', lambda n: guardnum(n).addint(-1))
def addimpl(n1, n2):
  return BinaryPrim(n1, n2, '+', lambda n1, n2: guardnum(n1).add(guardnum(n2)))
def subimpl(n1, n2):
  return BinaryPrim(n1, n2, '-', lambda n1, n2: guardnum(n1).sub(guardnum(n2)))
def multimpl(n1, n2):
  return BinaryPrim(n1, n2, '*', lambda n1, n2: guardnum(n1).mult(guardnum(n2)))
def ltimpl(n1, n2):
  return BinaryPrim(n1, n2, '<', lambda n1, n2: guardnum(n1).lt(guardnum(n2)))
def gtimpl(n1, n2):
  return BinaryPrim(n1, n2, '>', lambda n1, n2: guardnum(n1).gt(guardnum(n2)))
def lteqimpl(n1, n2):
  return BinaryPrim(n1, n2, '<=', lambda n1, n2: guardnum(n1).lteq(guardnum(n2)))
def gteqimpl(n1, n2):
  return BinaryPrim(n1, n2, '>=', lambda n1, n2: guardnum(n1).gteq(guardnum(n2)))
def divimpl(n1, n2):
  return BinaryPrim(n1, n2, '/', lambda n1, n2: guardnum(n1).div(guardnum(n2)))
def eqlimpl(v1, v2):
  return BinaryPrim(v1, v2, 'equal?', lambda v1, v2: m.cl_true() if v1.eq(v2) else m.cl_false())
def numequalimpl(v1, v2):
  return BinaryPrim(v1, v2, '=', lambda v1, v2: m.cl_true() if v1.eq(v2) else m.cl_false())
def exacttoinexactimpl(v):
  return UnaryPrim(v, 'exact->inexact', lambda v: guardint(v).toinexact())
def exactintegerp(v):
  return UnaryPrim(v, 'exact-integer?', lambda v: m.cl_true() if isinstance(v, Integer) else m.cl_false())
def inexactp(v):
  return UnaryPrim(v, 'inexact?', lambda n: m.cl_true() if isinstance(guardnum(n), Float) else m.cl_false())

def mkbox(v):
  return Box(v)
def guardbox(v):
  if not isinstance(v, Box):
    raise CEKError("Expected a box")
  return v
class Box(m.cl_v):
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
  return UnaryPrim(b, 'unbox', lambda b: guardbox(b).get())
def setboximpl(b, v):
  return BinaryPrim(b, v, 'box-set!', lambda b, v: guardbox(b).set(v))

class NullaryPrim(m.cl_e):
  def __init__(self, opname, op):
    self.opname = opname
    self.op = op
  def interpret(self, env, k):
    return m.val_ignore_sing, env, m.cl_ret(self.op(), k)
  def pprint(self, indent):
    return ' ' * indent + '(p#%s)' % self.opname

class UnaryPrim(m.cl_e):
  def __init__(self, arg, opname, op):
    self.arg = arg
    self.opname = opname
    self.op = op
  def interpret(self, env, k):
    v = lookup(env, self.arg)
    return m.val_ignore_sing, env, m.cl_ret(self.op(v), k)
  def pprint(self, indent):
    return ' ' * indent + '(p#%s %s)' % (self.opname, self.arg.pprint(0))

class BinaryPrim(m.cl_e):
  _immutable_fields_ = ['arg1', 'arg2', 'opname', 'op']
  def __init__(self, arg1, arg2, opname, op):
    self.arg1 = arg1
    self.arg2 = arg2
    self.opname = opname
    self.op = op
  def interpret(self, env, k):
    v1 = lookup(env, self.arg1)
    v2 = lookup(env, self.arg2)
    return m.val_ignore_sing, env, m.cl_ret(self.op(v1, v2), k)
  def pprint(self, indent):
    return ' ' * indent + '(p#%s %s %s)' % (self.opname, self.arg1.pprint(0), self.arg2.pprint(0))

class TernaryPrim(m.cl_e):
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
    return m.val_ignore_sing, env, m.cl_ret(self.op(v1, v2, v3), k)
  def pprint(self, indent):
    return ' ' * indent + '(p#%s %s %s %s)' % (self.opname, self.arg1.pprint(0), self.arg2.pprint(0),
                                               self.arg3.pprint(0))

class Env(m.cl_env):
  _immutable_fields_ = ['promote']
  def __init__(self):
    self.promote = False
  def lookup(self, x):
    raise Exception("subclass responsibility")
class EmptyEnv(Env):
  _immutable_fields_ = ['promote']
  def __init__(self):
    self.promote = False
    pass
  def lookup(self, y):
    raise CEKError("Variable %s not found" % y)
  def pprint(self, indent):
    return ' ' * indent + 'emptyenv'
class ExtendedEnv(Env):
  _immutable_fields_ = ['x', 'v', 'e', 'promote']
  def __init__(self, x, v, e):
    assert isinstance(x, PrimVariable)
    assert isinstance(e, Env)
    self.x = x
    self.v = v
    self.e = e
    self.promote = not tracing()
  def lookup(self, y):
    jit.promote(self.x)
    if self.x.literal == y.literal:
      return self.v
    else:
      if self.e.promote:
        jit.promote(self.e)
      return self.e.lookup(y)
  def pprint(self, indent):
    return ' ' * indent + '([%s -> %s], %s)' % (self.x.pprint(0), self.v.pprint(0), self.e.pprint(0))

def tracing():
  return jit.current_trace_length() >= 0

def emptyenv():
  return EmptyEnv()
def lookup(e, x):
  assert isinstance(x, PrimVariable)
  result = e.lookup(x)
  if isinstance(result, Cell):
    result = result.get()
    if isinstance(result, m.cl_undefinedv):
      raise CEKError("%s: undefined; cannot use before initialization" % x.pprint(0))
  return result

@jit.unroll_safe
def extend(e, xs, vs):
  result = e
  while isinstance(xs, m.cl_varl) and isinstance(vs, m.cl_vl):
    x, xs = xs.var0, xs.vars1
    v, vs = vs.v0, vs.vs1
    result = ExtendedEnv(x, v, result)
  if isinstance(xs, m.cl_varl) or isinstance(vs, m.cl_vl):
    raise CEKError("Function called with the wrong number of arguments")
  return result

def extend1(e, x, v):
  return ExtendedEnv(x, v, e)
def extendrest(e, xs, x_rest, vs):
  result = e
  while isinstance(xs, m.cl_varl) and isinstance(vs, m.cl_vl):
    x, xs = xs.var0, xs.vars1
    v, vs = vs.v0, vs.vs1
    result = extend1(result, x, v)
  if isinstance(xs, m.cl_varl):
    raise CEKError("Function called with too few arguments")

  restvs_reversed = vsreverse(vs)
  rest_list = m.val_nil_sing
  while isinstance(restvs_reversed, m.cl_vl):
    rest_list = m.cl_cons(restvs_reversed.v0, rest_list)
    restvs_reversed = restvs_reversed.vs1
  return extend1(result, x_rest, rest_list)

def resulttovlist(result):
  if isinstance(result, m.cl_v):
    return m.cl_cons(result, m.val_nil_sing)

  vs_reversed = vsreverse(result)
  resultlist = m.val_nil_sing
  while isinstance(vs_reversed, m.cl_vl):
    resultlist = m.cl_cons(vs_reversed.v0, resultlist)
    vs_reversed = vs_reversed.vs1
  return resultlist

def ret(v):
  raise CEKDone(v)

def modformsreverse(mfs):
  result = m.val_mfnil_sing

  while isinstance(mfs, m.cl_mf):
    result = m.cl_mf(mfs.modform0, result)
    mfs = mfs.modforms1
  return result

def varsreverse(vars):
  result = m.val_varsnil_sing

  while isinstance(vars, m.cl_varl):
    result = m.cl_varl(vars.var0, result)
    vars = vars.vars1
  return result

@jit.unroll_safe
def vsreverse(vs):
  result = m.val_vsnil_sing
  while isinstance(vs, m.cl_vl):
    result = m.cl_vl(vs.v0, result)
    vs = vs.vs1
  return result

def printimpl(x): return UnaryPrim(x, 'print', lambda v: pprint(v) and m.val_voidv_sing)
def pprint(v):
  output = stdout
  if isinstance(v, m.cl_clo):
    str = v.l0.pprint(0)
  elif isinstance(v, m.cl_voidv):
    str = ""
  else:
    str = v.pprint(0)
  fprintf(output, mkstr(str), m.val_nil_sing)
  return v

def exitimpl(x): return UnaryPrim(x, 'exit', exit)
def exit(v):
  if isinstance(v, Integer) and 1 <= v.value <= 255:
    ret(v)
  else:
    ret(Integer(0))

class Cell(m.cl_v):
  _immutable_fields_ = ['val']
  def __init__(self, init):
    self.val = init
  def set(self, v):
    self.val = v
    return v
  def get(self):
    return self.val
  def pprint(self, indent):
    v = self.val
    if isinstance(v, m.cl_clo):
      valstr = v.l0.pprint(0)
    else:
      valstr = v.pprint(0)

    return ' ' * indent + valstr
def mkcell(v):
  return Cell(v)
def setcell(var, env, v):
  # We can't use the lookup function because it handles cell
  # unwrapping; we have to instead call the environment's lookup
  # method
  assert isinstance(var, PrimVariable)
  cell = env.lookup(var)
  assert isinstance(cell, Cell)
  return cell.set(v)
def setcells(vars, env, vs):
  while isinstance(vars, m.cl_varl) and isinstance(vs, m.cl_vl):
    setcell(vars.var0, env, vs.v0)
    vars = vars.vars1
    vs = vs.vs1
  if not isinstance(vars, m.cl_varsnil) and not isinstance(vs, m.cl_vsnil):
    raise CEKError("Number of variables and values did not agree")
  return m.val_voidv_sing

class String(m.cl_string):
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

class Vector(m.cl_v):
  def __init__(self, vs):
    self.vs = []

    while isinstance(vs, m.cl_vl):
      self.vs.append(vs.v0)
      vs = vs.vs1
    assert isinstance(vs, m.cl_vsnil)

  def _checkrange(self, name, pos):
    if pos < 0 or pos >= self.length():
      raise CEKError("%s: index %s is out of range; max index: %s" % (name, pos, self.length() - 1))
  def ref(self, pos):
    self._checkrange("vector-ref", pos)
    return self.vs[pos]
  def set(self, pos, v):
    self._checkrange("vector-set!", pos)
    self.vs[pos] = v
    return m.val_voidv_sing
  def length(self):
    return len(self.vs)
  def pprint(self, indent):
    return ' ' * indent + '#(%s)' % " ".join([v.pprint(0) for v in self.vs])
def guardvector(v):
  if not isinstance(v, Vector):
    raise CEKError("Expected a vector")
  return v
def vectorimpl(vlist):
  return UnaryPrim(vlist, 'vector', lambda vlist: Vector(vlisttovs(vlist)))
def vecrefimpl(vec, pos):
  return BinaryPrim(vec, pos, 'vector-ref', lambda v, p: guardvector(v).ref(guardint(p).value))
def vecsetimpl(vec, pos, v):
  return TernaryPrim(vec, pos, v, 'vector-set!', lambda vec, p, v: guardvector(vec).set(guardint(p).value, v))
def veclengthimpl(vec):
  return UnaryPrim(vec, 'vector-length', lambda v: mkint(guardvector(v).length()))
def makevectorimpl(size, v):
  return BinaryPrim(size, v, 'make-vector', makevector)
def makevector(size_num, v):
  size = guardint(size_num).value
  if size < 0:
    raise CEKError("make-vector: expected an exact nonnegative integer")
  return Vector(listtovs(size * [v]))

def listtovs(lst):
  lst.reverse()
  result = m.val_vsnil_sing
  while not lst == []:
    v = lst[0]
    lst = lst[1:]
    result = m.cl_vl(v, result)
  return result

def vlisttovs(vlist):
  result_reversed = m.val_vsnil_sing
  while isinstance(vlist, m.cl_cons):
    result_reversed = m.cl_vl(vlist.v0, result_reversed)
    vlist = vlist.v1
  if not isinstance(vlist, m.cl_nil):
    raise CEKError("apply only accepts proper lists")
  return vsreverse(result_reversed)

# no interning behavior yet
class Symbol(m.cl_v):
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
  assert isinstance(var, PrimVariable)
  return Symbol(var.literal)
def issymbolimpl(s):
  return UnaryPrim(s, "symbol?", lambda s: m.cl_true() if isinstance(s, Symbol) else m.cl_false())

from rpython.rlib import streamio as sio
# Heavily inspired by Pycket's representations, not that it's anything
# too special. I couldn't find too good of docs on sio so I checked
# their implementation for reference.
class FileOutputPort(m.cl_v):
  def __init__(self, file):
    self.file = file
  def write(self, string):
    self.file.write(string)
  def flush(self):
    self.file.flush()
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
  while isinstance(vals, m.cl_cons):
    out.write(vals.v0.pprint(0))
    out.write("\n")
    vals = vals.v1
  return m.val_voidv_sing

def currentsecondsimpl():
  return NullaryPrim('current-seconds', lambda: mkint(int(time.clock())))

def apply(f, args, k):
  vs = m.cl_vl(f, vlisttovs(args))
  return m.val_ignore_sing, emptyenv(), m.cl_fn(vsreverse(vs), m.val_esnil_sing, emptyenv(), k)

class TimeApply(m.cl_e):
  def __init__(self, proc, lst):
    self.proc = proc
    self.lst = lst
  def pprint(self, indent):
    return ' ' * indent + '(p#time-apply %s %s)' % (self.proc.pprint(0), self.lst.pprint(0))

  def interpret(self, env, k):
    procv = lookup(env, self.proc)
    lstv = lookup(env, self.lst)
    return apply(procv, lstv, TimeApplyK(time.clock(), k))

class ExtensionK(m.cl_extensionk):
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
    return m.val_ignore_sing, emptyenv(), m.cl_ret(timing_results, self.k)
  def pprint(self, indent):
    return ' ' * indent + '(timeapplyk %s %s)' % (self.start, self.k.pprint(0))

def docontinuation(extensionk, result):
  assert isinstance(extensionk, ExtensionK)
  c, e, k = extensionk.interpretspecial(result)
  return m.cl_conf(c, e, k)

def timeapplyimpl(proc, init):
  return TimeApply(proc, init)

driver = jit.JitDriver(reds = ['e', 'k'],
                       greens = ['c', 'prev_c'],
                       get_printable_location=lambda c, prev_c: c.pprint(0))
def run(p):
  c, e, k = m.init(p)
  prev_c = c
  while True:
    driver.jit_merge_point(c = c, prev_c = prev_c, e = e, k = k)
    prev_c = c if isinstance(c, m.cl_app) else prev_c
    # prev_c = c
    # print "c: %s, e: %s, k: %s" % (c.pprint(0), e.pprint(0), k.pprint(0))
    try:
      c, e, k = c.interpret(e, k)
      if isinstance(c, m.cl_app):
        driver.can_enter_jit(c = c, prev_c = prev_c, e = e, k = k)
    except CEKDone as d:
      result = d.result
      if isinstance(result, Integer):
        return result.value
      return 0
    except CEKError as err:
      print "c: %s, e: %s, k: %s" % (c.pprint(0), e.pprint(0), k.pprint(0))
      print err.__str__()
      print c.pprint(0)
      return 1
    finally:
      stdout.flush()
