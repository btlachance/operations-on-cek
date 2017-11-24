import time
import math
import machine as m
import pprint_hacks, can_enter_hacks, surrounding_lambda_hacks, cont_next_ast_hacks
from pycket.callgraph import CallGraph
from rpython.rlib import jit, types
from rpython.rlib.signature import signature

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
  _attrs_ = ['result']
  def __init__(self, result):
    self.result = result

def mkvariable(name):
  return PrimVariable.make(name)
class PrimVariable(m.cl_variable):
  _immutable_fields_ = ['literal', 'should_enter']
  def __init__(self, name):
    self.literal = name
    self.should_enter = False
  def pprint(self, indent):
    return self.literal

  @staticmethod
  def make(name):
    var = PrimVariable.all_vars.get(name, None)
    if var is None:
      var = PrimVariable(name)
      PrimVariable.all_vars[name] = var
    return var

PrimVariable.all_vars = {}

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
  def quotient(self, other):
    return mkint(int(self.value / other.value))
  def sin(self):
    if self.value == 0:
      return mkint(0)
    return mkfloat(math.sin(self.value))

def mkfloat(n):
  return Float(n)
class Float(Number):
  _immutable_fields_ = ['value']
  def __init__(self, n):
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
  def sin(self):
    return mkfloat(math.sin(self.value))

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
def quotientimpl(m, n):
  return BinaryPrim(m, n, 'quotient', lambda m, n: guardint(m).quotient(guardint(n)))
def sinimpl(n):
  return UnaryPrim(n, 'sin', lambda n: guardnum(n).sin())

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
    self.should_enter = False
  def interpret(self, env, k):
    return m.val_ignore_sing, env, m.cl_ret(self.op(), k)
  def pprint(self, indent):
    return ' ' * indent + '(p#%s)' % self.opname

class UnaryPrim(m.cl_e):
  def __init__(self, arg, opname, op):
    self.arg = arg
    self.opname = opname
    self.op = op
    self.should_enter = False
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
    self.should_enter = False
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
    self.should_enter = False
  def interpret(self, env, k):
    v1 = lookup(env, self.arg1)
    v2 = lookup(env, self.arg2)
    v3 = lookup(env, self.arg3)
    return m.val_ignore_sing, env, m.cl_ret(self.op(v1, v2, v3), k)
  def pprint(self, indent):
    return ' ' * indent + '(p#%s %s %s %s)' % (self.opname, self.arg1.pprint(0), self.arg2.pprint(0),
                                               self.arg3.pprint(0))

class Env(m.cl_env):
  _immutable_fields_ = ['e', 'shape']
  def lookup(self, x):
    raise Exception("subclass responsibility")

  @staticmethod
  def make(xs, args, e):
    argcount = len(args)
    if argcount == 0:
      return e

    assert isinstance(xs, m.cl_varl)
    if argcount == 1:
      return Env1(xs.var0, args[0], e, xs)
    else:
      return MultiExtendedEnv(xs, args, e)

@jit.unroll_safe
def equal_varl(xs, ys):
  xs = jit.promote(xs)
  ys = jit.promote(ys)
  while isinstance(xs, m.cl_varl) and isinstance(ys, m.cl_varl):
    x, xs = xs.var0, xs.vars1
    y, ys = ys.var0, ys.vars1
    if x != y:
      return False
  return isinstance(xs, m.cl_varsnil) and isinstance(ys, m.cl_varsnil)

@jit.unroll_safe
def env_for_call(clo_env, envinfo, current_env):
  assert isinstance(clo_env, Env)
  assert isinstance(current_env, Env)

  xs = jit.promote(clo_env.shape)
  envinfo = jit.promote(envinfo)

  while isinstance(envinfo, m.cl_info):
    if equal_varl(xs, envinfo.vars0):
      if clo_env is current_env:
        return current_env
    envinfo = envinfo.envinfo1
    current_env = current_env.e
  return clo_env

callgraph = CallGraph()
def register_call(lam, callingapp, cont):
  assert isinstance(callingapp, m.cl_callingapp)
  env_arg_unused = None
  if isinstance(callingapp, m.cl_ca):
    calling_app = callingapp.a0
    assert isinstance(calling_app, m.cl_a)
  else:
    calling_app = None


  callgraph.register_call(lam, calling_app, cont, env_arg_unused)
  return lam

class EmptyEnv(Env):
  _immutable_fields_ = ['e', 'shape']
  def __init__(self):
    self.e = None
    self.shape = None
  def lookup(self, y):
    raise CEKError("Variable %s not found" % y)
  def pprint(self, indent):
    return ' ' * indent + 'emptyenv'

class Env1(Env):
  _immutable_fields_ = ['x', 'v', 'e']
  def __init__(self, x, v, e, shape):
    assert isinstance(e, Env)
    self.x = x
    self.v = v
    self.e = e
    self.shape = shape
  def lookup(self, y):
    x = jit.promote(self.x)
    if x is y:
      return self.v
    else:
      return self.e.lookup(y)

@jit.unroll_safe
def len_varl(xs):
  n = 0
  while isinstance(xs, m.cl_varl):
    x, xs = xs.var0, xs.vars1
    n += 1
  return n

class MultiExtendedEnv(Env):
  _immutable_fields_ = ['e', 'xs', 'values[*]']
  def __init__(self, xs, values, e):
    assert isinstance(e, Env)
    self.e = e
    self.xs = self.shape = jit.promote(xs)
    self.values = values[:]

    if not len_varl(self.xs) == len(self.values):
      raise CEKError("Function called with the wrong number of arguments")

  @jit.unroll_safe
  def lookup(self, y):
    n, xs = 0, jit.promote(self.xs)
    i = -1

    while isinstance(xs, m.cl_varl):
      x, xs = xs.var0, xs.vars1
      if x is y:
        i = n
        break
      n += 1

    if i != -1:
      return self.values[i]
    return self.e.lookup(y)

class OfftraceVarsAccessedInfo(object):
  def __init__(self):
    self.info = {}
  def log_lookup(self, x):
    if not jit.we_are_jitted():
      if not x in self.info:
        self.info[x] = 0
      self.info[x] += 1
offtrace_vars_info = OfftraceVarsAccessedInfo()

def emptyenv():
  return EmptyEnv()
def lookup(e, x):
  assert isinstance(x, PrimVariable)

  x = jit.promote(x)
  offtrace_vars_info.log_lookup(x)

  result = e.lookup(x)
  if isinstance(result, Cell):
    result = result.get()
    if isinstance(result, m.cl_undefinedv):
      raise CEKError("%s: undefined; cannot use before initialization" % x.pprint(0))
  return result

@jit.unroll_safe
def extendcells(e, xs):
  xs = jit.promote(xs)
  n = len_varl(xs)
  vs = [mkcell(m.val_undefinedv_sing) for i in range(n)]
  return Env.make(xs, vs, e)

def extend(e, xs, result):
  if isinstance(result, m.cl_v):
    v = result
    return Env.make(xs, [v], e)

  vs = result
  return Env.make(xs, vstolist(vs), e)

@jit.unroll_safe
# invariant: len_varl(xs) >= 1 and the last var is the rest arg
def extendrest(e, xs, vs):
  jit.promote(xs)
  fixed_argcount = len_varl(xs) - 1
  assert fixed_argcount >= 0

  args = vstolist(vs)
  if len(args) < fixed_argcount:
    raise CEKError("Function called with too few arguments")

  fixed_args = args[:fixed_argcount]
  rest_args = args[fixed_argcount:]

  rest_list = m.val_nil_sing
  for v in reversed(rest_args):
    rest_list = m.cl_cons(v, rest_list)

  return Env.make(xs, fixed_args + [rest_list], e)

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
    return ' ' * indent + '(cell %s)' % self.val.pprint(0)
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

def setcells(vars, env, result):
  if isinstance(result, m.cl_v):
    v = result
    assert isinstance(vars, m.cl_varl)
    var, vars = vars.var0, vars.vars1
    if not isinstance(vars, m.cl_varsnil):
      raise CEKError("Number of variables and values did not agree")
    return setcell(var, env, v)

  vs = result
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

@jit.unroll_safe
def vstolist(vs):
  values = []
  while isinstance(vs, m.cl_vl):
    v, vs = vs.v0, vs.vs1
    values.append(v)
  assert isinstance(vs, m.cl_vsnil)
  return values

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
  return m.val_ignore_sing, emptyenv(), m.cl_fn(vsreverse(vs), m.val_esnil_sing,
                                                emptyenv(), m.val_infoempty_sing,
                                                m.val_nocallingapp_sing, k)

class TimeApply(m.cl_e):
  def __init__(self, proc, lst):
    self.proc = proc
    self.lst = lst
    self.should_enter = False
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
  def get_next_executed_ast(self):
    return self.k.get_next_executed_ast()

def docontinuation(extensionk, result):
  assert isinstance(extensionk, ExtensionK)
  c, e, k = extensionk.interpretspecial(result)
  return m.cl_conf(c, e, k)

def timeapplyimpl(proc, init):
  return TimeApply(proc, init)

def run(p):
  c, e, k = m.init(p)
  c.set_surrounding_lambda()
  try:
    inner(c, e, k)
  except CEKDone as d:
    result = d.result
    if isinstance(result, Integer):
      return result.value
    return 0
  except CEKError as err:
    print err.__str__()
    return 1
  finally:
    formatted = ', '.join(['%s=%s' % (var.pprint(0), n) for (var,n) in offtrace_vars_info.info.items()])
    print 'offtrace lookup info: %s' % formatted
    stdout.flush()

def get_printable_location(c, prev_c):
  if c.can_enter():
    return '%s from %s' % (c.pprint(0), prev_c.pprint(0))
  return c.pprint(0)

driver = jit.JitDriver(reds = ['e', 'k'],
                       greens = ['c', 'prev_c'],
                       get_printable_location=get_printable_location)

@signature(types.any(), types.any(), types.instance(m.cl_k), returns=types.any())
def inner(c, e, k):
  # Ugh, without annotating k w/type m.cl_k the translator complains
  # about the initial value of k (k_0) not being in _forcelink. Part
  # of the reason it complains is because k_0 is known to be constant:
  # it's always the singleton value for the mt continuation. And this
  # somehow causes the _forcelink error. Anyhow, pulling this part of
  # the main loop into its own function and using signature seems to
  # make the error go away---simply asserting that k has a more
  # general type does *not* make the error go away; and without
  # pulling this into it's own function, it's hard to use signature
  # since the intial continuation is the result of a function call.

  prev_c = c
  while True:
    driver.jit_merge_point(c = c, prev_c = prev_c, e = e, k = k)
    prev_c = c if isinstance(c, m.cl_a) else prev_c
    # print "c: %s, e: %s, k: %s" % (c.pprint(0), e.pprint(0), k.pprint(0))
    c, e, k = c.interpret(e, k)
    if c.can_enter():
      driver.can_enter_jit(c = c, prev_c = prev_c, e = e, k = k)
