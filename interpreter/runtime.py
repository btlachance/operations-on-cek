import time
import math
import machine as m
import pprint_hacks, can_enter_hacks, surrounding_lambda_hacks, cont_next_ast_hacks, free_vars_hacks
from pycket.callgraph import CallGraph
from pycket.hash.persistent_hash_map import make_persistent_hash_type
from rpython.rlib import jit, types, rarithmetic, objectmodel
from rpython.rlib.rbigint import rbigint
from rpython.rlib.rarithmetic import r_uint
from rpython.rlib.objectmodel import compute_hash
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

  # frees is defined here because otherwise there's a definition-time
  # circular dependency between runtime and free_vars_hacks
  def frees(self):
    return SymbolSet.singleton(self)

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

@objectmodel.always_inline
def equal(a, b):
    assert a is None or isinstance(a, PrimVariable)
    assert b is None or isinstance(b, PrimVariable)
    return a is b

@objectmodel.always_inline
def hashfun(v):
    assert v is None or isinstance(v, PrimVariable)
    return r_uint(compute_hash(v))

SymbolSet = make_persistent_hash_type(
  super = m.CEKTop,
  base = object,
  keytype = PrimVariable,
  valtype = PrimVariable,
  name = "SymbolSet",
  hashfun = hashfun,
  equal = equal)

def guardnum(v):
  if not isinstance(v, Number):
    raise CEKError("Expected a number")
  return v

class Number(m.cl_number):
  def normalize(self, v):
    raise CEKError("subclass responsibility")
  def eq(self, other):
    self, v = self.normalize(other)
    return self.eq_same(v)
  def eq_same(self, v):
    raise CEKError("Subclass responsibility")
  def ne(self, other):
    return not self.eq(other)
  def is_zero(self):
    raise CEKError("Subclass responsibility")
  def add(self, v):
    self, v = self.normalize(v)
    return self.add_same(v)
  def add_same(self, v):
    raise CEKError("Subclass responsibility")
  def sub(self, v):
    self, v = self.normalize(v)
    return self.sub_same(v)
  def sub_same(self, v):
    raise CEKError("Subclass responsibility")
  def mult(self, v):
    self, v = self.normalize(v)
    return self.mult_same(v)
  def mult_same(self, v):
    raise CEKError("Subclass responsibility")
  def div(self, v):
    self, v = self.normalize(v)
    return self.div_same(v)
  def div_same(self, v):
    raise CEKError("Subclass responsibility")
  def lt(self, v):
    self, v = self.normalize(v)
    return self.lt_same(v)
  def lt_same(self, v):
    raise CEKError("Subclass responsibility")
  def gt(self, v):
    self, v = self.normalize(v)
    return self.gt_same(v)
  def gt_same(self, v):
    raise CEKError("Subclass responsibility")
  def lteq(self, v):
    self, v = self.normalize(v)
    return self.lteq_same(v)
  def lteq_same(self, v):
    raise CEKError("Subclass responsibility")
  def gteq(self, v):
    self, v = self.normalize(v)
    return self.gteq_same(v)
  def gteq_same(self, v):
    raise CEKError("Subclass responsibility")

def mkint(n):
  return Integer(n)
def guardint(v):
  if not isinstance(v, Integer):
    raise CEKError("Expected an exact integer")
  return v
class Integer(Number):
  _immutable_fields_ = ['value']
  def __init__(self, n):
    assert isinstance(n, int)
    self.value = n
  def pprint(self, indent):
    return ' ' * indent + '%s' % self.value
  def normalize(self, v):
    if isinstance(v, Integer):
      return self, v
    if isinstance(v, Float):
      return Float(float(self.value)), v
    if isinstance(v, BigInteger):
      return BigInteger.fromint(self.value), v
    assert False
  def is_zero(self):
    return m.cl_true() if self.value == 0 else m.cl_false()
  def eq_same(self, v):
    assert isinstance(v, Integer)
    return self.value == v.value
  def add_same(self, v):
    assert isinstance(v, Integer)
    try:
      return Integer(rarithmetic.ovfcheck(self.value + v.value))
    except OverflowError:
      return BigInteger(rbigint.fromint(self.value)).add(v)
  def sub_same(self, v):
    assert isinstance(v, Integer)
    try:
      return Integer(rarithmetic.ovfcheck(self.value - v.value))
    except OverflowError:
      return BigInteger(rbigint.fromint(self.value)).sub(v)
  def mult_same(self, v):
    assert isinstance(v, Integer)
    try:
      return Integer(rarithmetic.ovfcheck(self.value * v.value))
    except OverflowError:
      return BigInteger(rbigint.fromint(self.value)).mult(v)
  def div_same(self, v):
    assert isinstance(v, Integer)
    return Float(float(self.value) / float(v.value))
  def lt_same(self, v):
    assert isinstance(v, Integer)
    return m.cl_true() if self.value < v.value else m.cl_false()
  def gt_same(self, v):
    assert isinstance(v, Integer)
    return m.cl_true() if self.value > v.value else m.cl_false()
  def lteq_same(self, v):
    assert isinstance(v, Integer)
    return m.cl_true() if self.value <= v.value else m.cl_false()
  def gteq_same(self, v):
    assert isinstance(v, Integer)
    return m.cl_true() if self.value >= v.value else m.cl_false()
  def toinexact(self):
    return Float(float(self.value))
  def quotient(self, other):
    return Integer(self.value / other.value)
  def sin(self):
    if self.value == 0:
      return self
    return Float(math.sin(float(self.value)))

class BigInteger(Number):
  _immutable_fields_ = ['value']
  def __init__(self, n):
    assert isinstance(n, rbigint)
    self.value = n

  @staticmethod
  def fromint(n):
    return BigInteger(rbigint.fromint(n))

  def pprint(self, indent):
    return ' ' * indent + '%s' % self.value
  def normalize(self, v):
    if isinstance(v, BigInteger):
      return self, v
    if isinstance(v, Integer):
      return self, BigInteger.fromint(v.value)
    if isinstance(v, Float):
      return Float(self.value.tofloat()), v
    assert False
  def is_zero(self):
    return m.cl_true() if self.value.int_eq(0) else m.cl_false()
  def eq_same(self, v):
    assert isinstance(v, BigInteger)
    return self.value.eq(v.value)
  def add_same(self, v):
    assert isinstance(v, BigInteger)
    return BigInteger(self.value.add(v.value))
  def sub_same(self, v):
    assert isinstance(v, BigInteger)
    return BigInteger(self.value.sub(v.value))
  def mult_same(self, v):
    assert isinstance(v, BigInteger)
    return BigInteger(self.value.mul(v.value))
  def div_same(self, v):
    assert isinstance(v, BigInteger)
    return Float(self.value.div(v.value).tofloat())
  def lt_same(self, v):
    assert isinstance(v, BigInteger)
    return m.cl_true() if self.value.lt(v.value) else m.cl_false()
  def gt_same(self, v):
    assert isinstance(v, BigInteger)
    return m.cl_true() if self.value.gt(v.value) else m.cl_false()
  def lteq_same(self, v):
    assert isinstance(v, BigInteger)
    return m.cl_true() if self.value.le(v.value) else m.cl_false()
  def gteq_same(self, v):
    assert isinstance(v, BigInteger)
    return m.cl_true() if self.value.ge(v.value) else m.cl_false()
  def toinexact(self):
    return Float(self.value.tofloat())
  def sin(self):
    if self.value.int_eq(0):
      return self
    return Float(math.sin(self.value.tofloat()))

def mkfloat(n):
  return Float(n)
class Float(Number):
  _immutable_fields_ = ['value']
  def __init__(self, n):
    assert isinstance(n, float)
    self.value = n
  def pprint(self, indent):
    return ' ' * indent + '%s' % self.value
  def normalize(self, v):
    if isinstance(v, Float):
      return self, v
    if isinstance(v, Integer):
      return self, Float(float(v.value))
    if isinstance(v, BigInteger):
      return self, Float(v.value.tofloat())
    assert False
  def is_zero(self):
    return m.cl_true() if self.value == 0.0 else m.cl_false()
  def eq_same(self, v):
    assert isinstance(v, Float)
    return self.value == v.value
  def add_same(self, v):
    assert isinstance(v, Float)
    return Float(self.value + v.value)
  def sub_same(self, v):
    assert isinstance(v, Float)
    return Float(self.value - v.value)
  def mult_same(self, v):
    assert isinstance(v, Float)
    return Float(self.value * v.value)
  def div_same(self, v):
    assert isinstance(v, Float)
    return Float(self.value / v.value)
  def lt_same(self, v):
    assert isinstance(v, Float)
    return m.cl_true() if self.value < v.value else m.cl_false()
  def gt_same(self, v):
    assert isinstance(v, Float)
    return m.cl_true() if self.value > v.value else m.cl_false()
  def lteq_same(self, v):
    assert isinstance(v, Float)
    return m.cl_true() if self.value <= v.value else m.cl_false()
  def gteq_same(self, v):
    assert isinstance(v, Float)
    return m.cl_true() if self.value >= v.value else m.cl_false()
  def sin(self):
    return Float(math.sin(self.value))

def variadicaddimpl(args):
  return UnaryPrim(args, '+', lambda ns: addlist(ns))

@jit.unroll_safe
def addlist(ns):
  if isinstance(ns, m.cl_nil):
    return Integer(0)
  assert isinstance(ns, m.cl_cons)
  n1, ns = ns.v0, ns.v1
  if isinstance(ns, m.cl_nil):
    return guardnum(n1).add(Integer(0))
  assert isinstance(ns, m.cl_cons)
  n2, ns = ns.v0, ns.v1
  result = guardnum(n1).add(guardnum(n2))
  if isinstance(ns, m.cl_nil):
    return result
  while isinstance(ns, m.cl_cons):
    n, ns = ns.v0, ns.v1
    result = result.add(guardnum(n))
  assert isinstance(ns, m.cl_nil)
  return result

def variadicsubimpl(args):
  return UnaryPrim(args, '-', lambda ns: sublist(ns))

@jit.unroll_safe
def sublist(ns):
  assert isinstance(ns, m.cl_cons)
  n1, ns = ns.v0, ns.v1
  if isinstance(ns, m.cl_nil):
    return Integer(0).sub(guardnum(n1))
  assert isinstance(ns, m.cl_cons)
  n2, ns = ns.v0, ns.v1
  result = guardnum(n1).sub(guardnum(n2))
  if isinstance(ns, m.cl_nil):
    return result
  while isinstance(ns, m.cl_cons):
    n, ns = ns.v0, ns.v1
    result = result.add(guardnum(n))
  assert isinstance(ns, m.cl_nil)
  return result

def variadicdivimpl(args):
  return UnaryPrim(args, '/', lambda ns: divlist(ns))

@jit.unroll_safe
def divlist(ns):
  assert isinstance(ns, m.cl_cons)
  n1, ns = ns.v0, ns.v1
  if isinstance(ns, m.cl_nil):
    return Integer(1).div(guardnum(n1))
  assert isinstance(ns, m.cl_cons)
  n2, ns = ns.v0, ns.v1
  result = guardnum(n1).div(guardnum(n2))
  if isinstance(ns, m.cl_nil):
    return result
  while isinstance(ns, m.cl_cons):
    n, ns = ns.v0, ns.v1
    result = result.div(guardnum(n))
  assert isinstance(ns, m.cl_nil)
  return result


def zeropimpl(n):
  return UnaryPrim(n, 'zerop', lambda n: guardnum(n).is_zero())
def succimpl(n):
  return UnaryPrim(n, 'succ', lambda n: guardnum(n).add(Integer(1)))
def predimpl(n):
  return UnaryPrim(n, 'pred', lambda n: guardnum(n).add(Integer(-1)))
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

def lengthimpl(n):
  return UnaryPrim(n, 'length', lambda v: guardedlength(v))

@jit.unroll_safe
def guardedlength(v):
  if isinstance(v, m.cl_nil):
    return Integer(0)
  assert isinstance(v, m.cl_cons)
  length = 0
  while isinstance(v, m.cl_cons):
    _, v = v.v0, v.v1
    length += 1
  assert isinstance(v, m.cl_nil)
  return Integer(length)

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
  _immutable_fields_ = ['opname', 'op', 'should_enter']
  def __init__(self, opname, op):
    self.opname = opname
    self.op = op
    self.should_enter = False
  def interpret(self, env, k):
    return m.val_ignore_sing, env, m.cl_ret(self.op(), k)
  def pprint(self, indent):
    return ' ' * indent + '(p#%s)' % self.opname

class UnaryPrim(m.cl_e):
  _immtuable_fields_ = ['arg', 'opname', 'op', 'should_enter']
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
  _immutable_fields_ = ['arg1', 'arg2', 'opname', 'op', 'should_enter']
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
  _immutable_fields_ = ['arg1', 'arg2', 'arg3', 'opname', 'op', 'should_enter']
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
    if len_varl(xs) != len(args):
      raise CEKError("Function called with the wrong number of arguments")

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

def varl_to_xs(varl):
  xs = []
  while isinstance(varl, m.cl_varl):
    var, varl = varl.var0, varl.vars1
    xs.append(var)
  return xs
def format_xs(xs):
  return ",".join([x.pprint(0) for x in xs])

@jit.unroll_safe
def env_for_call(clo_env, envinfo, current_env):
  # envinfo describes current_env; it's not the info for clo_env
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
  def getindex(self, y):
    raise CEKError("Variable %s not found" % y.pprint(0))
  def lookup(self, y):
    raise CEKError("Variable %s not found" % y.pprint(0))
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

  def getindex(self, y):
    x = jit.promote(self.x)
    if x is y:
      return 0
    return -1

  def lookup(self, y):
    i = self.getindex(y)
    if i == 0:
      return self.v
    else:
      return self.e.lookup(y)

@jit.unroll_safe
def len_varl(xs):
  n = 0
  while isinstance(xs, m.cl_varl):
    xs = xs.vars1
    n += 1
  assert isinstance(xs, m.cl_varsnil)
  return n

class MultiExtendedEnv(Env):
  _immutable_fields_ = ['e', 'xs', 'values[*]']
  def __init__(self, xs, values, e):
    assert isinstance(e, Env)
    self.e = e
    self.xs = self.shape = xs
    self.values = values[:]

  @jit.unroll_safe
  def getindex(self, y):
    n, xs = 0, jit.promote(self.xs)
    i = -1

    while isinstance(xs, m.cl_varl):
      x, xs = xs.var0, xs.vars1
      if x is y:
        i = n
        break
      n += 1

    return i

  def lookup(self, y):
    i = self.getindex(y)
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

def extendtoplevel(e, xs, result):
  return extend(e, xs, result, True)

def extend(e, xs, result, toplevel=False):
  if isinstance(result, m.cl_v):
    vs = [result]
  else:
    vs = vstolist(result)

  if toplevel:
    toplevel_vars.set(xs)
  return Env.make(xs, vs, e)

class ToplevelVars(object):
  _attrs_ = _immutable_fields_ = ['vars']
  vars = None

  def set(self, varl):
    vars = SymbolSet.EMPTY
    for x in varl_to_xs(varl):
      vars = vars.union(SymbolSet.singleton(x))
    self.vars = vars
  def contains_all(self, frees):
    if not self.vars:
      return False

    for x in frees.keys():
      if not self.vars.haskey(x):
        return False
    return True
toplevel_vars = ToplevelVars()

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
  _immutable_fields_ = ['v']
  def __init__(self, v):
    self.val = v
  def set(self, v):
    self.val = v
    return v
  def get(self):
    return self.val
  def pprint(self, indent):
    return ' ' * indent + '(cell %s)' % self.val.pprint(0)

class __extend__(m.cl_v):
  def promote(self):
    pass
class PromotableClosure(m.cl_clo):
  def promote(self):
    jit.promote(self)
def trypromote(v):
  v.promote()
  return v

def promotees(es):
  return jit.promote(es)

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

@jit.unroll_safe
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

class __extend__(m.cl_cons):
  def eq(self, other):
    if not isinstance(other, m.cl_cons):
      return False
    assert isinstance(other, m.cl_cons)
    return self.v0.eq(other.v0) and self.v1.eq(other.v1)

class __extend__(m.cl_nil):
  def eq(self, other):
    return isinstance(other, m.cl_nil)

class Vector(m.cl_v):
  def __init__(self, vs):
    self.vs = []

    while isinstance(vs, m.cl_vl):
      self.vs.append(vs.v0)
      vs = vs.vs1
    assert isinstance(vs, m.cl_vsnil)

  def eq(self, other):
    if not isinstance(other, Vector):
      return False
    vs, othervs = self.vs, other.vs
    if len(vs) != len(othervs):
      return False
    for i in range(len(vs)):
      if not vs[i].eq(othervs[i]):
        return False
    return True

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

@jit.unroll_safe
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

@jit.unroll_safe
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

class __extend__(m.cl_false):
  def eq(self, other):
    return isinstance(other, m.cl_false)
class __extend__(m.cl_true):
  def eq(self, other):
    return isinstance(other, m.cl_true)

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
