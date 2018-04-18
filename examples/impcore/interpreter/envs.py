from rpython.rlib import jit
import machine as m
import runtime as r
import util as u

class VariableNotFound(Exception):
  pass

class PrimVariable(m.cl_variable):
  _immutable_fields_ = ['literal', 'can_enter']
  def __init__(self, name):
    self.literal = name
    self.can_enter = False

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

class Env(m.cl_env):
  _immutable_fields_ = ['e', 'shape']
  def lookup(self, x):
    raise Exception("subclass responsibility")

  @staticmethod
  def make(xs, args, e):
    if u.len_varl(xs) != len(args):
      raise r.CEKError("Function called with the wrong number of arguments")

    argcount = len(args)
    if argcount == 0:
      return e

    assert isinstance(xs, m.cl_varl)
    if argcount == 1:
      return Env1(xs.var0, args[0], e, xs)
    else:
      return MultiExtendedEnv(xs, args, e)

class EmptyEnv(Env):
  _immutable_fields_ = ['e', 'shape']
  def __init__(self):
    self.e = None
    self.shape = None
  def getindex(self, y):
    raise VariableNotFound("Variable %s not found" % y.pprint(0))
  def lookup(self, y):
    raise VariableNotFound("Variable %s not found" % y.pprint(0))
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
