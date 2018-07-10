from rpython.rlib import jit
import machine as m

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

class EmptyEnv(m.cl_env):
  _immutable_fields_ = ['e']
  def __init__(self):
    self.e = None
  def getindex(self, y):
    raise VariableNotFound("Variable %s not found" % y.pprint(0))
  def lookup(self, y):
    raise VariableNotFound("Variable %s not found" % y.pprint(0))
  def pprint(self, indent):
    return ' ' * indent + 'emptyenv'

class Env1(m.cl_env):
  _immutable_fields_ = ['x', 'v', 'e']
  def __init__(self, x, v, e):
    assert isinstance(e, m.cl_env)
    self.x = x
    self.v = v
    self.e = e

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
