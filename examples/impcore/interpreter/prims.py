import machine as m
import runtime as r
import envs

def funenv(eee):
  assert isinstance(eee, m.cl_eee)
  return eee.env1
def localenv(eee):
  assert isinstance(eee, m.cl_eee)
  return eee.env2
def globalenv(eee):
  assert isinstance(eee, m.cl_eee)
  return eee.env0

class UnaryPrim(m.cl_e):
  _immutable_fields_ = ['arg', 'opname', 'op', 'should_enter']
  def __init__(self, arg, opname, op):
    self.arg = arg
    self.opname = opname
    self.op = op
    self.should_enter = False
  def interpret(self, eee, k):
    try:
      e = localenv(eee)
      v = r.lookup(e, self.arg)
    except envs.VariableNotFound:
      e = globalenv(eee)
      v = r.lookup(e, self.arg)
    return m.val_ignore_sing, eee, m.cl_ret(self.op(v), k)
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
  def interpret(self, eee, k):
    try:
      e = localenv(eee)
      v1 = r.lookup(e, self.arg1)
      v2 = r.lookup(e, self.arg2)
    except envs.VariableNotFound:
      e = globalenv(eee)
      v1 = r.lookup(e, self.arg1)
      v2 = r.lookup(e, self.arg2)
    return m.val_ignore_sing, eee, m.cl_ret(self.op(v1, v2), k)
  def pprint(self, indent):
    return ' ' * indent + '(p#%s %s %s)' % (self.opname, self.arg1.pprint(0), self.arg2.pprint(0))


