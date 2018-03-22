import machine as m
import runtime as r
import envs as envs

def localenv(envs):
  return envs.env2
def globalenv(envs):
  return envs.env0

class UnaryPrim(m.cl_e):
  _immutable_fields_ = ['arg', 'opname', 'op', 'should_enter']
  def __init__(self, arg, opname, op):
    self.arg = arg
    self.opname = opname
    self.op = op
    self.should_enter = False
  def interpret(self, es, k):
    try:
      e = localenv(es)
      v = r.lookup(e, self.arg)
    except envs.VariableNotFound:
      e = globalenv(es)
      v = r.lookup(e, self.arg)
    return m.val_ignore_sing, es, m.cl_ret(self.op(v), k)
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
  def interpret(self, es, k):
    try:
      e = localenv(es)
      v1 = r.lookup(e, self.arg1)
      v2 = r.lookup(e, self.arg2)
    except envs.VariableNotFound:
      e = globalenv(es)
      v1 = r.lookup(e, self.arg1)
      v2 = r.lookup(e, self.arg2)
    return m.val_ignore_sing, es, m.cl_ret(self.op(v1, v2), k)
  def pprint(self, indent):
    return ' ' * indent + '(p#%s %s %s)' % (self.opname, self.arg1.pprint(0), self.arg2.pprint(0))


