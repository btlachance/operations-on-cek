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

class UnaryPrimK(cl_k):
  def __init__(self, opname, op, ret):
    self.opname = opname
    self.op = op
    self.ret = ret
  def interpret(self, v, env):
    return self.op(v), env, self.ret
  def pprint(self, indent):
    return ' ' * indent + '(%s %s)' % (self.opname, self.ret.pprint(0))
class UnaryPrim(cl_e):
  def __init__(self, arg, opname, op):
    self.arg = arg
    self.opname = opname
    self.op = op
  def interpret(self, env, k):
    return self.arg, env, UnaryPrimK(self.opname, self.op, k)
  def pprint(self, indent):
    return ' ' * indent + '(%s %s)' % (self.opname, self.arg.pprint(0))

def zeropimpl(n):
  return UnaryPrim(n, 'zerop', lambda n: cl_true() if n.value == 0 else cl_false())
def succimpl(n):
  return UnaryPrim(n, 'succ', lambda n: Integer(n.value + 1))
def predimpl(n):
  return UnaryPrim(n, 'pred', lambda n: Integer(n.value - 1))

class BinaryPrimK1(cl_k):
  def __init__(self, arg2, env, k, opname, op):
    self.arg2 = arg2
    self.env = env
    self.k = k
    self.opname = opname
    self.op = op
  def interpret(self, v1, env):
    return self.arg2, self.env, BinaryPrimK2(v1, env, self.k, self.opname, self.op)
  def pprint(self, indent):
    return ' ' * indent + '(%s1 %s %s %s)' % (self.opname, self.arg2.pprint(0), self.env.pprint(0), self.k.pprint(0))
class BinaryPrimK2(cl_k):
  def __init__(self, v1, env, k, opname, op):
    self.v1 = v1
    self.env = env
    self.k = k
    self.opname = opname
    self.op = op
  def interpret(self, v2, env):
    return self.op(self.v1, v2), env, self.k
  def pprint(self, indent):
    return ' ' * indent + '(%s2 %s %s %s)' % (self.opname, self.v1.pprint(0), self.env.pprint(0), self.k.pprint(0))
class BinaryPrim(cl_e):
  def __init__(self, arg1, arg2, opname, op):
    self.arg1 = arg1
    self.arg2 = arg2
    self.opname = opname
    self.op = op
  def interpret(self, env, k):
    return self.arg1, env, BinaryPrimK1(self.arg2, env, k, self.opname, self.op)
def addimpl(n1, n2):
  return BinaryPrim(n1, n2, '+', lambda n1, n2: Integer(n1.value + n2.value))
def subimpl(n1, n2):
  return BinaryPrim(n1, n2, '-', lambda n1, n2: Integer(n1.value - n2.value))
def multimpl(n1, n2):
  return BinaryPrim(n1, n2, '*', lambda n1, n2: Integer(n1.value * n2.value))

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
class ExtendedEnv(Env):
  def __init__(self, x, v, e):
    self.x = x
    self.v = v
    self.e = e
  def lookup(self, y):
    if self.x.literal == y.literal:
      return self.v
    else:
      return self.e.lookup(y)

def emptyenv():
  return EmptyEnv()
def lookup(e, x):
  return e.lookup(x)
def extend(e, x, v):
  return ExtendedEnv(x, v, e)

def pprint(v):
  print v.pprint(0)
  return v

def ret(v):
  raise CEKDone(v)

def run(p):
  c, e, k = init(p)
  while True:
    # print "c: %s, e: %s, k: %s" % (c.pprint(0), e.pprint(0), k.pprint(0))
    try:
      c, e, k = c.interpret(e, k)
    except CEKDone as d:
      return d.result
    except CEKError as err:
      print err.__str__()
      print c.pprint(0)
      return c
