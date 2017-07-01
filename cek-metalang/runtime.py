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
  def __eq__(self, other):
    return isinstance(other, Integer) and self.value == other.value
  def __ne__(self, other):
    return not self == other
def succimpl(n):
  return Integer(n.value + 1)
def predimpl(n):
  return Integer(n.value - 1)

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
  c, e, k = p, emptyenv(), cl_mt()
  while True:
    try:
      c, e, k = c.interpret(e, k)
    except CEKDone as d:
      return d.result
    except CEKError as err:
      print err.__str__()
      print c.pprint(0)
      return c
