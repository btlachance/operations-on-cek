class CEKError(Exception):
  def __init__(self, message):
    self.message = message
  def __str__(self):
    return repr(self.message)

def mkvariable(name):
  return PrimVariable(name)
class PrimVariable(cl_variable):
  def __init__(self, name):
    self.literal = name

class Env(cl_env):
  def __init__(self):
    pass
  def lookup(self, x):
    raise Exception("subclass responsibility")
class EmptyEnv(Env):
  def __init__(self):
    pass
  def lookup(self, y):
    raise CEKError("Variable {} not found".format(y))
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


def run(p):
  c, e, k = p, emptyenv(), cl_mt()
  while True:
    try:
      c, e, k = c.interpret(e, k)
    except CEKError as err:
      print err.__str__()
      print c
      return c
