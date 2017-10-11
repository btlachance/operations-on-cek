class Env(object):
  pass
class EmptyEnv(Env):
  def lookup(x):
    raise "Variable %s not found" % x
class Env1(Env):
  def __init__(self, x, v, env):
    self.x = x
    self.v = v
    self.env = env
  def lookup(x):
    if self.x == x:
      return self.v
    else:
      return self.env.lookup(x)
