class W_Value(object):
  pass
class W_Integer(W_Value):
  def __init__(self, n):
    self.value = n
class W_Closure(W_Value):
  def __init__(self, lam, env):
    self.lam = lam
    self.env = env
  def call(self, v):
    pass
