import values as v

class W_SimpleBinOp(v.W_Value):
  def __init__(self, op):
    self.op = op
  def call(self, v1, v2):
    return self.op(v1, v2)

def add(v1, v2):
  assert isinstance(v1, W_Integer)
  assert isinstance(v2, w_Integer)
  return v1.value + v2.value

PrimAdd = W_SimpleBinOp(add)
