from rpython.rlib import rarithmetic
import machine as m
import runtime as r

def guardint(v):
  if not isinstance(v, Integer):
    raise r.CEKError("Expected an integer")
  return v

def overflow():
  raise r.CEKError("Integer overflow")

class Integer(m.cl_number):
  _immutable_fields_ = ['value']
  def __init__(self, n):
    assert isinstance(n, int)
    self.value = n
  def pprint(self, indent):
    return ' ' * indent + '%s' % self.value
  def eq(self, v):
    assert isinstance(v, Integer)
    return self.value == v.value
  def add(self, v):
    assert isinstance(v, Integer)
    try:
      return Integer(rarithmetic.ovfcheck(self.value + v.value))
    except OverflowError:
      overflow()
  def sub(self, v):
    assert isinstance(v, Integer)
    try:
      return Integer(rarithmetic.ovfcheck(self.value - v.value))
    except OverflowError:
      overflow()
