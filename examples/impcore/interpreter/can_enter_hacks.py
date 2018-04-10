import machine as m
import prims as p

class __extend__(m.cl_term):
  _immutable_fields_ = m.cl_term._immutable_fields_ + ['can_enter']
  _attrs_ = m.cl_term._attrs_ + ['can_enter']
  can_enter = False
  def set_should_enter(self):
    self.can_enter = True

class __extend__(m.cl_define):
  def __init__(self, var0, vars1, e2):
    assert isinstance(e2, m.cl_e)

    self.var0 = var0
    self.vars1 = vars1
    self.e2 = e2

    self.e2.set_should_enter()

class __extend__(m.cl_while):
  def __init__(self, e0, e1):
    self.e0 = e0
    self.e1 = e1

    self.e1.set_should_enter()

class __extend__(p.UnaryPrim, p.BinaryPrim):
  def set_should_enter(self):
    self.can_enter = False
