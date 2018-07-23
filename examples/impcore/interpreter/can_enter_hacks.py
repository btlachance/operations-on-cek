import machine as m
import prims as p

class __extend__(m.CEKTop):
  _immutable_fields_ = m.CEKTop._immutable_fields_ + ['can_enter']
  _attrs_ = m.CEKTop._attrs_ + ['can_enter']
  can_enter = False
  def set_should_enter(self):
    if not self.can_enter:
      self.can_enter = True

class __extend__(m.cl_while):
  def __init__(self, e0, e1):
    self.e0 = e0
    self.e1 = e1

    self.e1.set_should_enter()

class __extend__(p.UnaryPrim, p.BinaryPrim):
  def set_should_enter(self):
    self.can_enter = False

class __extend__(m.cl_lam):
  def enable_jitting(self):
    self.e1.set_should_enter()
