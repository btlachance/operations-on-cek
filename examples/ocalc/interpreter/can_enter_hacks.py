import machine as m

class __extend__(m.cl_e):
  _immutable_fields_ = m.cl_e._immutable_fields_ + ['can_enter']
  _attrs_ = m.cl_e._attrs_ + ['can_enter']
  can_enter = False
  def set_should_enter(self):
    self.can_enter = True
