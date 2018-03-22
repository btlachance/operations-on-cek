import machine as m

class __extend__(m.cl_term):
  _immutable_fields_ = m.cl_term._immutable_fields_ + ['can_enter']
  _attrs_ = m.cl_term._attrs_ + ['can_enter']
  can_enter = False

class __extend__(m.cl_define):
  def __init__(self, var0, vars1, e2):
    assert isinstance(e2, m.cl_e)

    self.var0 = var0
    self.vars1 = vars1
    self.e2 = e2

    self.e2.can_enter = True

    
