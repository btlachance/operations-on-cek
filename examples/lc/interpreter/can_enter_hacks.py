import machine as m

# Since we have more than just expressions in the control string
# (e.g. modbegin) things other than m.cl_e need can_enter
class __extend__(m.cl_term):
  def set_should_enter(self):
    if not self.should_enter:
      self.should_enter = True
  def can_enter(self):
    return self.should_enter

class __extend__(m.cl_l):
  def enable_jitting(self):
    self.body0().set_should_enter()
class __extend__(m.cl_lamrest):
  def body0(self):
    return self.e1
class __extend__(m.cl_lam):
  def body0(self):
    return self.e1
