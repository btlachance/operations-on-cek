import machine as m

class __extend__(m.CEKTop):
  def set_surrounding_lambda(self, lam=None):
    self.surrounding_lambda = lam
    for subterm in self.subterms():
      subterm.set_surrounding_lambda(lam)

class __extend__(m.cl_lam):
  def __init__(self, vars0, e1):
    self.vars0 = vars0
    self.e1 = e1

    self.e1.set_surrounding_lambda(self)

  def set_surrounding_lambda(self, lam=None):
    for subterm in self.subterms():
      subterm.set_surrounding_lambda(self)
