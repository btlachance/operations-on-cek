import machine as m

class __extend__(m.CEKTop):
  def set_surrounding_lambda(self, lam=None):
    self.surrounding_lambda = lam
    for subterm in self.subterms():
      subterm.set_surrounding_lambda(lam)

class __extend__(m.cl_lam, m.cl_lamrest):
  def set_surrounding_lambda(self, lam=None):
    for subterm in self.subterms():
      subterm.set_surrounding_lambda(self)
