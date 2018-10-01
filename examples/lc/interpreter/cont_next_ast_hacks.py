import machine as m
import runtime as r

class __extend__(m.cl_k, m.cl_extensionk):
  def get_next_executed_ast(self):
    return None
  def set_should_enter_return(self):
    pass

class __extend__(m.cl_fn):
  def get_next_executed_ast(self):
    callingapp = self.callingapp4
    if isinstance(callingapp, m.cl_ca):
      return callingapp.a0
    return None
  def set_should_enter_return(self):
    if isinstance(self.es1, m.cl_el):
      return

    vs = r.vstolist(self.vs0)
    if len(vs) == 0:
      return
    vs.reverse()

    v = vs[0]
    if isinstance(v, m.cl_clo):
      lam = v.l0
      if isinstance(lam, m.cl_l):
        lam.enable_jitting()
