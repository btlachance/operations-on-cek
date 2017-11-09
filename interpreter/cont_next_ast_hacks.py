import machine as m

class __extend__(m.cl_k, m.cl_extensionk):
  def get_next_executed_ast(self):
    return None

def next_executed_in(es, k):
  if isinstance(es, m.cl_el):
    return es.e0
  return k.get_next_executed_ast()

class __extend__(m.cl_fn):
  def get_next_executed_ast(self):
    return next_executed_in(self.es1, self.k5)

class __extend__(m.cl_expsk):
  def get_next_executed_ast(self):
    return next_executed_in(self.es1, self.k2)

class __extend__(m.cl_ret):
  def get_next_executed_ast(self):
    return self.k1.get_next_executed_ast()

class __extend__(m.cl_sel):
  def get_next_executed_ast(self):
    # The callgraph code assumes that there's only one "next" AST.
    # But, the if continuation that I have has two potential next
    # ASTs, and we don't know which one is next until we've evaluated
    # the test condition. Pycket doesn't have to deal with this
    # because they don't have an if continuation; in ANF, they just
    # evaluate an if expression by seeing whether the test condition
    # was true or not. So, I'm going to naively assume here that both
    # should eventually be marked, and that the else branch should be
    # makred first.
    if not self.e1.can_enter():
      return self.e1
    return self.e0

def next_executed_in_binding(valuesbinds, bodies, k):
  if isinstance(valuesbinds, m.cl_vbl):
    bindingpair = valuesbinds.valuesbind0
    assert isinstance(bindingpair, m.cl_vb)
    return bindingpair.e1
  return next_executed_in(bodies, k)

class __extend__(m.cl_bindvaluesk):
  def get_next_executed_ast(self):
    return next_executed_in_binding(self.valuesbinds2, self.es4, self.k5)
class __extend__(m.cl_evalrec):
  def get_next_executed_ast(self):
    return next_executed_in_binding(self.valuesbinds0, self.es1, self.k2)

# When the bindrec and bindvarscells code were both written in a
# single __extend__, the inferred type for self was expk... which
# doesn't have a k1 attribute. Apparently the single __extend__ wasn't
# checked as being applied to separate classes
class __extend__(m.cl_bindrec):
  def get_next_executed_ast(self):
    return next_executed_in_binding(self.valuesbinds0, m.val_esnil_sing, self.k1)
class __extend__(m.cl_bindvarscells):
  def get_next_executed_ast(self):
    return self.k1.get_next_executed_ast()
