import machine as m
import runtime as r

# We call frees on more than just expressions (e.g. expression lists)
# so I think we'll need to give CEKTop this method, too
class __extend__(m.CEKTop):
  def frees(self):
    return r.SymbolSet.EMPTY

class __extend__(m.cl_appinfo):
  def frees(self):
    return self.e0.frees().union(self.es1.frees())

class __extend__(m.cl_define):
  def __init__(self, var0, e1):
    self.var0 = var0
    self.e1 = e1.remake_with_recs([var0])

class __extend__(m.cl_e):
  def frees(self):
    frees = r.SymbolSet.EMPTY
    for subterm in self.subterms():
      frees = frees.union(subterm.frees())
    return frees
  # Given a list of variables, returns an object like self but the
  # free variables of self/expressions inside don't contain recs
  def remake_with_recs(self, recs):
    return self

class __extend__(m.cl_el):
  def frees(self):
    return self.e0.frees().union(self.es1.frees())

class __extend__(m.cl_letrecvalues):
  def __init__(self, valuesbinds0, e1, es2):
    recnames = valuesbinds0.boundnames()
    self.valuesbinds0 = valuesbinds0.remake_with_recs(recnames.keys())
    self.e1 = e1
    self.es2 = es2

class __extend__(m.cl_valuesbinds):
  # Returns a set of variables that this form binds
  def boundnames(self):
    return r.SymbolSet.EMPTY
  def remake_with_recs(self, recs):
    return self
class __extend__(m.cl_vbl):
  def boundnames(self):
    return self.valuesbind0.boundnames().union(self.valuesbinds1.boundnames())
  def remake_with_recs(self, recs):
    return m.cl_vbl(self.valuesbind0.remake_with_recs(recs),
                    self.valuesbinds1.remake_with_recs(recs))
class __extend__(m.cl_vb):
  def boundnames(self):
    names = r.SymbolSet.EMPTY
    for x in r.varl_to_xs(self.vars0):
      names = names.union(r.SymbolSet.singleton(x))
    return names
  def remake_with_recs(self, recs):
    return m.cl_vb(self.vars0, self.e1.remake_with_recs(recs))



class ClosureStrategy(object):
  pass
class DefaultClosureStrategy(ClosureStrategy):
  def get(self, lam, e):
    return m.cl_clo(lam, e)

class FixedClosureStrategy(ClosureStrategy):
  _immutable_fields_ = ['clo']
  def __init__(self, lam, e):
    self.clo = r.PromotableClosure(lam, e)

  def get(self, lam, e):
    return self.clo

# Because of unioning I apparently have to put _attrs_ in m.cl_l
# instead of inside the loop below. And that means _immutable_fields_
# has to be here, too.
class __extend__(m.cl_l):
  _attrs_ = m.cl_l._attrs_ + ['_frees', 'closure_strategy']
  _immutable_fields_ = m.cl_l._immutable_fields_ + ['_frees', 'closure_strategy']
  closure_strategy = None

for c in [m.cl_lam, m.cl_lamrest]:
  class __extend__(c):
    def __init__(self, vars0, e1, es2, frees=None):
      self.vars0 = vars0
      self.e1 = e1
      self.es2 = es2
      if not frees:
        frees = e1.frees()
        frees = frees.union(es2.frees())
        frees = frees.without_many(r.varl_to_xs(vars0))
      self._frees = frees

    def interpret(self, e, k):
      # We only query r.toplevel_vars when we've started to interpret
      # lambdas; otherwise, vars might not yet be initialized
      # initialized. Having to initialize this at interpret-time meant
      # that it was a little easier to make a strategy that decides
      # fixed-ness, rather than encoding that w/variables like Pycket
      if not self.closure_strategy:
        if r.toplevel_vars.contains_all(self._frees):
          self.closure_strategy = FixedClosureStrategy(self, e)
        else:
          self.closure_strategy = DefaultClosureStrategy()

      result = self.closure_strategy.get(self, e)
      return m.val_ignore_sing, e, m.cl_ret(result, k)
    def frees(self):
      return self._frees

# For some reason I couldn't define these methods inside the loop
# above... I didn't track down the root cause, but it looked like
# other code I generate couldn't correctly distinguish between cl_lam
# and cl_lamrest
class __extend__(m.cl_lam):
  def remake_with_recs(self, recs):
    return m.cl_lam(self.vars0, self.e1, self.es2, self._frees.without_many(recs))
class __extend__(m.cl_lamrest):
  def remake_with_recs(self, recs):
    return m.cl_lamrest(self.vars0, self.e1, self.es2, self._frees.without_many(recs))
