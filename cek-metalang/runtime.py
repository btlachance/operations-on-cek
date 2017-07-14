from rpython.rlib import jit
class CEKError(Exception):
  def __init__(self, message):
    self.message = message
  def __str__(self):
    return self.message
class CEKMatchFailure(CEKError):
  pass
class CEKUnlessFailure(CEKError):
  def __init__(self):
    pass
class CEKDone(Exception):
  def __init__(self, result):
    self.result = result

def mkvariable(name):
  return PrimVariable(name)
class PrimVariable(cl_variable):
  def __init__(self, name):
    self.literal = name
  def pprint(self, indent):
    return self.literal

def mkint(n):
  return Integer(n)
class Integer(cl_integer):
  def __init__(self, n):
    self.value = n
  def eq(self, other):
    return isinstance(other, Integer) and self.value == other.value
  def ne(self, other):
    return not self.eq(other)
  def pprint(self, indent):
    return ' ' * indent + '%s' % self.value

class UnaryPrim(cl_e):
  def __init__(self, arg, opname, op):
    self.arg = arg
    self.opname = opname
    self.op = op
  def interpret(self, env, k):
    v = lookup(env, self.arg)
    return mkexp(self.op(v)), env, k
  def pprint(self, indent):
    return ' ' * indent + '(%s %s)' % (self.opname, self.arg.pprint(0))
def zeropimpl(n):
  return UnaryPrim(n, 'zerop', lambda n: cl_true() if n.value == 0 else cl_false())
def succimpl(n):
  return UnaryPrim(n, 'succ', lambda n: Integer(n.value + 1))
def predimpl(n):
  return UnaryPrim(n, 'pred', lambda n: Integer(n.value - 1))

class BinaryPrim(cl_e):
  def __init__(self, arg1, arg2, opname, op):
    self.arg1 = arg1
    self.arg2 = arg2
    self.opname = opname
    self.op = op
  def interpret(self, env, k):
    v1 = lookup(env, self.arg1)
    v2 = lookup(env, self.arg2)
    return mkexp(self.op(v1, v2)), env, k
  def pprint(self, indent):
    return ' '* indent + '(%s %s %s)' % (self.opname, self.arg1.pprint(0), self.arg2.pprint(0))
def addimpl(n1, n2):
  return BinaryPrim(n1, n2, '+', lambda n1, n2: Integer(n1.value + n2.value))
def subimpl(n1, n2):
  return BinaryPrim(n1, n2, '-', lambda n1, n2: Integer(n1.value - n2.value))
def multimpl(n1, n2):
  return BinaryPrim(n1, n2, '*', lambda n1, n2: Integer(n1.value * n2.value))

class Env(cl_env):
  def __init__(self):
    pass
  def lookup(self, x):
    raise Exception("subclass responsibility")
class EmptyEnv(Env):
  def __init__(self):
    pass
  def lookup(self, y):
    raise CEKError("Variable %s not found" % y)
class ExtendedEnv(Env):
  def __init__(self, x, v, e):
    self.x = x
    self.v = v
    self.e = e
  def lookup(self, y):
    if self.x.literal == y.literal:
      return self.v
    else:
      return self.e.lookup(y)

def emptyenv():
  return EmptyEnv()
def lookup(e, x):
  return e.lookup(x)
def extend(e, x, v):
  return ExtendedEnv(x, v, e)

# Any cl_v will do here; it's just a placeholder
_sing_exp = cl_exp(None)
def mkexp(v):
  _sing_exp.v0 = v
  return _sing_exp

def pprint(v):
  if isinstance(v, cl_clo):
    print v.l0.pprint(0)
  else:
    print v.pprint(0)
  return v

def ret(v):
  raise CEKDone(v)

driver = jit.JitDriver(reds = ['e', 'k'],
                       greens = ['c'],
                       get_printable_location=lambda c: c.pprint(0))

def run(p):
  c, e, k = init(p)
  while True:
    driver.jit_merge_point(c = c, e = e, k = k)
    # print "c: %s, e: %s, k: %s" % (c.pprint(0), e.pprint(0), k.pprint(0))
    try:
      c, e, k = c.interpret(e, k)
      if isinstance(c, cl_app):
        driver.can_enter_jit(c = c, e = e, k = k)
    except CEKDone as d:
      return d.result
    except CEKError as err:
      print err.__str__()
      print c.pprint(0)
      return None
