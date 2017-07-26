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
def guardint(v):
  if not isinstance(v, Integer):
    raise CEKError("Expected an integer")
  return v
def zeropimpl(n):
  return UnaryPrim(n, 'zerop', lambda n: cl_true() if guardint(n).value == 0 else cl_false())
def succimpl(n):
  return UnaryPrim(n, 'succ', lambda n: Integer(guardint(n).value + 1))
def predimpl(n):
  return UnaryPrim(n, 'pred', lambda n: Integer(guardint(n).value - 1))
def addimpl(n1, n2):
  return BinaryPrim(n1, n2, '+', lambda n1, n2: Integer(guardint(n1).value + guardint(n2).value))
def subimpl(n1, n2):
  return BinaryPrim(n1, n2, '-', lambda n1, n2: Integer(guardint(n1).value - guardint(n2).value))
def multimpl(n1, n2):
  return BinaryPrim(n1, n2, '*', lambda n1, n2: Integer(guardint(n1).value * guardint(n2).value))

def mkbox(v):
  return Box(v)
class Box(cl_v):
  def __init__(self, v):
    self.v = v
  def set(self, new_v):
    self.v = new_v
    return self.v
  def get(self):
    return self.v
  def pprint(self, indent):
    return ' ' * indent + '<box %s>' % self.v.pprint(0)
def boximpl(v):
  return UnaryPrim(v, 'box', lambda v: mkbox(v))
def unboximpl(b):
  return UnaryPrim(b, 'unbox', lambda b: b.get())
def setboximpl(b, v):
  return BinaryPrim(b, v, 'box-set!', lambda b, v: b.set(v))

class UnaryPrim(cl_e):
  def __init__(self, arg, opname, op):
    self.arg = arg
    self.opname = opname
    self.op = op
  def interpret(self, env, k):
    v = lookup(env, self.arg)
    return _ignore_sing, env, cl_ret(self.op(v), k)
  def pprint(self, indent):
    return ' ' * indent + '(p#%s %s)' % (self.opname, self.arg.pprint(0))

class BinaryPrim(cl_e):
  def __init__(self, arg1, arg2, opname, op):
    self.arg1 = arg1
    self.arg2 = arg2
    self.opname = opname
    self.op = op
  def interpret(self, env, k):
    v1 = lookup(env, self.arg1)
    v2 = lookup(env, self.arg2)
    return _ignore_sing, env, cl_ret(self.op(v1, v2), k)
  def pprint(self, indent):
    return ' ' * indent + '(p#%s %s %s)' % (self.opname, self.arg1.pprint(0), self.arg2.pprint(0))

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
  def pprint(self, indent):
    return ' ' * indent + 'emptyenv'
class ExtendedEnv(Env):
  def __init__(self, x, v, e):
    self.x = x
    self.v = v
    self.e = e
  def lookup(self, y):
    if (isinstance(self.x, PrimVariable) and
        isinstance(y, PrimVariable) and
        self.x.literal == y.literal):
      return self.v
    else:
      return self.e.lookup(y)
  def pprint(self, indent):
    return ' ' * indent + '([%s -> %s], %s)' % (self.x.pprint(0), self.v.pprint(0), self.e.pprint(0))

def emptyenv():
  return EmptyEnv()
def lookup(e, x):
  result = e.lookup(x)
  if isinstance(result, Cell):
    result = result.get()
  return result
def extend(e, xs, vs):
  result = e
  while isinstance(xs, cl_varl) and isinstance(vs, cl_vl):
    x, xs = xs.var0, xs.vars1
    v, vs = vs.v0, vs.vs1
    result = ExtendedEnv(x, v, result)
  if isinstance(xs, cl_varl) or isinstance(vs, cl_vl):
    raise CEKError("Function called with the wrong number of arguments")
  return result
def extend1(e, x, v):
  return ExtendedEnv(x, v, e)
def extendrest(e, xs, x_rest, vs):
  result = e
  while isinstance(xs, cl_varl) and isinstance(vs, cl_vl):
    x, xs = xs.var0, xs.vars1
    v, vs = vs.v0, vs.vs1
    result = ExtendedEnv(x, v, result)
  if isinstance(xs, cl_varl):
    raise CEKError("Function called with too few arguments")

  restvs_reversed = vsreverse(vs)
  rest_list = _nil_sing
  while isinstance(restvs_reversed, cl_vl):
    rest_list = cl_cons(restvs_reversed.v0, rest_list)
    restvs_reversed = restvs_reversed.vs1
  return ExtendedEnv(x_rest, rest_list, result)

def pprint(v):
  if isinstance(v, cl_clo):
    print v.l0.pprint(0)
  else:
    print v.pprint(0)
  return v

def ret(v):
  raise CEKDone(v)

def modformsreverse(mfs):
  result = _mfnil_sing

  while isinstance(mfs, cl_mf):
    result = cl_mf(mfs.modform0, result)
    mfs = mfs.modforms1
  return result

def varsreverse(vars):
  result = _varsnil_sing

  while isinstance(vars, cl_varl):
    result = cl_varl(vars.var0, result)
    vars = vars.vars1
  return result

def vsreverse(vs):
  result = _vsnil_sing
  while isinstance(vs, cl_vl):
    result = cl_vl(vs.v0, result)
    vs = vs.vs1
  return result

class Cell(cl_v):
  def __init__(self, init):
    self.val = init
  def set(self, v):
    self.val = v
    return v
  def get(self):
    return self.val
def mkcell(v):
  return Cell(v)
def setcell(var, env, v):
  cell = env.lookup(var)
  return cell.set(v)

from rpython.rlib import jit
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
      print "c: %s, e: %s, k: %s" % (c.pprint(0), e.pprint(0), k.pprint(0))
      print err.__str__()
      print c.pprint(0)
      return None
