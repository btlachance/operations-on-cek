from rpython.rlib import jit
from rpython.rlib import streamio as sio
import machine as m
import numbers as nums
import envs
import prims as p
import util as u
import can_enter_hacks

class CEKError(Exception):
  def __init__(self, message):
    self.message = message
  def __str__(self):
    return self.message
class CEKMatchFailure(CEKError):
  def __init__(self, message):
    self.message = message
class CEKUnlessFailure(CEKError):
  def __init__(self):
    pass
class CEKDone(Exception):
  _attrs_ = ['result']
  def __init__(self, result):
    self.result = result

driver = jit.JitDriver(reds = ['e', 'k'], greens = ['c'],
                       get_printable_location = lambda c: c.pprint(0))
def run(p):
  c, e, k = m.init(p)
  try:
    while True:
      driver.jit_merge_point(c = c, e = e, k = k)

      c, e, k = c.interpret(e, k)

      if c.can_enter:
        driver.can_enter_jit(c = c, e = e, k = k)
  except CEKDone as d:
    return 0
  except CEKError as err:
    print "k: %s" % k.pprint(0)
    print err.__str__()
    return 1
  finally:
    stdout.flush()

def ret(e):
  raise CEKDone(e)

def mkvariable(name):
  return envs.PrimVariable.make(name)
def emptyenv():
  return envs.EmptyEnv()
def lookup(e, x):
  assert isinstance(x, envs.PrimVariable)
  x = jit.promote(x)
  return e.lookup(x)
def lookup_backtrack(e, x):
  try:
    result = lookup(e, x)
  except envs.VariableNotFound:
    raise CEKMatchFailure("VariableNotFound")
  return result
def extend(e, xs, vs):
  values = u.vstolist(vs)
  return envs.Env.make(xs, values, e)
def mkint(n):
  return nums.Integer(n)
def numequalimpl(v1, v2):
  return p.BinaryPrim(v1, v2, '=', lambda v1, v2: u.true() if v1.eq(v2) else u.false())
def addimpl(n1, n2):
  return p.BinaryPrim(n1, n2, '+', lambda n1, n2: nums.guardint(n1).add(nums.guardint(n2)))
def subimpl(n1, n2):
  return p.BinaryPrim(n1, n2, '-', lambda n1, n2: nums.guardint(n1).sub(nums.guardint(n2)))
def multimpl(n1, n2):
  return p.BinaryPrim(n1, n2, '*', lambda n1, n2: nums.guardint(n1).mult(nums.guardint(n2)))
def divimpl(n1, n2):
  return p.BinaryPrim(n1, n2, '/', lambda n1, n2: nums.guardint(n1).div(nums.guardint(n2)))
def ltimpl(n1, n2):
  return p.BinaryPrim(n1, n2, '<', lambda n1, n2: nums.guardint(n1).lt(nums.guardint(n2)))
def gtimpl(n1, n2):
  return p.BinaryPrim(n1, n2, '>', lambda n1, n2: nums.guardint(n1).gt(nums.guardint(n2)))

@jit.unroll_safe
def vsreverse(vs):
  result = m.val_vsnil_sing
  while isinstance(vs, m.cl_vl):
    result = m.cl_vl(vs.v0, result)
    vs = vs.vs1
  return result

stdout = sio.fdopen_as_stream(1, "w", buffering = 1)
def printimpl(n):
  def _print(n): # since I can't put statements in a lambda...
    import sys
    stdout.write(n.pprint(0))
    return n
  return p.UnaryPrim(n, 'print', _print)
def printlnimpl(n):
  def _println(n):
    stdout.write(n.pprint(0))
    stdout.write("\n")
    return n
  return p.UnaryPrim(n, 'print', _println)

# XXX These have to be in here because I have hacks in the LC language
# that rely on these functions being available at JSON->AST time
def boximpl(v):
  assert False
def unboximpl(b):
  assert False
def setboximpl(b, v):
  assert False
def mkstr(s):
  assert False
def mkfloat(f):
  assert False

