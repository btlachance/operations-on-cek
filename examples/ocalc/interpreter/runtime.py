from rpython.rlib import jit
from rpython.rlib import streamio as sio
import machine as m
import numbers as nums
import envs
import can_enter_hacks
import pprint_hacks

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

def get_printable_location(c, prev_c):
  if c.can_enter:
    return '%s from %s' % (c.pprint(0), prev_c.pprint(0))
  return c.pprint(0)
driver = jit.JitDriver(reds = ['e', 'k'], greens = ['c', 'prev_c'],
                       get_printable_location = get_printable_location)
def run(p):
  c, e, k = m.init(p)
  prev_c = c
  try:
    while True:
      driver.jit_merge_point(c = c, prev_c = prev_c, e = e, k = k)

      prev_c = c if isinstance(c, m.cl_app) else prev_c
      c, e, k = c.interpret(e, k)

      if c.can_enter:
        driver.can_enter_jit(c = c, prev_c = prev_c, e = e, k = k)
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
def extend(e, x, v):
  return envs.Env1(x, v, e)
def lookup(e, x):
  assert isinstance(x, envs.PrimVariable)
  x = jit.promote(x)
  return e.lookup(x)

def mkint(n):
  return nums.Integer(n)

def primadd(v1, v2):
  return nums.guardint(v1).add(nums.guardint(v2))

stdout = sio.fdopen_as_stream(1, "w", buffering = 1)
def primprint(v):
  s = None
  if isinstance(v, m.cl_clo):
    s = "<procedure>"
  elif isinstance(v, m.cl_obj):
    s = "<object>"
  else:
    s = v.pprint(0)
  stdout.write(s)
  stdout.write("\n")

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
