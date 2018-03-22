from rpython.rlib import jit
import machine as m
import numbers as nums
import runtime as r

@jit.unroll_safe
def len_varl(xs):
  n = 0
  while isinstance(xs, m.cl_varl):
    xs = xs.vars1
    n += 1
  assert isinstance(xs, m.cl_varsnil)
  return n

@jit.unroll_safe
def vstolist(vs):
  values = []
  while isinstance(vs, m.cl_vl):
    v, vs = vs.v0, vs.vs1
    values.append(v)
  assert isinstance(vs, m.cl_vsnil)
  return values

def true():
  return r.mkint(1)
def false():
  return r.mkint(0)
