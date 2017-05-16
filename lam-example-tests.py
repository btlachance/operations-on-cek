def lamvar(v):
  varkey = [k for k in v.__dict__.keys() if 'var' in k][0]
  return v.__dict__[varkey]
def lambody(v):
  bodykey = [k for k in v.__dict__.keys() if 'e' in k][0]
  return v.__dict__[bodykey]

def tests():
  test1()
  test2()
  test3()

def test1():
  # lam x. lam y. x
  k = cl_v_comb0(cl_x(), cl_v_comb0(cl_y(), cl_x()))
  idz = cl_v_comb0(cl_z(), cl_z())
  idx = cl_v_comb0(cl_x(), cl_x())
  # k idz idx
  p = cl_e_comb2(cl_e_comb2(k, idz), idx)
  result = run(p)

  if not isinstance(result, cl_v_comb0):
    print "error! result was not a lambda"
  if not (lamvar(result).literal == "z" and
          lambody(result).literal == "z"):
    print "error! result should be lam z. z"

def test2():
  # lam x. lam y. y x
  revapp = cl_v_comb0(cl_x(), cl_v_comb0(cl_y(), cl_e_comb2(cl_y(), cl_x())))
  idx = cl_v_comb0(cl_x(), cl_x())
  idz = cl_v_comb0(cl_z(), cl_z())
  # revapp idx idz
  p = cl_e_comb2(cl_e_comb2(revapp, idx), idz)
  result = run(p)

  if not isinstance(result, cl_v_comb0):
    print "error! result was not a lambda"
  if not (lamvar(result).literal == "x" and
          lambody(result).literal == "x"):
    print "error! results should be lam x. x"

def test3():
  # This is a minimal test for lexical scope. The term in k takes an
  # argument, x, and produces a constant function that closes over x.
  # The term in app applies its first argument, y, to its second
  # argument, x. If the language has lexical scope, then applying the
  # constant function produced by k will produce the x it closed over;
  # otherwise, it produces app's x.

  # lam x. lam z. x
  k = cl_v_comb0(cl_x(), cl_v_comb0(cl_z(), cl_x()))
  # lam y. lam x. y x
  app = cl_v_comb0(cl_y(), cl_v_comb0(cl_x(), cl_e_comb2(cl_y(), cl_x())))
  idy = cl_v_comb0(cl_y(), cl_y())
  idz = cl_v_comb0(cl_z(), cl_z())

  # app (k idy) idz
  p = cl_e_comb2(cl_e_comb2(app, cl_e_comb2(k, idy)), idz)
  result = run(p)

  if not isinstance(result, cl_v_comb0):
    print "error! result was not a lambda"
  if not (lamvar(result).literal == "y" and
          lambody(result).literal == "y"):
    print ("error! result should be lam y. y "
           "got lam {}. {}".format(lamvar(result).literal,
                                   lambody(result).literal))