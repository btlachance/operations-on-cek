def lamvar(v):
  varkey = [k for k in v.__dict__.keys() if 'var' in k][0]
  return v.__dict__[varkey]
def lambody(v):
  bodykey = [k for k in v.__dict__.keys() if 'e' in k][0]
  return v.__dict__[bodykey]

def cl_x():
  return mkvariable("x")
def cl_y():
  return mkvariable("y")
def cl_z():
  return mkvariable("z")

def tests():
  test1()
  test2()
  test3()

def test1():
  # lam x. lam y. x
  k = cl_lam(cl_x(), cl_lam(cl_y(), cl_x()))
  idz = cl_lam(cl_z(), cl_z())
  idx = cl_lam(cl_x(), cl_x())
  # k idz idx
  p = cl_app(cl_app(k, idz), idx)
  result = run(p)

  if not isinstance(result, cl_lam):
    print "error! result was not a lambda"
  if not (lamvar(result).literal == "z" and
          lambody(result).literal == "z"):
    print "error! result should be lam z. z"

def test2():
  # lam x. lam y. y x
  revapp = cl_lam(cl_x(), cl_lam(cl_y(), cl_app(cl_y(), cl_x())))
  idx = cl_lam(cl_x(), cl_x())
  idz = cl_lam(cl_z(), cl_z())
  # revapp idx idz
  p = cl_app(cl_app(revapp, idx), idz)
  result = run(p)

  if not isinstance(result, cl_lam):
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
  k = cl_lam(cl_x(), cl_lam(cl_z(), cl_x()))
  # lam y. lam x. y x
  app = cl_lam(cl_y(), cl_lam(cl_x(), cl_app(cl_y(), cl_x())))
  idy = cl_lam(cl_y(), cl_y())
  idz = cl_lam(cl_z(), cl_z())

  # app (k idy) idz
  p = cl_app(cl_app(app, cl_app(k, idy)), idz)
  result = run(p)

  if not isinstance(result, cl_lam):
    print "error! result was not a lambda"
  if not (lamvar(result).literal == "y" and
          lambody(result).literal == "y"):
    print ("error! result should be lam y. y "
           "got lam {}. {}".format(lamvar(result).literal,
                                   lambody(result).literal))
