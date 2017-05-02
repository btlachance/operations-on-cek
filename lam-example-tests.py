def tests():
  test1()
  test2()

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
  if not (result.var4424.literal == "z" and
          result.e4425.literal == "z"):
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
  if not (result.var4424.literal == "x" and
          result.e4425.literal == "x"):
    print "error! results should be lam x. x"
