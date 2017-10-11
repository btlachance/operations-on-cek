import env, values, prims

# Idea: use pairtypes to implement arbitrary tuples and dispatching on
# them; who knows, maybe it won't be horribly inefficient!
#   from rpython.tool.pairtype import pairtype, pair
#   class __extend__(pairtype(int, pairtype(int, int))):
#     def add((x, (y, z))):
#       return 'triple: %s' % (x + y + z)
#   assert pair(3, pair(4, 5)).add() == 'triple: 12'


class Exp(object):
  pass
class Var(Exp):
  pass
class Lam(Exp):
  pass
class App(Exp):
  pass
class Literal(Exp):
  pass

class Cont(object):
  pass
class Final(Cont):
  pass
class EvalArgs(Cont):
  pass
class CallFn(Cont):
  pass
