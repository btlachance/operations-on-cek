from main import main
from rpython.rlib import jit
from runtime import driver

def entry_point(argv):
  # Mostly Pycket's numbers
  jit.set_param(None, 'trace_limit', 10000)
  jit.set_param(None, 'threshold', 131)
  jit.set_param(None, 'trace_eagerness', 50)
  jit.set_param(None, 'max_unroll_loops', 15)

  # Thanks, pyrolog
  for i in range(len(argv)):
    if argv[i] == '--jit':
      if len(argv) == i + 1:
        print 'missing argument after --jit'
        return 2
      jitarg = argv[i + 1]
      del argv[i:i+2]
      jit.set_user_param(driver, jitarg)
      break
  if len(argv) > 1:
    print 'too many arguments'
  return main(argv)

def target(driver, args):
  driver.exe_name = 'cek-%(backend)s'
  return entry_point, None
