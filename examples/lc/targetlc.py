from lc import main

def entry_point(argv):
  main()
  return 0

def target(*args):
  return entry_point, None
