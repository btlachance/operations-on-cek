from main import main

def entry_point(argv):
  main(argv)
  return 0

def target(driver, args):
  driver.exe_name = 'cek-%(backend)s'
  return entry_point, None
