import parser
from runtime import run
from rpython.rlib.objectmodel import we_are_translated

def main(argv):
  if we_are_translated():
    return main_translated(argv)
  else:
    import sys
    sys.exit(main_untranslated(argv))

def main_translated(argv):
  from rpython.rlib import streamio as sio
  from pycket import pycket_json

  stdin = sio.fdopen_as_stream(0, "r")
  json = pycket_json.loads(stdin.readall())
  ast = parser.parse(json)
  return run(ast)

def main_untranslated(argv):
  import sys
  import json
  import pycket_json_adapter as pja

  adapted = pja.adapt(json.load(sys.stdin))
  ast = parser.parse(adapted)
  return run(ast)

if __name__ == "__main__":
  import sys
  main(sys.argv)
