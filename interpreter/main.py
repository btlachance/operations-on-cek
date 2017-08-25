import parser
from runtime import run

def main(argv):
  from rpython.rlib import streamio as sio
  from pycket import pycket_json

  stdin = sio.fdopen_as_stream(0, "r")
  json = pycket_json.loads(stdin.readall())
  ast = parser.parse(json)
  return run(ast)

def main_untranslated(argv):
  import json
  import pycket_json_adapter as pja

  adapted = pja.adapt(json.load(sys.stdin))
  ast = parser.parse(adapted)
  return run(ast)

if __name__ == "__main__":
  import sys
  main_untranslated(sys.argv)
