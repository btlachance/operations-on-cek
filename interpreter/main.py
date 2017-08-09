import parser
from runtime import run
from pycket import pycket_json
from rpython.rlib import streamio as sio

def main(argv):
  stdin = sio.fdopen_as_stream(0, "r")
  ast = parser.parse(get_json_string(stdin.readall()))
  return run(ast)

def get_json_string(s):
  return pycket_json.loads(s)
