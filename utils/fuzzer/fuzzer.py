import random
from random import randint

"""
A simple python script for generating lots of Swift code. This code should compile but not necessarily run successfully.
"""

inLoop = False
NUMFUNC = 30

def getptvar(): return random.choice(["pt2", "pt1"])
def maybe(): return random.random() > 0.5

def getexpr():
   global inLoop

   if (maybe() and maybe()): return random.choice([getifexpr, getifexpr, getifexpr, getrangeexpr])()
   if (maybe()): return getvarvalue(True) + " += " + getvarvalue(False)
   if (maybe()): return getvarvalue(True) + " -= " + getvarvalue(False)
   if (maybe()): return getvarvalue(True) + " *= " + getvarvalue(False)
   if (maybe()): return "assert(" + getCond() + ")"

   if (maybe() and inLoop): return "break"
   if (maybe() and inLoop): return "continue"
   if (maybe()): return "swap(&" + getvarvalue(True)  +",&" + getvarvalue(True) + ")"

   if (maybe()):
     ptObj = getptvar()
     funcNum = str(randint(0, NUMFUNC - 1))
     randDir = random.choice(["d", "Direction.Up"])
     barcode = random.choice(["b", "Barcode.Num(" + getvarvalue() + ")", "Barcode.Str(\"bar\")"])
     closure = random.choice(["cb", "{ return " + getvarvalue()  + "}", "{1}"])
     return "foo" + funcNum + "(" + getvarvalue() + "," + getvarvalue() + "," + randDir + "," + barcode + ",&" + ptObj + "," + closure + ")"

   if (maybe()): return "print(\"\(" + getvarvalue(True) + ")\")"
   if (maybe()): return "k += \"3\""
   if (maybe()): return "return ;"
   return "assert(true)"

def getCond():
   if (maybe()): return getvarvalue(False) + " > " + getvarvalue(False)
   if (maybe()): return getvarvalue(False) + " < " + getvarvalue(False)
   if (maybe()): return getvarvalue(False) + " != "+ getvarvalue(False)
   if (maybe()): return "d == Direction." + random.choice(["Up","Down"])
   if (maybe()): return getCond() + " && " + getCond()
   if (maybe()): return getCond() + " || " + getCond()
   return "true"

def getifexpr():
  return "if (" + getCond() + ") {\n" + getexpr() + "\n} else {\n" + getexpr()  + "\n}"

def getvarvalue(needToBeWritable = False):
  values = ["glob", "x2", "y2", "arr[" + str(randint(0,10)) + "]", "pt2.X", "pt2.Y", "pt1.X", "pt1.Y"]
  if not needToBeWritable: values += ["x!", "y", "cb()", str(randint(0, 100))]
  return random.choice(values)

def getrangeexpr():
  global inLoop
  if (maybe()): return "switch " + getvarvalue(True) + " {\n case 0: " + getexpr() + "\n\n case 1: \n" + getexpr() + "\n\n default: \n" + getexpr() + "\n\n}\n"
  if (maybe()): return "switch d {\n case .Up: \n" + getexpr() + "\n\n case .Down: \n" + getexpr() + "\n\n}\n"
  if (maybe()): return "switch b {\n case let .Str(ss): \n" + " k += ss " + "\n\n case let .Num(ii): \n" + " y2 = ii\n" + "\n case .None:\n" + getexpr() + "\n}\n"

  nextOperator = ".." if maybe() else "..."
  oldInLoop = inLoop
  inLoop = True
  loopStr = "for i in 0" + nextOperator + getvarvalue(True) + " {\n" + getexpr() + "\n}\n"
  inLoop = oldInLoop
  return loopStr

def gen_func(name):
  sb = """
func """ + name + """(x : Int?, y: Int, d : Direction, b : Barcode, inout pt2 : Point, cb : () -> Int) {
var x2 : Int = 0
var y2 : Int = 0
var arr = [1,2,3,4,5,6,7,8,9,10]
var pt1 = pt2
var k : String = "hi"
"""
  for i in range(randint(0,90)):
    sb += getexpr() + "\n"
  return sb + "}\n"


decl = random.choice(["struct ","@final class ", "class "])
print decl + """ Point { init(_ x : Int, _ y : Int) {\nX=x\nY=y\n}\nvar X : Int\nvar Y: Int}
enum Barcode {\ncase Str(String)\ncase Num(Int)\ncase None}
enum Direction {\ncase Up\ncase Down\n}
var glob : Int = 9
"""

for i in xrange(NUMFUNC):
  print (gen_func("foo" + str(i)))

print("var tmp = Point(0,0)\nfoo0(1,2, Direction.Down, Barcode.None, &tmp, {return 0})")

