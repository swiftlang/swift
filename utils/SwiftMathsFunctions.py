from SwiftFloatingPointTypes import all_floating_point_types

class SwiftMathsFunction(object):
  def __init__(self, name, cName=None, intrinsic=False, args="x"):
    self.name = name
    self.cName = cName if cName is not None else name
    self.intrinsic = intrinsic
    self.args = args

  def params(self, prefix="", suffix=""):
    return ", ".join(
      map(lambda a: prefix + a + suffix, self.args)
    )
  
  def decl(self, type):
    return self.name + "(" + self.params("_ ", ": " + type) + ") -> " + type
  
  def free_decl(self, protocol="Mathsable"):
    return self.name + "<T>(" + self.params("_ ", ": T") + ") -> T where T: " + protocol
  
  def impl(self, type):
    if self.intrinsic:
      builtin = "Builtin.int_" + self.name + "_FPIEEE" + str(type.bits)
      return type.stdlib_name + "(" + builtin + "(" + self.params("","._value") + "))"
    return self.cName + type.cFuncSuffix + "(" + self.params() + ")"

MathFunctions = [
  SwiftMathsFunction(name="cos", intrinsic=True),
  SwiftMathsFunction(name="sin", intrinsic=True),
  SwiftMathsFunction(name="exp", intrinsic=True),
  SwiftMathsFunction(name="exp2", intrinsic=True),
  SwiftMathsFunction(name="log", intrinsic=True),
  SwiftMathsFunction(name="log10", intrinsic=True),
  SwiftMathsFunction(name="log2", intrinsic=True),
]


