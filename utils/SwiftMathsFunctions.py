from SwiftFloatingPointTypes import all_floating_point_types

class SwiftMathsFunction(object):
  def __init__(self, name, kind=None, swiftName=None, args="x"):
    self.name = name
    self.swiftName = swiftName if swiftName is not None else name
    self.kind = kind if kind is not None else "library"
    self.args = args

  def params(self, prefix="", suffix=""):
    return ", ".join(
      map(lambda a: prefix + a + suffix, self.args)
    )
  
  def decl(self, type):
    return self.swiftName + "(" + self.params("_ ", ": " + type) + ") -> " + type
  
  def free_decl(self, protocol="Mathsable"):
    return self.swiftName + "<T>(" + self.params("_ ", ": T") + ") -> T where T: " + protocol
  
  def impl(self, type):
    if self.kind == "intrinsic":
      builtin = "Builtin.int_" + self.name + "_FPIEEE" + str(type.bits)
      return type.stdlib_name + "(" + builtin + "(" + self.params("","._value") + "))"
    if self.kind == "builtin":
      builtin = "Builtin." + self.name + "_FPIEEE" + str(type.bits)
      return type.stdlib_name + "(" + builtin + "(" + self.params("","._value") + "))"
    return "_swift_stdlib_" + self.name + type.cFuncSuffix + "(" + self.params() + ")"

MathsFunctions = [
  SwiftMathsFunction(name="cos", kind="intrinsic"),
  SwiftMathsFunction(name="sin", kind="intrinsic"),
  SwiftMathsFunction(name="tan"),
  SwiftMathsFunction(name="acos"),
  SwiftMathsFunction(name="asin"),
  SwiftMathsFunction(name="atan"),
  SwiftMathsFunction(name="atan2", args="yx"),
  SwiftMathsFunction(name="exp", kind="intrinsic"),
  SwiftMathsFunction(name="exp2", kind="intrinsic"),
  SwiftMathsFunction(name="log", kind="intrinsic"),
  SwiftMathsFunction(name="log10", kind="intrinsic"),
  SwiftMathsFunction(name="log2", kind="intrinsic"),
  SwiftMathsFunction(name="pow", kind="intrinsic", args="xy"),
]


