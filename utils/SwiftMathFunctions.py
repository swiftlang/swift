class SwiftMathFunction(object):
    def __init__(self, name, kind=None, swiftName=None, args="x", comment=None,
                 platforms=None):
        self.name = name
        self.swiftName = swiftName if swiftName is not None else name
        self.kind = kind if kind is not None else "library"
        self.args = args
        if comment is not None:
            self.comment = comment
        else:
            self.comment = "/// The " + str(self.swiftName) + " function."
        self.platforms = platforms

    def params(self, prefix="", suffix=""):
        return ", ".join(map(lambda a: prefix + a + suffix, self.args))

    def decl(self, type):
        return self.swiftName + "(" + self.params("_ ", ": " + type) + \
            ") -> " + type

    def free_decl(self, constraint="T: ElementaryFunctions"):
        return self.swiftName + "<T>(" + self.params("_ ", ": T") + \
            ") -> T where " + constraint

    def impl(self, type):
        if self.kind == "intrinsic":
            builtin = "Builtin.int_" + self.name + "_FPIEEE" + str(type.bits)
            return type.stdlib_name + "(" + builtin + "(" + \
                self.params("", "._value") + "))"
        return "_swift_stdlib_" + self.name + type.cFuncSuffix + "(" + \
            self.params() + ")"


ElementaryFunctions = [
    SwiftMathFunction(name="sqrt", kind="intrinsic", comment="""
  /// The square root of `x`.
  ///
  /// For real types, if the argument is negative, either the result is NaN
  /// or a precondition failure occurs. For complex types, this function has
  /// a branch cut along the negative real axis.
"""),
    SwiftMathFunction(name="cos", kind="intrinsic", comment="""
  /// The cosine of `x`.
  ///
  /// For real types, `x` is interpreted as an angle measured in radians.
"""),
    SwiftMathFunction(name="sin", kind="intrinsic", comment="""
  /// The sine of `x`.
  ///
  /// For real types, `x` is interpreted as an angle measured in radians.
"""),
    SwiftMathFunction(name="tan",
                      comment="/// The tangent of `x`."),
    SwiftMathFunction(name="acos"),
    SwiftMathFunction(name="asin"),
    SwiftMathFunction(name="atan"),
    SwiftMathFunction(name="cosh"),
    SwiftMathFunction(name="sinh"),
    SwiftMathFunction(name="tanh"),
    SwiftMathFunction(name="acosh"),
    SwiftMathFunction(name="asinh"),
    SwiftMathFunction(name="atanh"),
    SwiftMathFunction(name="exp", kind="intrinsic"),
    SwiftMathFunction(name="exp2", kind="intrinsic"),
    SwiftMathFunction(name="exp10"),
    SwiftMathFunction(name="expm1"),
    SwiftMathFunction(name="log", kind="intrinsic"),
    SwiftMathFunction(name="log2", kind="intrinsic"),
    SwiftMathFunction(name="log10", kind="intrinsic"),
    SwiftMathFunction(name="log1p"),
    # SwiftMathFunction(name="pow", kind="intrinsic", args="xy"), Handled
    # separately for edge cases.
    # SwiftMathFunction(name="root", args="xn"), Handled separately for
    # implementation.
]

RealFunctions = [
    # SwiftMathFunction(name="atan2"), Handled separately for explicit
    # argument labels.
    SwiftMathFunction(name="erf"),
    SwiftMathFunction(name="erfc"),
    SwiftMathFunction(name="hypot", args="xy"),
    SwiftMathFunction(name="tgamma", swiftName="gamma"),
    # SwiftMathFunction(name="lgamma"), Handled separately for sign result.
]
