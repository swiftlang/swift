// RUN: %target-parse-verify-swift

func f0(x: Float) -> Float {}
func f1(x: Float) -> Float {}
func f2(@autoclosure x: () -> Float) {}

var f : Float

f0(f0(f))
f0(1)
f1(f1(f))
f2(f)
f2(1.0)

func call_lvalue(@autoclosure rhs: ()->Bool) -> Bool {
  return rhs()
}

// Function returns
func weirdCast<T, U>(x: T) -> U {}

func ff() -> (Int) -> (Float) { return weirdCast }

// Block <-> function conversions

var funct: String -> String = { $0 }
var block: @convention(block) String -> String = funct
funct = block
block = funct

// Application of implicitly unwrapped optional functions

var optFunc: (String -> String)! = { $0 }
var s: String = optFunc("hi")
