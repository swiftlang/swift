// RUN: %swift -parse -verify %s

func f0(x: Float) -> Float {}
func f1(x: Float) -> Float {}
func f2(x: @auto_closure () -> Float) {}

var f : Float

f0(f0(f))
f0(1)
f1(f1(f))
f2(f)
f2(1.0)

func call_lvalue(rhs: @auto_closure ()->Bool) -> Bool {
  return rhs()
}

// Function returns
func weirdCast<T, U>(x: T) -> U {}

func ff() -> (Int) -> (Float) { return weirdCast }

// Block <-> function conversions

var funct: String -> String = { $0 }
var block: @objc_block String -> String = funct
funct = block
block = funct

// Application of implicitly unwrapped optional functions

var optFunc: (String -> String)! = { $0 }
var s: String = optFunc("hi")
