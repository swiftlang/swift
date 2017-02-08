// RUN: %target-typecheck-verify-swift

func f0(_ x: Int) -> Int { return 0 }
func f0(_ x: Float) -> Float { return 0.0 }
func f1() -> Int { return 0 }
func f1() -> Float { return 0.0 }

struct Y {
  func f0(_ x: Int) -> Int { return 0 }
  func f0(_ x: Float) -> Float { return 0.0 }
  func f1() -> Int { return 0 }
  func f1() -> Float { return 0.0 }
}

func testParenOverloads(_ x: inout Int, y: Y) {
  x = f0(x)
  x = (f0)(x)
  x = ((f0))(x)
  x = f1()
  x = (f1)()
  x = ((f1))()

  x = y.f0(x)
  x = (y.f0)(x)
  x = ((y.f0))(x)
  x = y.f1()
  x = (y.f1)()
  x = ((y.f1))()
}
