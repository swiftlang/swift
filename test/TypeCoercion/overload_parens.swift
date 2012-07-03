// RUN: %swift %s -verify

func f0(x : Int) -> Int {}
func f0(x : Float) -> Float {}
func f1() -> Int {}
func f1() -> Float {}

struct Y {
  func f0(x : Int) -> Int {}
  func f0(x : Float) -> Float {}
  func f1() -> Int {}
  func f1() -> Float {}
}

func testParenOverloads(x : Int, y : Y) {
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