// RUN: %target-parse-verify-swift

func f1() -> (Int, Int) { return (1, 2) }
func f2() -> (Int, Int) { return (1, 2) }
func f2() -> (Float, Float) { return (1, 2) }

func g() {
  var x1 : (a : Int, b : Int) = f1()
  var x2 : (a : Int, b : Int) = f2()
}
