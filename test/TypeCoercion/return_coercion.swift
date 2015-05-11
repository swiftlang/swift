// RUN: %target-parse-verify-swift

func f1() -> (Int, Int) { return (1, 2) }
func f2() -> (Int, Int) { return (1, 2) }
func f2() -> (Float, Float) { return (1, 2) }

func g() {
  var _ : (a : Int, b : Int) = f1()
  var _ : (a : Int, b : Int) = f2()
}
