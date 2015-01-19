// RUN: %target-parse-verify-swift

func takeIntToInt(f: (Int) -> Int) { }
func takeIntIntToInt(f: (Int, Int) -> Int) { }

// Simple closures
func simple() {
  takeIntToInt({(x: Int) -> Int in
    return x + 1
  })
  takeIntIntToInt({(x: Int, y: Int) -> Int in
    return x + y
  })
}

// Closures with variadic argument lists
func variadic() {
  var f = {(start: Int, rest: Int...) -> Int in
    var result = start
    for x in rest {
      result += x
    }
    return result
  }
  f(1)
  f(1, 2)
  f(1, 3)
}

// Closures with attributes in the parameter list.
func attrs() {
  var f1 = {(inout z: Int) -> Int in z }
}

// Closures with argument and parameter names.
func argAndParamNames() -> Int {
  var f1: (x: Int, y: Int) -> Int = { (a x, b y) in x + y }
  f1(x: 1, y: 2)
  return f1(x: 1, y: 2)
}
