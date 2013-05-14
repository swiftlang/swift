// RUN: %swift -parse %s -verify

func takeIntToInt(f : (Int) -> Int) { }
func takeIntIntToInt(f : (Int, Int) -> Int) { }

// Simple closures
func simple() {
  takeIntToInt({|x : Int| -> Int
    return x + 1
  })
  takeIntIntToInt({|x : Int, y : Int| -> Int
    return x + y
  })
}

// Closures with variadic argument lists
func variadic() {
  var f = {|start : Int, rest : Int...| -> Int
    for x in rest {
      start += x
    }
    return start
  }
  f(1)
  f(1, 2)
  f(1, 3)
}

