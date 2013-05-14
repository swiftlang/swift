// RUN: %swift -parse %s -verify

func takeIntToInt(f : (Int) -> Int) { }
func takeIntIntToInt(f : (Int, Int) -> Int) { }

// Anonymous arguments with inference
func myMap<T, U>(array : T[], f : (T) -> U) -> U[] {}

func testMap(array : Int[]) {
  var farray = myMap(array, { Float($0) })
  var f : Float = farray[0]
  var farray2 = myMap(array, { |x| Float(x) })
  farray = farray2
}

// Infer result type.
func testResultType() {
  takeIntToInt({|x|
    return x + 1
  })

  takeIntIntToInt({|x, y|
    return 2 + 3
  })
}
