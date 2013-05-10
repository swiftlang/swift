// RUN: %swift -parse %s -verify

// Simple subscript of arrays:
func simpleSubscript(array : Float[], x : Int) -> Float {
  var f = array[x]
  return array[x]
}

