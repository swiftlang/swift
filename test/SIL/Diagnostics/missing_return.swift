// RUN: %swift %s -verify

func singleBlock() -> Int {
  var y = 0 
} // expected-warning {{missing return in the function expected to return Int}}

func singleBlock2() -> Int {
  var y = 0 
  y++
} // expected-warning {{missing return in the function expected to return Int}}

func multipleBlocksSingleMissing(b:Bool) -> (String, Int) {
  var y = 0 
  if b {
    return ("a", 1)
  } else if (y == 0) {
    y++
  }
} // expected-warning {{missing return in the function expected to return (String, Int)}}

func multipleBlocksAllMissing(x:Int) -> Int {
  var y : Int = x + 1 
  while (y > 0 ) {
    --y;
    break;
  }
  var x: Int
  x++
} // expected-warning {{missing return in the function expected to return Int}}

// FIXME: We should not report the missing return here as this is a 
// no-return function. After we fix this, we can turn the warning into error.
func MYsubscriptNonASCII(idx : Int) -> Char {
  alwaysTrap() 
} // expected-warning {{missing return in the function expected to return Char}}
