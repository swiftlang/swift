// RUN: %swift %s -emit-sil -verify

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

func [noreturn] MYsubscriptNonASCII(idx : Int) -> Char {
} // no-warning

func [noreturn] exit ()->(){}
func [noreturn] tryingToReturn (x:Bool) -> () {
  if x {
    return // expected-error {{return from a 'noreturn' function}}
  }
  exit()
}
