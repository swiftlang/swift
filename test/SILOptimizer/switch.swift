// RUN: %target-swift-frontend -emit-sil %s -verify

func non_fully_covered_switch(x: Int) -> Int {
  var x = x
  switch x { // expected-error{{switch must be exhaustive}}
    case 0:
     x += 1
    case 3:
     x -= 1
  }
  return x
}

