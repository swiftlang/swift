// RUN: %swift %s -verify

import swift

var x : int

func test() {
  var y : x   // expected-error {{'x' does not name a type}}
}
  