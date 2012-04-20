// RUN: %swift -I %S/.. %s -verify

struct X { }
struct Y { }

var a : X
var a : Y

func test_assign(x : X, y : Y) {
  a = x
  a = y
  x = a
  y = a
}