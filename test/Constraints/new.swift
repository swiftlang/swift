// RUN: %swift -parse -verify %s

struct X<T> { }
var i : Int
func acceptXIntArray(xia: X<Int>[]) {}

class SlowPoint {
  init(i : Int, s : String) {}
}

class Dict<Key, Value> {
  init() {} 
}

func acceptDictStringInt(_: Dict<String, Int>) {}

class OvlCtor {
  init(i : Int) { }
  init(s : String) { }
}

new Int[i]
acceptXIntArray(new X[i])
SlowPoint(i: 1, s: "hello")
acceptDictStringInt(Dict())

OvlCtor(i: 1)
OvlCtor(s: "")

class ArrayLike<T> {
  init() { length = 0 }

  var length : Int
  func clone(x: T) -> ArrayLike<T> {
    new T[length] {i in x}
  }
}

// <rdar://problem/15653973>
{ new Int[$0] }(5)
