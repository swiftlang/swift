// RUN: %swift -parse -verify %s

enum Foo : Int {
  case A, B, C
}

var raw1: Int = Foo.A.toRaw()
var raw2: Foo.RawType = raw1
var cooked1: Foo? = Foo.fromRaw(0)
var cooked2: Foo? = Foo.fromRaw(22)

enum Bar : Double {
  case A, B, C
}

