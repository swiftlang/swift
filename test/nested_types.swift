// RUN: %swift %s -verify

struct X {
  typealias MyInt : Int
  func getInt() -> MyInt { return 7 }
  func getFloat() -> MyReal { return 3.14 }
}

extension X {
  typealias MyReal : Double
}
