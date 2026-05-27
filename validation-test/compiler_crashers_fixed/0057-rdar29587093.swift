// RUN: %target-swift-frontend %s -emit-ir

protocol P {
  associatedtype A
  func f()
}

extension P where A == Int {
  func f() {
    print("cool")
  }
}
extension P {
  func f() { print("semi-uncool") }
  func g() {
    f()
  }
}
struct X<T> : P {
  typealias A = T
}

extension X where A == Int {
  func f() {
    print("cool2")
  }
}

X<Int>().f()
X<Int>().g()
