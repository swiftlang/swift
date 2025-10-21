// RUN: %target-swift-frontend %s -emit-ir -o /dev/null

protocol P {
  static var name: String { get }
  init(i:Int)
}

class A: P {
  class var name: String {
    get { return "A" }
  }
  required init(i:Int) {}
}
class B: P {
  class var name: String {
    get { return "A" }
  }
  required init(i:Int) {}
}

let cls:P.Type = A.self

let p:P = cls.init(i:1)

func markUsed<T>(_ t: T) {}
markUsed(p)

