import Foundation

@objc
protocol SomeProto {
  func conform()
}

class AnotherClass {}

class SomeClass {
  // Intentionally referencing the wrong class name.
  var x : AnotherClass2? = nil
}

protocol P {}
protocol WithAssoc {
  associatedtype AssocType : P
}

struct BuggyConformer : WithAssoc {
  typealias AssocType = Int
}
