// RUN: not --crash %target-swift-emit-silgen %s

// REQUIRES: asserts

protocol Pub {
  associatedtype Other
  associatedtype Failure: Error
}

class AnyPub<Other, Failure: Error> {}

extension Pub {
  func erase() -> AnyPub<Other, Failure> {
    return AnyPub<Other, Failure>()
  }
}

protocol ObsObj : Pub {
  associatedtype NeverPub : Pub where Self.NeverPub.Failure == Never
}

class Subject<Other, Failure: Error> : Pub {}

extension Pub where Other: ObsObj, Other.NeverPub: Subject<Int, Error> {
  static func f() -> AnyPub<Other.NeverPub.Other, Other.NeverPub.Failure> {
    return Subject<Other.NeverPub.Other, Other.NeverPub.Failure>().erase()
  }
}
