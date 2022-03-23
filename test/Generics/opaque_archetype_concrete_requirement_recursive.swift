// RUN: %target-swift-frontend -emit-silgen %s -disable-availability-checking

protocol P {
  associatedtype T

  var t: T { get }
}

protocol RecursiveP {
  associatedtype T : RecursiveP
}

struct S_RecursiveP : RecursiveP {
  typealias T = S_RecursiveP
}

struct DefinesRecursiveP : P {
  var t: some RecursiveP {
    return S_RecursiveP()
  }
}

protocol HasRecursiveP {
  associatedtype T : RecursiveP
}

extension HasRecursiveP where T == DefinesRecursiveP.T {
  func checkSameType(_ t: T) -> DefinesRecursiveP.T { return t }
}

