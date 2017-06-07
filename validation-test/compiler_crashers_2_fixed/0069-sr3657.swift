// RUN: %target-swift-frontend %s -emit-ir

protocol P3 {
  associatedtype T
}

class C : P3 {
  typealias T = Int
}

func superclassConformance1<T>(_: T, t: T.T) where T : P3, T : C {}
