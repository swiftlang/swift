// RUN: %target-swift-frontend -emit-silgen %s -verify

struct S {}
class B {}
class C: B {}
class D: C {}

class Opaque<T> {
  typealias ObnoxiousTuple = (T, (T.Type, T -> T))

  func inAndOut(_ x: T) -> T { return x }
  func inAndOutGeneric<U>(_ x: T, y: U) -> U { return y }
  func inAndOutMetatypes(_ x: T.Type) -> T.Type { return x }
  func inAndOutFunctions(_ x: T -> T) -> T -> T { return x }
  func inAndOutTuples(_ x: ObnoxiousTuple) -> ObnoxiousTuple { return x }
  func variantOptionality(_ x: T) -> T? { return x }
  func variantOptionalityMetatypes(_ x: T.Type) -> T.Type? { return x }
  func variantOptionalityFunctions(_ x: T -> T) -> (T -> T)? { return x }
  func variantOptionalityTuples(_ x: ObnoxiousTuple) -> ObnoxiousTuple? { return x }
}

class StillOpaque<T>: Opaque<T> {
  override func variantOptionalityTuples(_ x: ObnoxiousTuple?) -> ObnoxiousTuple { return x! }
}

class ConcreteValue<X>: Opaque<S> {
  override func inAndOut(_ x: S) -> S { return x }
  override func inAndOutGeneric<Z>(_ x: S, y: Z) -> Z { return y }
  override func inAndOutMetatypes(_ x: S.Type) -> S.Type { return x }
  override func inAndOutFunctions(_ x: S -> S) -> S -> S { return x }
  override func inAndOutTuples(_ x: ObnoxiousTuple) -> ObnoxiousTuple { return x }
  override func variantOptionality(_ x: S?) -> S { return x! }
  override func variantOptionalityMetatypes(_ x: S.Type?) -> S.Type { return x! }
  override func variantOptionalityFunctions(_ x: (S -> S)?) -> S -> S { return x! }
  override func variantOptionalityTuples(_ x: ObnoxiousTuple?) -> ObnoxiousTuple { return x! }
}

class ConcreteClass<X>: Opaque<C> {
  override func inAndOut(_ x: C) -> C { return x }
  override func inAndOutMetatypes(_ x: C.Type) -> C.Type { return x }
  override func inAndOutFunctions(_ x: C -> C) -> C -> C { return x }
  override func inAndOutTuples(_ x: ObnoxiousTuple) -> ObnoxiousTuple { return x }
  override func variantOptionality(_ x: C?) -> C { return x! }
  override func variantOptionalityMetatypes(_ x: C.Type?) -> C.Type { return x! }
  override func variantOptionalityFunctions(_ x: (C -> C)?) -> C -> C { return x! }
  override func variantOptionalityTuples(_ x: ObnoxiousTuple?) -> ObnoxiousTuple { return x! }
}

class ConcreteClassVariance<X>: Opaque<C> {
  override func inAndOut(_ x: B) -> D { return x as! D }
  override func variantOptionality(_ x: B?) -> D { return x as! D }
}

class OpaqueTuple<U>: Opaque<(U, U)> {
  override func inAndOut(_ x: (U, U)) -> (U, U) { return x }
  override func variantOptionality(_ x: (U, U)?) -> (U, U) { return x! }
}

class ConcreteTuple<X>: Opaque<(S, S)> {
  override func inAndOut(_ x: (S, S)) -> (S, S) { return x }
  override func variantOptionality(_ x: (S, S)?) -> (S, S) { return x! }
}

class OpaqueFunction<U, V>: Opaque<U -> V> {
  override func inAndOut(_ x: U -> V) -> U -> V { return x }
  override func variantOptionality(_ x: (U -> V)?) -> U -> V { return x! }
}

class ConcreteFunction<X>: Opaque<S -> S> {
  override func inAndOut(_ x: S -> S) -> S -> S { return x }
  override func variantOptionality(_ x: (S -> S)?) -> S -> S { return x! }
}

class OpaqueMetatype<U>: Opaque<U.Type> {
  override func inAndOut(_ x: U.Type) -> U.Type { return x }
  override func variantOptionality(_ x: U.Type?) -> U.Type { return x! }
}

class ConcreteValueMetatype<X>: Opaque<S.Type> {
  override func inAndOut(_ x: S.Type) -> S.Type { return x }
  override func variantOptionality(_ x: S.Type?) -> S.Type { return x! }
}

class ConcreteClassMetatype<X>: Opaque<C.Type> {
  override func inAndOut(_ x: C.Type) -> C.Type { return x }
  override func variantOptionality(_ x: C.Type?) -> C.Type { return x! }
}

/*
class ConcreteOptional<X>: Opaque<S?> {
  override func inAndOut(_ x: S?) -> S? { return x }
  override func variantOptionality(_ x: S??) -> S? { return x! }
}
 */
