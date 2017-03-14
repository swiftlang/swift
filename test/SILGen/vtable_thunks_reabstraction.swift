// RUN: %target-swift-frontend -emit-silgen %s -verify

struct S {}
class B {}
class C: B {}
class D: C {}

class Opaque<T> {
  typealias ObnoxiousTuple = (T, (T.Type, (T) -> T))

  func inAndOut(x: T) -> T { return x }
  func inAndOutGeneric<U>(x: T, y: U) -> U { return y }
  func inAndOutMetatypes(x: T.Type) -> T.Type { return x }
  func inAndOutFunctions(x: @escaping (T) -> T) -> (T) -> T { return x }
  func inAndOutTuples(x: ObnoxiousTuple) -> ObnoxiousTuple { return x }
  func variantOptionality(x: T) -> T? { return x }
  func variantOptionalityMetatypes(x: T.Type) -> T.Type? { return x }
  func variantOptionalityFunctions(x: @escaping (T) -> T) -> ((T) -> T)? { return x }
  func variantOptionalityTuples(x: ObnoxiousTuple) -> ObnoxiousTuple? { return x }
}

class StillOpaque<T>: Opaque<T> {
  override func variantOptionalityTuples(x: ObnoxiousTuple?) -> ObnoxiousTuple { return x! }
}

class ConcreteValue<X>: Opaque<S> {
  override func inAndOut(x: S) -> S { return x }
  override func inAndOutGeneric<Z>(x: S, y: Z) -> Z { return y }
  override func inAndOutMetatypes(x: S.Type) -> S.Type { return x }
  override func inAndOutFunctions(x: @escaping (S) -> S) -> (S) -> S { return x }
  override func inAndOutTuples(x: ObnoxiousTuple) -> ObnoxiousTuple { return x }
  override func variantOptionality(x: S?) -> S { return x! }
  override func variantOptionalityMetatypes(x: S.Type?) -> S.Type { return x! }
  override func variantOptionalityFunctions(x: ((S) -> S)?) -> (S) -> S { return x! }
  override func variantOptionalityTuples(x: ObnoxiousTuple?) -> ObnoxiousTuple { return x! }
}

class ConcreteClass<X>: Opaque<C> {
  override func inAndOut(x: C) -> C { return x }
  override func inAndOutMetatypes(x: C.Type) -> C.Type { return x }
  override func inAndOutFunctions(x: @escaping (C) -> C) -> (C) -> C { return x }
  override func inAndOutTuples(x: ObnoxiousTuple) -> ObnoxiousTuple { return x }
  override func variantOptionality(x: C?) -> C { return x! }
  override func variantOptionalityMetatypes(x: C.Type?) -> C.Type { return x! }
  override func variantOptionalityFunctions(x: ((C) -> C)?) -> (C) -> C { return x! }
  override func variantOptionalityTuples(x: ObnoxiousTuple?) -> ObnoxiousTuple { return x! }
}

class ConcreteClassVariance<X>: Opaque<C> {
  override func inAndOut(x: B) -> D { return x as! D }
  override func variantOptionality(x: B?) -> D { return x as! D }
}

class OpaqueTuple<U>: Opaque<(U, U)> {
  override func inAndOut(x: (U, U)) -> (U, U) { return x }
  override func variantOptionality(x: (U, U)?) -> (U, U) { return x! }
}

class ConcreteTuple<X>: Opaque<(S, S)> {
  override func inAndOut(x: (S, S)) -> (S, S) { return x }
  override func variantOptionality(x: (S, S)?) -> (S, S) { return x! }
}

class OpaqueFunction<U, V>: Opaque<(U) -> V> {
  override func inAndOut(x: @escaping (U) -> V) -> (U) -> V { return x }
  override func variantOptionality(x: ((U) -> V)?) -> (U) -> V { return x! }
}

class ConcreteFunction<X>: Opaque<(S) -> S> {
  override func inAndOut(x: @escaping (S) -> S) -> (S) -> S { return x }
  override func variantOptionality(x: ((S) -> S)?) -> (S) -> S { return x! }
}

class OpaqueMetatype<U>: Opaque<U.Type> {
  override func inAndOut(x: U.Type) -> U.Type { return x }
  override func variantOptionality(x: U.Type?) -> U.Type { return x! }
}

class ConcreteValueMetatype<X>: Opaque<S.Type> {
  override func inAndOut(x: S.Type) -> S.Type { return x }
  override func variantOptionality(x: S.Type?) -> S.Type { return x! }
}

class ConcreteClassMetatype<X>: Opaque<C.Type> {
  override func inAndOut(x: C.Type) -> C.Type { return x }
  override func variantOptionality(x: C.Type?) -> C.Type { return x! }
}

class ConcreteOptional<X>: Opaque<S?> {
  override func inAndOut(x: S?) -> S? { return x }
  // override func variantOptionality(x: S??) -> S? { return x! }
}

// Make sure we remap the method's innermost generic parameters
// to the correct depth
class GenericBase<T> {
  func doStuff<U>(t: T, u: U) {}
  init<U>(t: T, u: U) {}
}

class ConcreteSub : GenericBase<Int> {
  override func doStuff<U>(t: Int, u: U) {
    super.doStuff(t: t, u: u)
  }
  override init<U>(t: Int, u: U) {
    super.init(t: t, u: u)
  }
}

class ConcreteBase {
  init<U>(t: Int, u: U) {}
  func doStuff<U>(t: Int, u: U) {}
}

class GenericSub<T> : ConcreteBase {
  override init<U>(t: Int, u: U) {
    super.init(t: t, u: u)
  }
  override func doStuff<U>(t: Int, u: U) {
    super.doStuff(t: t, u: u)
  }
}
