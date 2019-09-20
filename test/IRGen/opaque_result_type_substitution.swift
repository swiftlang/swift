// RUN: %target-swift-frontend -disable-availability-checking -emit-ir -primary-file %s | %FileCheck %s

protocol E {}

struct Pair<T, V> : E {
  var fst : T
  var snd : V

  init(_ f: T, _ s: V) {
    self.fst = f
    self.snd = s
  }

  func foobar() -> some E {
    return self
  }
}

public func usePair<T, V>(_ t: T, _ v: V) {
  var x = Pair(t, v)
  let q = x.foobar()
  let u = x.foobar()
  let p = Pair(q, u)
  print(p)
}

protocol P { }

struct C<S : Hashable> {
    struct Inner {}

    init(_ s: S) { }

    func getInner() -> Inner {
      return Inner()
    }
}

struct O<T> {
  var t: T

  init(_ t: T) {
    self.t = t
  }
}

struct M<T, V> : P {
  init(_ f: T, _ s: V) {
  }

  func foobar() -> some P {
    return self
  }
}

public func test2<S : Hashable, T, V>(_ s: S, _ t: T, _ v: V) {
  var x = M(C(s).getInner(), t)
  let q = x.foobar()
  let u = x.foobar()
  let y = O(q)
  print(y)
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s31opaque_result_type_substitution7usePairyyx_q_tr0_lF"({{.*}}, %swift.type* %T, %swift.type* %V)
// CHECK:  [[PAIR_TV:%.*]] = call swiftcc %swift.metadata_response @"$s31opaque_result_type_substitution4PairVMa"({{.*}}, %swift.type* %T, %swift.type* %V)
// CHECK:  [[MD:%.*]] = extractvalue %swift.metadata_response [[PAIR_TV]], 0
// CHECK:  [[PAIR_OPAQUE:%.*]] = call swiftcc %swift.metadata_response @"$s31opaque_result_type_substitution4PairVMa"({{.*}}, %swift.type* [[MD]], %swift.type* [[MD]])
// CHECK:  [[MD2:%.*]] = extractvalue %swift.metadata_response [[PAIR_OPAQUE]], 0
// CHECK:  call {{.*}}* @"$s31opaque_result_type_substitution4PairVyAC6foobarQryFQOyxq__Qo_AEGr0_lWOh"({{.*}}, %swift.type* {{.*}}, %swift.type* [[MD2]])

public protocol Thing { }

public struct Thingy : Thing {}

public protocol KeyProto {
  associatedtype Value
}

extension KeyProto {
  public static func transform3<T : Thing>(
    _ transform: @escaping (A<Self>) -> T)
  -> some Thing {
      return Thingy()
  }
}

public struct A<Key : KeyProto> {}

extension A {
  public func transform2<T>(_ transform: @escaping (Key.Value) -> T) -> Thingy {
    return Thingy()
  }
}

struct AKey : KeyProto {
  typealias Value = Int
}

extension Thing {
  public func transform<K>(key _: K.Type = K.self, transform: @escaping (inout K) -> Void) -> some Thing {
    return Thingy()
  }
}

struct OutterThing<Content : Thing> : Thing {
  let content: Content

  init(_ c: Content) {
    self.content = c
  }

  var body: some Thing {
    return AKey.transform3 { y in
      y.transform2 { i in
       self.content.transform(
         key: Thingy.self) { value in }
      }
    }
  }
}

public protocol W {}

struct Key : W {}

extension W {
  public static func transform(_ transform: @escaping (P1<Self>) -> ()) -> some W {
    return Key()
  }
}

public struct P1<Key : W> { }

extension P1 {
  public func transform2<T>(_ transform: @escaping (Key) -> T) { }
}

public struct T<Content> : W {
  public init(content : ()->Content) {}
}

public struct Content<Content> : W {
  public init(content: Content) {}
}

extension W {
  func moo() -> some W {
    return Content(content: self)
  }
}

struct Test<Label : W> {
  var label: Label
  // This function used to crash.
  var dontCrash: some W {
    return Key.transform { y in
      y.transform2 { i in
        T() {
          return self.label
        }.moo()
      }
    }
  }
}
