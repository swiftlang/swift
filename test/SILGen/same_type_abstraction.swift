// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

protocol Associated {
  associatedtype Assoc
}

struct Abstracted<T: Associated, U: Associated> {
  let closure: (T.Assoc) -> U.Assoc
}

struct S1 {}
struct S2 {}

// CHECK-LABEL: sil hidden @$s21same_type_abstraction28callClosureWithConcreteTypes{{[_0-9a-zA-Z]*}}F
// CHECK:         function_ref @$s{{.*}}TR :
func callClosureWithConcreteTypes<T: Associated, U: Associated>(x: Abstracted<T, U>, arg: S1) -> S2 where T.Assoc == S1, U.Assoc == S2 {
  return x.closure(arg)
}

// Problem when a same-type constraint makes an associated type into a tuple

protocol MyProtocol {
    associatedtype ReadData
    associatedtype Data

    func readData() -> ReadData
}

extension MyProtocol where Data == (ReadData, ReadData) {
  // CHECK-LABEL: sil hidden @$s21same_type_abstraction10MyProtocolPAA8ReadDataQz_AEt0G0RtzrlE07currentG0AE_AEtyF : $@convention(method) <Self where Self : MyProtocol, Self.Data == (Self.ReadData, Self.ReadData)> (@in_guaranteed Self) -> (@out Self.ReadData, @out Self.ReadData)
  func currentData() -> Data {
    // CHECK: bb0(%0 : @trivial $*Self.ReadData, %1 : @trivial $*Self.ReadData, %2 : @trivial $*Self):
    // CHECK:   [[READ_FN:%.*]] = witness_method $Self, #MyProtocol.readData!1 : {{.*}} : $@convention(witness_method: MyProtocol) <τ_0_0 where τ_0_0 : MyProtocol> (@in_guaranteed τ_0_0) -> @out τ_0_0.ReadData
    // CHECK:   apply [[READ_FN]]<Self>(%0, %2) : $@convention(witness_method: MyProtocol) <τ_0_0 where τ_0_0 : MyProtocol> (@in_guaranteed τ_0_0) -> @out τ_0_0.ReadData
    // CHECK:   [[READ_FN:%.*]] = witness_method $Self, #MyProtocol.readData!1 : {{.*}} : $@convention(witness_method: MyProtocol) <τ_0_0 where τ_0_0 : MyProtocol> (@in_guaranteed τ_0_0) -> @out τ_0_0.ReadData
    // CHECK:   apply [[READ_FN]]<Self>(%1, %2) : $@convention(witness_method: MyProtocol) <τ_0_0 where τ_0_0 : MyProtocol> (@in_guaranteed τ_0_0) -> @out τ_0_0.ReadData
    // CHECK:   return
    return (readData(), readData())
  }
}

// Problem with protocol typealiases, which are modeled as same-type
// constraints

protocol Refined : Associated {
  associatedtype Key
  typealias Assoc = Key

  init()
}

extension Refined {
  // CHECK-LABEL: sil hidden @$s21same_type_abstraction7RefinedPAAE12withElementsx5AssocQz_tcfC : $@convention(method) <Self where Self : Refined> (@in Self.Assoc, @thick Self.Type) -> @out Self
  init(withElements newElements: Key) {
    self.init()
  }
}
