// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership -emit-verbose-sil %s | %FileCheck %s

// CHECK-LABEL: sil [serialized] @_T020inlineable_attribute15fragileFunctionyyF : $@convention(thin) () -> ()
@_inlineable public func fragileFunction() {

}

public struct MySt {
  // CHECK-LABEL: sil [serialized] @_T020inlineable_attribute4MyStV6methodyyF : $@convention(method) (MySt) -> ()
  @_inlineable public func method() {}

  // CHECK-LABEL: sil [serialized] @_T020inlineable_attribute4MyStV8propertySivg : $@convention(method) (MySt) -> Int
  @_inlineable public var property: Int {
    return 5
  }

  // CHECK-LABEL: sil [serialized] @_T020inlineable_attribute4MyStVS2icig : $@convention(method) (Int, MySt) -> Int
  @_inlineable public subscript(x: Int) -> Int {
    return x
  }
}

public class MyCls {
  // CHECK-LABEL: sil [serialized] @_T020inlineable_attribute5MyClsCfD : $@convention(method) (@owned MyCls) -> ()
  @_inlineable deinit {}

  // Allocating entry point is [serialized]

  // CHECK-LABEL: sil [serialized] @_T020inlineable_attribute5MyClsCACyt14designatedInit_tcfC : $@convention(method) (@thick MyCls.Type) -> @owned MyCls
  public init(designatedInit: ()) {}

  // Note -- convenience init is intentionally not [serialized]

  // CHECK-LABEL: sil @_T020inlineable_attribute5MyClsCACyt15convenienceInit_tcfC : $@convention(method) (@thick MyCls.Type) -> @owned MyCls
  public convenience init(convenienceInit: ()) {
    self.init(designatedInit: ())
  }
}

// Make sure enum case constructors for public and versioned enums are
// [serialized].
@_versioned enum MyEnum {
  case c(MySt)
}

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] @_T020inlineable_attribute6MyEnumO1cAcA0C2StVcACmFTc : $@convention(thin) (@thin MyEnum.Type) -> @owned @callee_guaranteed (MySt) -> MyEnum

@_inlineable public func referencesMyEnum() {
  _ = MyEnum.c
}

// CHECK-LABEL: sil [transparent] @_T020inlineable_attribute15HasInitializersV1xSivpfi : $@convention(thin) () -> Int

public struct HasInitializers {
  public let x = 1234

  @_inlineable public init() {}
}

public class Horse {
  public func gallop() {}
}

// CHECK-LABEL: sil [serialized] @_T020inlineable_attribute15talkAboutAHorseyAA5HorseC1h_tF : $@convention(thin) (@owned Horse) -> () {
// CHECK: function_ref @_T020inlineable_attribute5HorseC6gallopyyFTc
// CHECK: return
// CHECK: }

// CHECK-LABEL: sil shared [serializable] [thunk] @_T020inlineable_attribute5HorseC6gallopyyFTc : $@convention(thin) (@owned Horse) -> @owned @callee_guaranteed () -> () {
// CHECK: class_method
// CHECK: return
// CHECK: }

@_inlineable public func talkAboutAHorse(h: Horse) {
  _ = h.gallop
}

@_versioned class Base {
  @_versioned
  @_inlineable
  init(horse: Horse) {}
}

// CHECK-LABEL: sil [serialized] @_T020inlineable_attribute7DerivedCfd : $@convention(method) (@guaranteed Derived) -> @owned Builtin.NativeObject
// CHECK-LABEL: sil [serialized] @_T020inlineable_attribute7DerivedCfD : $@convention(method) (@owned Derived) -> ()

// Make sure the synthesized delegating initializer is inlineable also

// CHECK-LABEL: sil [serialized] @_T020inlineable_attribute7DerivedCAcA5HorseC5horse_tcfc : $@convention(method) (@owned Horse, @owned Derived) -> @owned Derived
@_versioned class Derived : Base {
  // Allow @_inlineable deinits
  @_inlineable deinit {}
}
