
// RUN: %target-swift-emit-silgen -module-name inlinable_attribute -enable-sil-ownership -emit-verbose-sil %s | %FileCheck %s

// CHECK-LABEL: sil [serialized] @$S19inlinable_attribute15fragileFunctionyyF : $@convention(thin) () -> ()
@inlinable public func fragileFunction() {

}

public struct MySt {
  // CHECK-LABEL: sil [serialized] @$S19inlinable_attribute4MyStV6methodyyF : $@convention(method) (MySt) -> ()
  @inlinable public func method() {}

  // CHECK-LABEL: sil [serialized] @$S19inlinable_attribute4MyStV8propertySivg : $@convention(method) (MySt) -> Int
  @inlinable public var property: Int {
    return 5
  }

  // CHECK-LABEL: sil [serialized] @$S19inlinable_attribute4MyStVyS2icig : $@convention(method) (Int, MySt) -> Int
  @inlinable public subscript(x: Int) -> Int {
    return x
  }
}

public class MyCls {
  // CHECK-LABEL: sil [serialized] @$S19inlinable_attribute5MyClsCfD : $@convention(method) (@owned MyCls) -> ()
  @inlinable deinit {}

  // Allocating entry point is [serialized]

  // CHECK-LABEL: sil [serialized] @$S19inlinable_attribute5MyClsC14designatedInitACyt_tcfC : $@convention(method) (@thick MyCls.Type) -> @owned MyCls
  public init(designatedInit: ()) {}

  // Note -- convenience init is intentionally not [serialized]

  // CHECK-LABEL: sil @$S19inlinable_attribute5MyClsC15convenienceInitACyt_tcfC : $@convention(method) (@thick MyCls.Type) -> @owned MyCls
  public convenience init(convenienceInit: ()) {
    self.init(designatedInit: ())
  }
}

// Make sure enum case constructors for public and versioned enums are
// [serialized].
@usableFromInline enum MyEnum {
  case c(MySt)
}

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] @$S19inlinable_attribute6MyEnumO1cyAcA0C2StVcACmFTc : $@convention(thin) (@thin MyEnum.Type) -> @owned @callee_guaranteed (MySt) -> MyEnum

@inlinable public func referencesMyEnum() {
  _ = MyEnum.c
}

// CHECK-LABEL: sil non_abi [transparent] [serialized] @$S19inlinable_attribute15HasInitializersV1xSivpfi : $@convention(thin) () -> Int
// CHECK-LABEL: sil non_abi [transparent] [serialized] @$S19inlinable_attribute15HasInitializersV1ySivpfi : $@convention(thin) () -> Int

@_fixed_layout
public struct HasInitializers {
  public let x = 1234
  internal let y = 4321

  @inlinable public init() {}
}

public class Horse {
  public func gallop() {}
}

// CHECK-LABEL: sil [serialized] @$S19inlinable_attribute15talkAboutAHorse1hyAA5HorseC_tF : $@convention(thin) (@guaranteed Horse) -> () {
// CHECK: function_ref @$S19inlinable_attribute5HorseC6gallopyyFTc
// CHECK: return
// CHECK: }

// CHECK-LABEL: sil shared [serializable] [thunk] @$S19inlinable_attribute5HorseC6gallopyyFTc : $@convention(thin) (@guaranteed Horse) -> @owned @callee_guaranteed () -> () {
// CHECK: class_method
// CHECK: return
// CHECK: }

@inlinable public func talkAboutAHorse(h: Horse) {
  _ = h.gallop
}

@usableFromInline
@_fixed_layout
class Base {
  @usableFromInline
  @inlinable
  init(horse: Horse) {}
}

// CHECK-LABEL: sil [serialized] @$S19inlinable_attribute7DerivedCfd : $@convention(method) (@guaranteed Derived) -> @owned Builtin.NativeObject
// CHECK-LABEL: sil [serialized] @$S19inlinable_attribute7DerivedCfD : $@convention(method) (@owned Derived) -> ()

// Make sure the synthesized delegating initializer is inlinable also

// CHECK-LABEL: sil [serialized] @$S19inlinable_attribute7DerivedC5horseAcA5HorseC_tcfc : $@convention(method) (@owned Horse, @owned Derived) -> @owned Derived
@usableFromInline
@_fixed_layout
class Derived : Base {
  // Allow @inlinable deinits
  @inlinable deinit {}
}
