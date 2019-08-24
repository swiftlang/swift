// RUN: %target-swift-emit-silgen -module-name inlinable_attribute -emit-verbose-sil -warnings-as-errors %s | %FileCheck %s

// CHECK-LABEL: sil [serialized] [ossa] @$s19inlinable_attribute15fragileFunctionyyF : $@convention(thin) () -> ()
@inlinable public func fragileFunction() {

}

public struct MySt {
  // CHECK-LABEL: sil [serialized] [ossa] @$s19inlinable_attribute4MyStV6methodyyF : $@convention(method) (MySt) -> ()
  @inlinable public func method() {}

  // CHECK-LABEL: sil [serialized] [ossa] @$s19inlinable_attribute4MyStV8propertySivg : $@convention(method) (MySt) -> Int
  @inlinable public var property: Int {
    return 5
  }

  // CHECK-LABEL: sil [serialized] [ossa] @$s19inlinable_attribute4MyStVyS2icig : $@convention(method) (Int, MySt) -> Int
  @inlinable public subscript(x: Int) -> Int {
    return x
  }
}

public class MyCls {
  // CHECK-LABEL: sil [serialized] [ossa] @$s19inlinable_attribute5MyClsCfD : $@convention(method) (@owned MyCls) -> ()
  @inlinable deinit {}

  // Allocating entry point is [serialized]

  // CHECK-LABEL: sil [serialized] [exact_self_class] [ossa] @$s19inlinable_attribute5MyClsC14designatedInitACyt_tcfC : $@convention(method) (@thick MyCls.Type) -> @owned MyCls
  public init(designatedInit: ()) {}

  // Note -- convenience init is intentionally not [serialized]

  // CHECK-LABEL: sil [ossa] @$s19inlinable_attribute5MyClsC15convenienceInitACyt_tcfC : $@convention(method) (@thick MyCls.Type) -> @owned MyCls
  public convenience init(convenienceInit: ()) {
    self.init(designatedInit: ())
  }
}

// Make sure enum case constructors for public and versioned enums are
// [serialized].
@usableFromInline enum MyEnum {
  case c(MySt)
}

// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] [ossa] @$s19inlinable_attribute6MyEnumO1cyAcA0C2StVcACmFTc : $@convention(thin) (@thin MyEnum.Type) -> @owned @callee_guaranteed (MySt) -> MyEnum

@inlinable public func referencesMyEnum() {
  _ = MyEnum.c
}

// CHECK-LABEL: sil non_abi [transparent] [serialized] [ossa] @$s19inlinable_attribute15HasInitializersV1xSivpfi : $@convention(thin) () -> Int
// CHECK-LABEL: sil non_abi [transparent] [serialized] [ossa] @$s19inlinable_attribute15HasInitializersV1ySivpfi : $@convention(thin) () -> Int

@frozen
public struct HasInitializers {
  public let x = 1234
  internal let y = 4321

  @inlinable public init() {}
}

public class Horse {
  public func gallop() {}
}

// CHECK-LABEL: sil [serialized] [ossa] @$s19inlinable_attribute15talkAboutAHorse1hyAA5HorseC_tF : $@convention(thin) (@guaranteed Horse) -> () {
// CHECK: function_ref @$s19inlinable_attribute5HorseC6gallopyyFTc
// CHECK: return
// CHECK: }

// CHECK-LABEL: sil shared [serializable] [thunk] [ossa] @$s19inlinable_attribute5HorseC6gallopyyFTc : $@convention(thin) (@guaranteed Horse) -> @owned @callee_guaranteed () -> () {
// CHECK: class_method
// CHECK: return
// CHECK: }

@inlinable public func talkAboutAHorse(h: Horse) {
  _ = h.gallop
}

@_fixed_layout
public class PublicBase {
  @inlinable
  public init(horse: Horse) {}
}

@usableFromInline
@_fixed_layout
class UFIBase {
  @usableFromInline
  @inlinable
  init(horse: Horse) {}
}

// CHECK-LABEL: sil [serialized] [ossa] @$s19inlinable_attribute017PublicDerivedFromC0Cfd : $@convention(method) (@guaranteed PublicDerivedFromPublic) -> @owned Builtin.NativeObject
// CHECK-LABEL: sil [serialized] [ossa] @$s19inlinable_attribute017PublicDerivedFromC0CfD : $@convention(method) (@owned PublicDerivedFromPublic) -> ()

// Make sure the synthesized delegating initializer is inlinable also

// CHECK-LABEL: sil [serialized] [ossa] @$s19inlinable_attribute017PublicDerivedFromC0C5horseAcA5HorseC_tcfc : $@convention(method) (@owned Horse, @owned PublicDerivedFromPublic) -> @owned PublicDerivedFromPublic
@_fixed_layout
public class PublicDerivedFromPublic : PublicBase {
  // Allow @inlinable deinits
  @inlinable deinit {}
}

// CHECK-LABEL: sil [serialized] [ossa] @$s19inlinable_attribute20UFIDerivedFromPublicC5horseAcA5HorseC_tcfc : $@convention(method) (@owned Horse, @owned UFIDerivedFromPublic) -> @owned UFIDerivedFromPublic
@usableFromInline
@_fixed_layout
class UFIDerivedFromPublic : PublicBase {
}

// CHECK-LABEL: sil [serialized] [ossa] @$s19inlinable_attribute17UFIDerivedFromUFIC5horseAcA5HorseC_tcfc : $@convention(method) (@owned Horse, @owned UFIDerivedFromUFI) -> @owned UFIDerivedFromUFI
@usableFromInline
@_fixed_layout
class UFIDerivedFromUFI : UFIBase {
  // Allow @inlinable deinits
  @inlinable deinit {}
}

// CHECK-LABEL: sil hidden [ossa] @$s19inlinable_attribute25InternalDerivedFromPublicC5horseAcA5HorseC_tcfc : $@convention(method) (@owned Horse, @owned InternalDerivedFromPublic) -> @owned InternalDerivedFromPublic
class InternalDerivedFromPublic : PublicBase {}

// CHECK-LABEL: sil hidden [ossa] @$s19inlinable_attribute22InternalDerivedFromUFIC5horseAcA5HorseC_tcfc : $@convention(method) (@owned Horse, @owned InternalDerivedFromUFI) -> @owned InternalDerivedFromUFI
class InternalDerivedFromUFI : UFIBase {}

// CHECK-LABEL: sil private [ossa] @$s19inlinable_attribute24PrivateDerivedFromPublic{{.+}}LLC5horseAdA5HorseC_tcfc : $@convention(method) (@owned Horse, @owned PrivateDerivedFromPublic) -> @owned PrivateDerivedFromPublic
private class PrivateDerivedFromPublic : PublicBase {}

// CHECK-LABEL: sil private [ossa] @$s19inlinable_attribute21PrivateDerivedFromUFI{{.+}}LLC5horseAdA5HorseC_tcfc : $@convention(method) (@owned Horse, @owned PrivateDerivedFromUFI) -> @owned PrivateDerivedFromUFI
private class PrivateDerivedFromUFI : UFIBase {}

// Make sure that nested functions are also serializable.

// CHECK-LABEL: sil [serialized] [ossa] @$s19inlinable_attribute3basyyF
@inlinable
public func bas() {
  // CHECK-LABEL: sil shared [serializable] [ossa] @$s19inlinable_attribute3basyyF3zimL_yyF
  func zim() {
    // CHECK-LABEL: sil shared [serializable] [ossa] @$s19inlinable_attribute3basyyF3zimL_yyF4zangL_yyF
    func zang() { }
  }

  // CHECK-LABEL: sil shared [serialized] [ossa] @$s19inlinable_attribute3bas{{[_0-9a-zA-Z]*}}U_
  let _ = {
    // CHECK-LABEL: sil shared [serializable] [ossa] @$s19inlinable_attribute3basyyFyycfU_7zippityL_yyF
    func zippity() { }
  }
}

// CHECK-LABEL: sil [ossa] @$s19inlinable_attribute6globalyS2iF : $@convention(thin) (Int) -> Int
public func global(_ x: Int) -> Int { return x }

// CHECK-LABEL: sil [serialized] [ossa] @$s19inlinable_attribute16cFunctionPointeryyF : $@convention(thin) () -> ()
@inlinable func cFunctionPointer() {
  // CHECK: function_ref @$s19inlinable_attribute6globalyS2iFTo
  let _: @convention(c) (Int) -> Int = global

  // CHECK: function_ref @$s19inlinable_attribute16cFunctionPointeryyFS2icfU_To
  let _: @convention(c) (Int) -> Int = { return $0 }

  func local(_ x: Int) -> Int {
    return x
  }

  // CHECK: function_ref @$s19inlinable_attribute16cFunctionPointeryyF5localL_yS2iFTo
  let _: @convention(c) (Int) -> Int = local
}

// CHECK-LABEL: sil shared [serializable] [thunk] [ossa] @$s19inlinable_attribute6globalyS2iFTo : $@convention(c) (Int) -> Int
// CHECK: function_ref @$s19inlinable_attribute6globalyS2iF
// CHECK: return

// CHECK-LABEL: sil shared [serialized] [ossa] @$s19inlinable_attribute16cFunctionPointeryyFS2icfU_ : $@convention(thin) (Int) -> Int {

// CHECK-LABEL: sil shared [serializable] [thunk] [ossa] @$s19inlinable_attribute16cFunctionPointeryyFS2icfU_To : $@convention(c) (Int) -> Int {
// CHECK: function_ref @$s19inlinable_attribute16cFunctionPointeryyFS2icfU_
// CHECK: return

// CHECK-LABEL: sil shared [serializable] [ossa] @$s19inlinable_attribute16cFunctionPointeryyF5localL_yS2iF : $@convention(thin) (Int) -> Int {

// CHECK-LABEL: sil shared [serializable] [thunk] [ossa] @$s19inlinable_attribute16cFunctionPointeryyF5localL_yS2iFTo : $@convention(c) (Int) -> Int {
// CHECK: function_ref @$s19inlinable_attribute16cFunctionPointeryyF5localL_yS2iF
// CHECK: return
