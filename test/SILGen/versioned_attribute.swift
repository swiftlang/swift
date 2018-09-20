// RUN: %target-swift-emit-silgen -enable-sil-ownership -emit-verbose-sil %s | %FileCheck %s

// Check that @usableFromInline entities have public linkage.
// CHECK-LABEL: sil @$s19versioned_attribute25referencedFromTransparentyyF : $@convention(thin) () -> () {
@usableFromInline func referencedFromTransparent() {}

// CHECK-LABEL: sil [serialized] @$s19versioned_attribute23referencesVersionedFuncyycyF : $@convention(thin) () -> @owned @callee_guaranteed () -> () {
@inlinable public func referencesVersionedFunc() -> () -> () {
  return referencedFromTransparent
}

@usableFromInline class Horse {
  var mouth: AnyObject?
}

@usableFromInline class GiftHorse {
  var mouth: AnyObject?

  deinit {}
}

// CHECK-LABEL: sil @$s19versioned_attribute5HorseCfd : $@convention(method) (@guaranteed Horse) -> @owned Builtin.NativeObject
// CHECK-LABEL: sil @$s19versioned_attribute5HorseCfD : $@convention(method) (@owned Horse) -> ()

// CHEKC-LABEL: sil @$s19versioned_attribute9GiftHorseCfd : $@convention(method) (@guaranteed GiftHorse) -> @owned Builtin.NativeObject
// CHECK-LABEL: sil @$s19versioned_attribute9GiftHorseCfD : $@convention(method) (@owned GiftHorse) -> ()
