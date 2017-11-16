// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership -emit-verbose-sil %s | %FileCheck %s

// Check that @_versioned entities have public linkage.
// CHECK-LABEL: sil @_T019versioned_attribute25referencedFromTransparentyyF : $@convention(thin) () -> () {
@_versioned func referencedFromTransparent() {}

// CHECK-LABEL: sil [serialized] @_T019versioned_attribute23referencesVersionedFuncyycyF : $@convention(thin) () -> @owned @callee_guaranteed () -> () {
@_inlineable public func referencesVersionedFunc() -> () -> () {
  return referencedFromTransparent
}

@_versioned class Horse {
  var mouth: AnyObject?
}

@_versioned class GiftHorse {
  var mouth: AnyObject?

  deinit {}
}

// CHECK-LABEL: sil @_T019versioned_attribute5HorseCfd : $@convention(method) (@guaranteed Horse) -> @owned Builtin.NativeObject
// CHECK-LABEL: sil @_T019versioned_attribute5HorseCfD : $@convention(method) (@owned Horse) -> ()

// CHEKC-LABEL: sil @_T019versioned_attribute9GiftHorseCfd : $@convention(method) (@guaranteed GiftHorse) -> @owned Builtin.NativeObject
// CHECK-LABEL: sil @_T019versioned_attribute9GiftHorseCfD : $@convention(method) (@owned GiftHorse) -> ()
