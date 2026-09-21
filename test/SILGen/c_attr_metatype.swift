// RUN: %target-swift-emit-silgen %s -module-name c_attr_metatype | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// AnyClass (any AnyObject.Type) is representable in C as the Objective-C runtime
// type 'Class'. A @c function is emitted directly as its C entry point (no
// separate native thunk), so the foreign metatype is bridged to/from the native
// thick metatype in-place: objc_to_thick_metatype on entry, thick_to_objc_metatype
// on return.

// CHECK-LABEL: sil hidden [asmname "takesClass"] [ossa] @$s{{.*}}10takesClass{{.*}}FTo : $@convention(c) (@objc_metatype any AnyObject.Type) -> () {
// CHECK: bb0(%0 : $@objc_metatype any AnyObject.Type):
// CHECK: objc_to_thick_metatype %0 to $@thick any AnyObject.Type
@c(takesClass)
func takesClass(_ cls: AnyClass) { _ = cls }

// CHECK-LABEL: sil hidden [asmname "returnsClass"] [ossa] @$s{{.*}}12returnsClass{{.*}}FTo : $@convention(c) () -> @objc_metatype any AnyObject.Type {
// CHECK: thick_to_objc_metatype {{%[0-9]+}} to $@objc_metatype any AnyObject.Type
// CHECK: return
@c(returnsClass)
func returnsClass() -> AnyClass { return NSObject.self }
