// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-as-library -enable-experimental-feature BackDeployFrozenValueTypes -enable-library-evolution -module-name MyKit %S/Inputs/back_deployed_type_deinit_module.swift -o %t/MyKit.swiftmodule
// RUN: %target-swift-frontend -emit-silgen -parse-as-library -enable-experimental-feature BackDeployFrozenValueTypes -enable-library-evolution -module-name MyKit %S/Inputs/back_deployed_type_deinit_module.swift | %FileCheck %s --check-prefix=DEFINING
// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name Client %s -I %t | %FileCheck %s --check-prefix=CLIENT

// REQUIRES: swift_feature_BackDeployFrozenValueTypes
// REQUIRES: OS=macosx
// REQUIRES: CPU=arm64 || CPU=x86_64

// The deinit of a back-deployed noncopyable type has no back deployment
// thunk, because nothing calls it by name, but the defining module serializes
// a fallback copy that its deinit table refers to.

// DEFINING: sil non_abi [serialized] [ossa] @$s5MyKit6HandleVfDTwB : $@convention(method) (@owned Handle) -> ()
// DEFINING: sil_moveonlydeinit [serialized] Handle {
// DEFINING-NEXT: @$s5MyKit6HandleVfDTwB

// The client emits the type's value witnesses, which destroy a value by
// calling the client's own copy of the deinit rather than recursing through
// the value witness table.
import MyKit

// CLIENT-LABEL: define {{.*}} @"$s6Client3useyyF"
// CLIENT: call swiftcc void @"$s5MyKit6HandleVfDTwB"
// CLIENT: define linkonce_odr hidden swiftcc void @"$s5MyKit6HandleVfDTwB"
// CLIENT: call swiftcc void @"$s5MyKit5closeyySiF"
@available(macOS 10.15, *)
func use() {
  let h = Handle(fd: 1)
  _ = h.fd
}
