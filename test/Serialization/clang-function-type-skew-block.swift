// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/Library.swift \
// RUN:   -module-name Library -o %t/Library.swiftmodule
// RUN: %target-swift-frontend -emit-sil -O %t/Client.swift -I %t \
// RUN:   -use-clang-function-types -sil-verify-all | %FileCheck %s
// REQUIRES: objc_interop

// Disable temporarily for rebranch
// REQUIRES: rdar188269750

//--- Library.swift
@inlinable
public func invoke(_ block: @convention(block) (CInt) -> CInt, _ value: CInt) -> CInt {
  return block(value)
}

@inlinable
public func invokeVoid(_ block: @convention(block) () -> Void) {
  block()
}

//--- Client.swift
import Library

// CHECK-LABEL: sil {{.*}}@{{.*}}4call{{.*}} :
// CHECK: apply {{.*}} : $@convention(block) @noescape (Int32) -> Int32
public func call(_ block: @convention(block) (CInt) -> CInt, _ value: CInt) -> CInt {
  return invoke(block, value)
}

// CHECK-LABEL: sil {{.*}}@{{.*}}8callVoid{{.*}} :
// CHECK: apply {{.*}} : $@convention(block) @noescape () -> ()
public func callVoid(_ block: @convention(block) () -> Void) {
  invokeVoid(block)
}
