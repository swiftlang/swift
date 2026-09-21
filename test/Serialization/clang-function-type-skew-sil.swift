// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/Library.swift \
// RUN:   -module-name Library -o %t/Library.swiftmodule
// RUN: %target-swift-frontend -emit-sil -O %t/Client.swift -I %t \
// RUN:   -use-clang-function-types -sil-verify-all | %FileCheck %s

// A client can request Clang function types when a dependency was built
// without them. Reconstruct a dependency's derivable C function types while
// deserializing its serialized SIL body.

//--- Library.swift
public typealias Callback = @convention(c) (CInt) -> CInt

@inlinable
public func invoke(_ function: Callback, _ value: CInt) -> CInt {
  return function(value)
}

@inlinable
public func invokeVoid(_ function: @convention(c) () -> Void) {
  function()
}

//--- Client.swift
import Library

// CHECK-LABEL: sil {{.*}}@{{.*}}4call{{.*}} :
// CHECK: apply {{.*}} : $@convention(c) (Int32) -> Int32
public func call(_ function: Callback, _ value: CInt) -> CInt {
  return invoke(function, value)
}

// CHECK-LABEL: sil {{.*}}@{{.*}}8callVoid{{.*}} :
// CHECK: apply {{.*}} : $@convention(c) () -> ()
public func callVoid(_ function: @convention(c) () -> Void) {
  invokeVoid(function)
}
