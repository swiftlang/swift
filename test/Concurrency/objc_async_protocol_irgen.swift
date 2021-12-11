// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk)  -disable-availability-checking -import-objc-header %S/Inputs/Delegate.h %s -emit-ir -o - | %FileCheck %s -DALIGNMENT=%target-alignment
// REQUIRES: concurrency
// REQUIRES: objc_interop


let anyObject: AnyObject = (MyAsyncProtocol.self as AnyObject) // or something like this

// rdar://76192003
// Make sure we don't emit 2 copies of methods, due to a completion-handler
// version and another due to an async based version.

// CHECK-LABEL: @_PROTOCOL_INSTANCE_METHODS_MyAsyncProtocol = internal constant
// CHECK-SAME: selector_data(myAsyncMethod:)
// CHECK-NOT: selector_data(myAsyncMethod:)
// CHECK-SAME: align [[ALIGNMENT]]
