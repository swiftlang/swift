// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -target %target-stable-abi-triple -module-name main -I %S/Inputs %s
// RUN: %target-swift-frontend -emit-module -target %target-stable-abi-triple -module-name main -o %t -I %S/Inputs %s
// RUN: llvm-bcanalyzer %t/main.swiftmodule > %t/main.swiftmodule.txt
// RUN: %target-swift-ide-test -print-module -target %target-stable-abi-triple -module-to-print=main -I %t -source-filename=%s >%t/main.txt
// RUN: %FileCheck %s --input-file=%t/main.txt

// REQUIRES: objc_interop

import Foundation
import objc_implementation

// rdar://134730183 - ensure that errors reduced to warnings by early adopter
// syntax don't invalidate the @implementation attribute (and cause it to not
// be serialized)

// CHECK-LABEL: @_objcImplementation extension ObjCImpl
@_objcImplementation extension ObjCImpl {
  // CHECK-DAG: func cannotBeObjCMethod(_ value: Int?)
  private func cannotBeObjCMethod(_ value: Int?) {}
  // expected-warning@-1 {{method cannot be in an @objc @implementation extension of a class (without final or @nonobjc) because the type of the parameter cannot be represented in Objective-C}}

  // CHECK-DAG: @objc func goodMethod()
  @objc public func goodMethod() {}
}
