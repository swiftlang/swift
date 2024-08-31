// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name main -o %t -I %S/Inputs %s
// RUN: llvm-bcanalyzer %t/main.swiftmodule > %t/main.swiftmodule.txt
// RUN: %target-swift-ide-test -print-module -module-to-print=main -I %t -source-filename=%s >%t/main.txt
// RUN: %FileCheck %s --check-prefixes POSITIVE,CHECK --input-file=%t/main.txt
// RUN: %FileCheck %s --check-prefixes NEGATIVE,CHECK --input-file=%t/main.txt

// REQUIRES: objc_interop

import Foundation
import objc_implementation

// CHECK-LABEL: @objc @implementation extension ObjCImpl
@objc @implementation extension ObjCImpl {
  // These should not appear in the serialized output.

  // NEGATIVE-NOT: init(nonSerialized value
  @objc(initWithNonSerialized:)
  public init(nonSerialized value: Int32) {
    super.init()
  }

  // NEGATIVE-NOT: func nonSerializedMethod
  @objc public func nonSerializedMethod() {}

  // NEGATIVE-NOT: var nonSerializedProperty
  @objc public var nonSerializedProperty: Int32 = 0

  // NEGATIVE-NOT: deinit
  deinit {}

  // These should appear in the serialized output.

  // POSITIVE: func serializedMethod
  final public func serializedMethod() {}
  // POSITIVE: var serializedProperty
  final public var serializedProperty: Int32 = 0

  // POSITIVE: init(serialized value
  @nonobjc public convenience init(serialized value: Int32) {
    self.init(nonSerialized: value)
  }
}

// CHECK-LABEL: @objc(CategoryMembers) @implementation extension ObjCImpl
@objc(CategoryMembers) @implementation extension ObjCImpl {
  // These should not appear in the serialized output.

  // NEGATIVE-NOT: func nonSerializedCategoryMethod
  @objc public func nonSerializedCategoryMethod() {}

  // NEGATIVE-NOT: var nonSerializedCategoryProperty
  @objc public var nonSerializedCategoryProperty: Int32 {
    get { nonSerializedProperty }
    set { nonSerializedProperty = newValue }
  }

  // These should appear in the serialized output.

  // POSITIVE: func serializedCategoryMethod()
  final public func serializedCategoryMethod() {}

  // POSITIVE: var serializedCategoryProperty
  final public var serializedCategoryProperty: Int32 {
    get { serializedProperty }
    set { serializedProperty = newValue }
  }
}
