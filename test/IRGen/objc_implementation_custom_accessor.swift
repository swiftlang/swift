// Custom getter/setter selector names from imported ObjC
// properties must be honored by `@_objcImplementation` extensions, both at
// match time (no `@objc(<name>)` required) and in the emitted ObjC method
// metadata (so `[obj isEnabled]` from ObjC actually dispatches at runtime).

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// RUN: %target-swift-frontend %s -import-objc-header %S/Inputs/objc_implementation_custom_accessor.h -emit-ir -target %target-stable-abi-triple > %t.ir
// RUN: %FileCheck --input-file %t.ir %s
// RUN: %FileCheck --input-file %t.ir --check-prefix NEGATIVE %s

import Foundation

@_objcImplementation extension CustomAccessorClass {
  @objc override init() {
    self.isEnabledStorage = false
    self.bazStorage = ""
    self.hasFooStorage = false
    self.count = 0
    super.init()
  }

  // BOOL property with custom getter `isEnabled`. The Swift name imported for
  // this property is `isEnabled` (BOOL prefix omission), so the implementation
  // uses that name with no `@objc(enabled)` annotation.
  final var isEnabledStorage: Bool
  var isEnabled: Bool {
    get { isEnabledStorage }
    set { isEnabledStorage = newValue }
  }

  // Non-BOOL property with both a custom getter `fooName` and a custom setter
  // `setBar:`.
  final var bazStorage: String
  var baz: String {
    get { bazStorage }
    set { bazStorage = newValue }
  }

  // Readonly property with custom getter `hasFoo`.
  final var hasFooStorage: Bool
  var hasFoo: Bool { hasFooStorage }

  // Plain property with no custom selectors. Confirms the fix doesn't
  // change behaviour in the common case.
  var count: Int
}

// Category implementation: the imported category interface is exposed as a
// Swift `ExtensionDecl`, not a `NominalTypeDecl`, so a naive lookup pinned to
// `NominalTypeDecl` would miss its properties (rdar://111474022 second-pass
// review). The fix walks members through `IterableDeclContext` so categories
// are covered.
@_objcImplementation(CustomCategory) extension CustomAccessorClass {
  var isFlagSet: Bool {
    get { false }
    set { _ = newValue }
  }
}

// Second class: stored-property candidate (synthesized accessors), and the
// legacy `@objc(<property-name>)` workaround that early adopters had to write.
// Both must still produce the header-declared runtime selectors.
@_objcImplementation extension CustomAccessorClass2 {
  @objc override init() { super.init() }

  @objc(ready) var isReady: Bool = false
}

// CustomAccessorClass: the selector method-name globals must use the *custom*
// selectors declared in the header, not the property/Swift names; and the
// instance-methods list must reference those custom selectors.
//
// CHECK-DAG: private global [10 x i8] c"isEnabled\00", section "__TEXT,__objc_methname,cstring_literals"
// CHECK-DAG: private global [12 x i8] c"setEnabled:\00", section "__TEXT,__objc_methname,cstring_literals"
// CHECK-DAG: private global [8 x i8] c"fooName\00", section "__TEXT,__objc_methname,cstring_literals"
// CHECK-DAG: private global [8 x i8] c"setBar:\00", section "__TEXT,__objc_methname,cstring_literals"
// CHECK-DAG: private global [7 x i8] c"hasFoo\00", section "__TEXT,__objc_methname,cstring_literals"
// CHECK-DAG: private global [6 x i8] c"count\00", section "__TEXT,__objc_methname,cstring_literals"
// CHECK-DAG: private global [10 x i8] c"setCount:\00", section "__TEXT,__objc_methname,cstring_literals"

// CHECK-LABEL: @_INSTANCE_METHODS_CustomAccessorClass = internal constant
// CHECK-SAME: \01L_selector_data(isEnabled)
// CHECK-SAME: \01L_selector_data(setEnabled:)
// CHECK-SAME: \01L_selector_data(fooName)
// CHECK-SAME: \01L_selector_data(setBar:)
// CHECK-SAME: \01L_selector_data(hasFoo)
// CHECK-SAME: \01L_selector_data(count)
// CHECK-SAME: \01L_selector_data(setCount:)

// Category `CustomCategory` on CustomAccessorClass: header-declared selectors.
// CHECK-DAG: private global [10 x i8] c"isFlagSet\00", section "__TEXT,__objc_methname,cstring_literals"
// CHECK-DAG: private global [12 x i8] c"setFlagSet:\00", section "__TEXT,__objc_methname,cstring_literals"

// CHECK-LABEL: @"_CATEGORY_INSTANCE_METHODS_CustomAccessorClass_$_CustomCategory"
// CHECK-SAME: \01L_selector_data(isFlagSet)
// CHECK-SAME: \01L_selector_data(setFlagSet:)

// CustomAccessorClass2: stored property + `@objc(ready)` legacy workaround.
// CHECK-DAG: private global [8 x i8] c"isReady\00", section "__TEXT,__objc_methname,cstring_literals"
// CHECK-DAG: private global [10 x i8] c"setReady:\00", section "__TEXT,__objc_methname,cstring_literals"

// CHECK-LABEL: @_INSTANCE_METHODS_CustomAccessorClass2 = internal constant
// CHECK-SAME: \01L_selector_data(isReady)
// CHECK-SAME: \01L_selector_data(setReady:)

// Negative: we must *never* register the default selectors derived from the
// Swift name. Those are the broken selectors that at runtime in
// the original radar. The readonly `hasFoo` must not have a setter selector.
// NEGATIVE-NOT: \01L_selector_data(setIsEnabled:)
// NEGATIVE-NOT: \01L_selector_data(setFooName:)
// NEGATIVE-NOT: \01L_selector_data(setBaz:)
// NEGATIVE-NOT: \01L_selector_data(setHasFoo:)
// NEGATIVE-NOT: \01L_selector_data(setFoo:)
// NEGATIVE-NOT: \01L_selector_data(setIsReady:)
// NEGATIVE-NOT: \01L_selector_data(setIsFlagSet:)
// NEGATIVE-NOT: c"setIsEnabled:\00"
// NEGATIVE-NOT: c"setFooName:\00"
// NEGATIVE-NOT: c"setBaz:\00"
// NEGATIVE-NOT: c"setHasFoo:\00"
// NEGATIVE-NOT: c"setFoo:\00"
// NEGATIVE-NOT: c"setIsReady:\00"
// NEGATIVE-NOT: c"setIsFlagSet:\00"
