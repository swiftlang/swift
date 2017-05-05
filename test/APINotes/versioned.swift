// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swift-ide-test  -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -print-regular-comments -swift-version 4 | %FileCheck -check-prefix=CHECK-SWIFT-4 %s

// RUN: %target-swift-ide-test  -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -print-regular-comments -swift-version 3 | %FileCheck -check-prefix=CHECK-SWIFT-3 %s

// CHECK-SWIFT-4: func jumpTo(x: Double, y: Double, z: Double)
// CHECK-SWIFT-3: func jumpTo(x: Double, y: Double, z: Double)

// CHECK-SWIFT-4: func accept(_ ptr: UnsafeMutablePointer<Double>)
// CHECK-SWIFT-3: func acceptPointer(_ ptr: UnsafeMutablePointer<Double>?)

// RUN: not %target-swift-frontend -typecheck -F %S/Inputs/custom-frameworks -swift-version 4 %s 2>&1 | %FileCheck -check-prefix=CHECK-DIAGS -check-prefix=CHECK-DIAGS-4 %s
// RUN: not %target-swift-frontend -typecheck -F %S/Inputs/custom-frameworks -swift-version 3 %s 2>&1 | %FileCheck -check-prefix=CHECK-DIAGS -check-prefix=CHECK-DIAGS-3 %s

// RUN: %target-swift-frontend -emit-silgen -F %S/Inputs/custom-frameworks -swift-version 3 %s -DSILGEN 2>&1 | %FileCheck -check-prefix=CHECK-SILGEN -check-prefix=CHECK-SILGEN-3 %s
// RUN: %target-swift-frontend -emit-silgen -F %S/Inputs/custom-frameworks -swift-version 4 %s -DSILGEN 2>&1 | %FileCheck -check-prefix=CHECK-SILGEN -check-prefix=CHECK-SILGEN-4 %s

import APINotesFrameworkTest

#if !SILGEN
func testRenamedTopLevelDiags() {
  var value = 0.0

  // CHECK-DIAGS-4-NOT: versioned.swift:[[@LINE+1]]:
  accept(&value)
  // CHECK-DIAGS-3: versioned.swift:[[@LINE-1]]:3: error: 'accept' has been renamed to 'acceptPointer(_:)'
  // CHECK-DIAGS-3: note: 'accept' was introduced in Swift 4

  // CHECK-DIAGS-3-NOT: versioned.swift:[[@LINE+1]]:
  acceptPointer(&value)
  // CHECK-DIAGS-4: versioned.swift:[[@LINE-1]]:3: error: 'acceptPointer' has been renamed to 'accept(_:)'
  // CHECK-DIAGS-4: note: 'acceptPointer' was obsoleted in Swift 4

  acceptDoublePointer(&value)
  // CHECK-DIAGS: versioned.swift:[[@LINE-1]]:3: error: 'acceptDoublePointer' has been renamed to
  // CHECK-DIAGS-4-SAME: 'accept(_:)'
  // CHECK-DIAGS-3-SAME: 'acceptPointer(_:)'
  // CHECK-DIAGS: note: 'acceptDoublePointer' was obsoleted in Swift 3

  oldAcceptDoublePointer(&value)
  // CHECK-DIAGS: versioned.swift:[[@LINE-1]]:3: error: 'oldAcceptDoublePointer' has been renamed to
  // CHECK-DIAGS-4-SAME: 'accept(_:)'
  // CHECK-DIAGS-3-SAME: 'acceptPointer(_:)'
  // CHECK-DIAGS: note: 'oldAcceptDoublePointer' has been explicitly marked unavailable here

  _ = SomeCStruct()
  // CHECK-DIAGS: versioned.swift:[[@LINE-1]]:7: error: 'SomeCStruct' has been renamed to
  // CHECK-DIAGS-4-SAME: 'VeryImportantCStruct'
  // CHECK-DIAGS-3-SAME: 'ImportantCStruct'
  // CHECK-DIAGS: note: 'SomeCStruct' was obsoleted in Swift 3

  // CHECK-DIAGS-3-NOT: versioned.swift:[[@LINE+1]]:
  _ = ImportantCStruct()
  // CHECK-DIAGS-4: versioned.swift:[[@LINE-1]]:7: error: 'ImportantCStruct' has been renamed to 'VeryImportantCStruct'
  // CHECK-DIAGS-4: note: 'ImportantCStruct' was obsoleted in Swift 4

  // CHECK-DIAGS-4-NOT: versioned.swift:[[@LINE+1]]:
  _ = VeryImportantCStruct()
  // CHECK-DIAGS-3-NOTE: versioned.swift:[[@LINE-1]]:

  // CHECK-DIAGS-3-NOT: versioned.swift:[[@LINE+1]]:
  _ = InnerInSwift4()
  // CHECK-DIAGS-4: versioned.swift:[[@LINE-1]]:7: error: 'InnerInSwift4' has been renamed to 'Outer.Inner'
  // CHECK-DIAGS-4: note: 'InnerInSwift4' was obsoleted in Swift 4

  // CHECK-DIAGS-4-NOT: versioned.swift:[[@LINE+1]]:
  _ = Outer.Inner()
  // CHECK-DIAGS-3-NOT: versioned.swift:[[@LINE-1]]:
}

func testAKA(structValue: ImportantCStruct, aliasValue: ImportantCAlias) {
  let _: Int = structValue
  // CHECK-DIAGS-3: versioned.swift:[[@LINE-1]]:16: error: cannot convert value of type 'ImportantCStruct' to specified type 'Int'

  let _: Int = aliasValue
  // CHECK-DIAGS-3: versioned.swift:[[@LINE-1]]:16: error: cannot convert value of type 'ImportantCAlias' (aka 'Int32') to specified type 'Int'

  let optStructValue: Optional = structValue
  let _: Int = optStructValue
  // CHECK-DIAGS-3: versioned.swift:[[@LINE-1]]:16: error: cannot convert value of type 'Optional<ImportantCStruct>' to specified type 'Int'

  let optAliasValue: Optional = aliasValue
  let _: Int = optAliasValue
  // CHECK-DIAGS-3: versioned.swift:[[@LINE-1]]:16: error: cannot convert value of type 'Optional<ImportantCAlias>' (aka 'Optional<Int32>') to specified type 'Int'
}

#endif

#if !swift(>=4)

func useSwift3Name(_: ImportantCStruct) {}
// CHECK-SILGEN-3: sil hidden @_T09versioned13useSwift3NameySC20VeryImportantCStructVF

func useNewlyNested(_: InnerInSwift4) {}
// CHECK-SILGEN-3: sil hidden @_T09versioned14useNewlyNestedySC5OuterV5InnerVF
#endif

func useSwift4Name(_: VeryImportantCStruct) {}
// CHECK-SILGEN: sil hidden @_T09versioned13useSwift4NameySC20VeryImportantCStructVF



#if swift(>=4)
func testSwiftWrapperInSwift4() {
  _ = EnclosingStruct.Identifier.member
  let _: EnclosingStruct.Identifier = .member
}

#else
func testSwiftWrapperInSwift3() {
  _ = EnclosingStruct.Identifier.member
  let _: EnclosingStruct.Identifier = .member
}
#endif
