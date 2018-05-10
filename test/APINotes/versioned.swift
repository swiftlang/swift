// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test  -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -print-regular-comments -swift-version 4 | %FileCheck -check-prefix=CHECK-SWIFT-4 %s

// RUN: %target-swift-ide-test  -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -print-regular-comments -swift-version 3 | %FileCheck -check-prefix=CHECK-SWIFT-3 %s

// CHECK-SWIFT-4: func jumpTo(x: Double, y: Double, z: Double)
// CHECK-SWIFT-3: func jumpTo(x: Double, y: Double, z: Double)

// CHECK-SWIFT-4: func accept(_ ptr: UnsafeMutablePointer<Double>)
// CHECK-SWIFT-3: func acceptPointer(_ ptr: UnsafeMutablePointer<Double>?)

// CHECK-SWIFT-4: func normallyUnchanged()
// CHECK-SWIFT-4: @available(swift, obsoleted: 4, renamed: "normallyUnchanged()")
// CHECK-SWIFT-4-NEXT: func normallyUnchangedButChangedInSwift3()
// CHECK-SWIFT-3: @available(swift, obsoleted: 3, renamed: "normallyUnchangedButChangedInSwift3()")
// CHECK-SWIFT-3-NEXT: func normallyUnchanged()
// CHECK-SWIFT-3: func normallyUnchangedButChangedInSwift3()


// CHECK-SWIFT-4: func normallyChanged()
// CHECK-SWIFT-4-NEXT: @available(swift, obsoleted: 4, renamed: "normallyChanged()")
// CHECK-SWIFT-4-NEXT: func normallyChangedButSpecialInSwift3()
// CHECK-SWIFT-4-NEXT: @available(swift, obsoleted: 3, renamed: "normallyChanged()")
// CHECK-SWIFT-4-NEXT: func normallyChangedOriginal()
// CHECK-SWIFT-3: @available(swift, introduced: 4, renamed: "normallyChangedButSpecialInSwift3()")
// CHECK-SWIFT-3-NEXT: func normallyChanged()
// CHECK-SWIFT-3-NEXT: func normallyChangedButSpecialInSwift3()
// CHECK-SWIFT-3-NEXT: @available(swift, obsoleted: 3, renamed: "normallyChangedButSpecialInSwift3()")
// CHECK-SWIFT-3-NEXT: func normallyChangedOriginal()

// CHECK-SWIFT-4: @available(swift, obsoleted: 4, renamed: "NormallyUnchangedWrapper")
// CHECK-SWIFT-4-NEXT: typealias NormallyUnchangedButChangedInSwift3Wrapper = NormallyUnchangedWrapper
// CHECK-SWIFT-4: struct NormallyUnchangedWrapper {
// CHECK-SWIFT-3: typealias NormallyUnchangedButChangedInSwift3Wrapper = NormallyUnchangedWrapper
// CHECK-SWIFT-3-NEXT: struct NormallyUnchangedWrapper {

// CHECK-SWIFT-4: @available(swift, obsoleted: 4, renamed: "NormallyChangedWrapper")
// CHECK-SWIFT-4-NEXT: typealias NormallyChangedButSpecialInSwift3Wrapper = NormallyChangedWrapper
// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "NormallyChangedWrapper")
// CHECK-SWIFT-4-NEXT: typealias NormallyChangedOriginalWrapper = NormallyChangedWrapper
// CHECK-SWIFT-4: struct NormallyChangedWrapper {
// CHECK-SWIFT-3: typealias NormallyChangedButSpecialInSwift3Wrapper = NormallyChangedWrapper
// CHECK-SWIFT-3-NEXT: @available(swift, obsoleted: 3, renamed: "NormallyChangedButSpecialInSwift3Wrapper")
// CHECK-SWIFT-3-NEXT: typealias NormallyChangedOriginalWrapper = NormallyChangedButSpecialInSwift3Wrapper
// CHECK-SWIFT-3-NEXT: struct NormallyChangedWrapper {


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
  let s = InnerInSwift4()
  // CHECK-DIAGS-4: versioned.swift:[[@LINE-1]]:11: error: 'InnerInSwift4' has been renamed to 'Outer.Inner'
  // CHECK-DIAGS-4: note: 'InnerInSwift4' was obsoleted in Swift 4
  _ = s.value
  // CHECK-DIAGS-4-NOT: versioned.swift:[[@LINE-1]]:

  // CHECK-DIAGS-4-NOT: versioned.swift:[[@LINE+1]]:
  let t = Outer.Inner()
  // CHECK-DIAGS-3-NOT: versioned.swift:[[@LINE-1]]:
  _ = s.value
  // CHECK-DIAGS-3-NOT: versioned.swift:[[@LINE-1]]:
}

func testAKA(structValue: ImportantCStruct, aliasValue: ImportantCAlias) {
  let _: Int = structValue
  // CHECK-DIAGS-3: versioned.swift:[[@LINE-1]]:16: error: cannot convert value of type 'ImportantCStruct' to specified type 'Int'

  let _: Int = aliasValue
  // CHECK-DIAGS-3: versioned.swift:[[@LINE-1]]:16: error: cannot convert value of type 'ImportantCAlias' (aka 'Int32') to specified type 'Int'

  let optStructValue: Optional = structValue
  let _: Int = optStructValue
  // CHECK-DIAGS-3: versioned.swift:[[@LINE-1]]:16: error: cannot convert value of type 'ImportantCStruct?' to specified type 'Int'

  let optAliasValue: Optional = aliasValue
  let _: Int = optAliasValue
  // CHECK-DIAGS-3: versioned.swift:[[@LINE-1]]:16: error: cannot convert value of type 'ImportantCAlias?' (aka 'Optional<Int32>') to specified type 'Int'
}

func testRenamedEnumConstants() {
  _ = AnonymousEnumValue // okay

  // CHECK-DIAGS-4: [[@LINE+1]]:7: error: 'AnonymousEnumRenamed' has been renamed to 'AnonymousEnumRenamedSwiftUnversioned'
  _ = AnonymousEnumRenamed
  // CHECK-DIAGS-3: [[@LINE-1]]:7: error: 'AnonymousEnumRenamed' has been renamed to 'AnonymousEnumRenamedSwift3'

  // CHECK-DIAGS-4-NOT: :[[@LINE+1]]:7:
  _ = AnonymousEnumRenamedSwiftUnversioned
  // CHECK-DIAGS-3: [[@LINE-1]]:7: error: 'AnonymousEnumRenamedSwiftUnversioned' has been renamed to 'AnonymousEnumRenamedSwift3'

  // CHECK-DIAGS-4: [[@LINE+1]]:7: error: 'AnonymousEnumRenamedSwift3' has been renamed to 'AnonymousEnumRenamedSwiftUnversioned'
  _ = AnonymousEnumRenamedSwift3
  // CHECK-DIAGS-3-NOT: :[[@LINE-1]]:7:
}

func testRenamedUnknownEnum() {
  _ = UnknownEnumValue // okay

  // CHECK-DIAGS-4: [[@LINE+1]]:7: error: 'UnknownEnumRenamed' has been renamed to 'UnknownEnumRenamedSwiftUnversioned'
  _ = UnknownEnumRenamed
  // CHECK-DIAGS-3: [[@LINE-1]]:7: error: 'UnknownEnumRenamed' has been renamed to 'UnknownEnumRenamedSwift3'

  // CHECK-DIAGS-4-NOT: :[[@LINE+1]]:7:
  _ = UnknownEnumRenamedSwiftUnversioned
  // CHECK-DIAGS-3: [[@LINE-1]]:7: error: 'UnknownEnumRenamedSwiftUnversioned' has been renamed to 'UnknownEnumRenamedSwift3'

  // CHECK-DIAGS-4: [[@LINE+1]]:7: error: 'UnknownEnumRenamedSwift3' has been renamed to 'UnknownEnumRenamedSwiftUnversioned'
  _ = UnknownEnumRenamedSwift3
  // CHECK-DIAGS-3-NOT: :[[@LINE-1]]:7:
}

func testRenamedTrueEnum() {
  // CHECK-DIAGS: [[@LINE+1]]:7: error: use of unresolved identifier 'TrueEnumValue'
  _ = TrueEnumValue

  // CHECK-DIAGS: [[@LINE+1]]:7: error: type 'TrueEnum' has no member 'TrueEnumValue'
  _ = TrueEnum.TrueEnumValue

  // CHECK-DIAGS: [[@LINE+1]]:16: error: 'Value' has been renamed to 'value'
  _ = TrueEnum.Value

  _ = TrueEnum.value // okay

  // CHECK-DIAGS: [[@LINE+1]]:7: error: use of unresolved identifier 'TrueEnumRenamed'
  _ = TrueEnumRenamed

  // CHECK-DIAGS: [[@LINE+1]]:7: error: type 'TrueEnum' has no member 'TrueEnumRenamed'
  _ = TrueEnum.TrueEnumRenamed

  // CHECK-DIAGS-4: [[@LINE+1]]:16: error: 'Renamed' has been renamed to 'renamedSwiftUnversioned'
  _ = TrueEnum.Renamed
  // CHECK-DIAGS-3: [[@LINE-1]]:16: error: 'Renamed' has been renamed to 'renamedSwift3'

  // CHECK-DIAGS: [[@LINE+1]]:7: error: type 'TrueEnum' has no member 'renamed'
  _ = TrueEnum.renamed

  // CHECK-DIAGS-4-NOT: :[[@LINE+1]]:16:
  _ = TrueEnum.renamedSwiftUnversioned
  // CHECK-DIAGS-3: [[@LINE-1]]:16: error: 'renamedSwiftUnversioned' has been renamed to 'renamedSwift3'

  // CHECK-DIAGS-4: [[@LINE+1]]:16: error: 'renamedSwift3' has been renamed to 'renamedSwiftUnversioned'
  _ = TrueEnum.renamedSwift3
  // CHECK-DIAGS-3-NOT: :[[@LINE-1]]:16:

  // CHECK-DIAGS: [[@LINE+1]]:7: error: use of unresolved identifier 'TrueEnumAliasRenamed'
  _ = TrueEnumAliasRenamed

  // CHECK-DIAGS: [[@LINE+1]]:7: error: type 'TrueEnum' has no member 'TrueEnumAliasRenamed'
  _ = TrueEnum.TrueEnumAliasRenamed

  // CHECK-DIAGS-4: [[@LINE+1]]:16: error: 'AliasRenamed' has been renamed to 'aliasRenamedSwiftUnversioned'
  _ = TrueEnum.AliasRenamed
  // CHECK-DIAGS-3: [[@LINE-1]]:16: error: 'AliasRenamed' has been renamed to 'aliasRenamedSwift3'

  // CHECK-DIAGS: [[@LINE+1]]:7: error: type 'TrueEnum' has no member 'aliasRenamed'
  _ = TrueEnum.aliasRenamed

  // CHECK-DIAGS-4-NOT: :[[@LINE+1]]:16:
  _ = TrueEnum.aliasRenamedSwiftUnversioned
  // CHECK-DIAGS-3: [[@LINE-1]]:16: error: 'aliasRenamedSwiftUnversioned' has been renamed to 'aliasRenamedSwift3'

  // CHECK-DIAGS-4: [[@LINE+1]]:16: error: 'aliasRenamedSwift3' has been renamed to 'aliasRenamedSwiftUnversioned'
  _ = TrueEnum.aliasRenamedSwift3
  // CHECK-DIAGS-3-NOT: :[[@LINE-1]]:16:
}

func testRenamedOptionyEnum() {
  // CHECK-DIAGS: [[@LINE+1]]:7: error: use of unresolved identifier 'OptionyEnumValue'
  _ = OptionyEnumValue

  // CHECK-DIAGS: [[@LINE+1]]:7: error: type 'OptionyEnum' has no member 'OptionyEnumValue'
  _ = OptionyEnum.OptionyEnumValue

  // CHECK-DIAGS: [[@LINE+1]]:19: error: 'Value' has been renamed to 'value'
  _ = OptionyEnum.Value

  _ = OptionyEnum.value // okay

  // CHECK-DIAGS: [[@LINE+1]]:7: error: use of unresolved identifier 'OptionyEnumRenamed'
  _ = OptionyEnumRenamed

  // CHECK-DIAGS: [[@LINE+1]]:7: error: type 'OptionyEnum' has no member 'OptionyEnumRenamed'
  _ = OptionyEnum.OptionyEnumRenamed

  // CHECK-DIAGS-4: [[@LINE+1]]:19: error: 'Renamed' has been renamed to 'renamedSwiftUnversioned'
  _ = OptionyEnum.Renamed
  // CHECK-DIAGS-3: [[@LINE-1]]:19: error: 'Renamed' has been renamed to 'renamedSwift3'

  // CHECK-DIAGS: [[@LINE+1]]:7: error: type 'OptionyEnum' has no member 'renamed'
  _ = OptionyEnum.renamed

  // CHECK-DIAGS-4-NOT: :[[@LINE+1]]:19:
  _ = OptionyEnum.renamedSwiftUnversioned
  // CHECK-DIAGS-3: [[@LINE-1]]:19: error: 'renamedSwiftUnversioned' has been renamed to 'renamedSwift3'

  // CHECK-DIAGS-4: [[@LINE+1]]:19: error: 'renamedSwift3' has been renamed to 'renamedSwiftUnversioned'
  _ = OptionyEnum.renamedSwift3
  // CHECK-DIAGS-3-NOT: :[[@LINE-1]]:19:
}

#endif

#if !swift(>=4)

func useSwift3Name(_: ImportantCStruct) {}
// CHECK-SILGEN-3: sil hidden @$S9versioned13useSwift3NameyySo11SomeCStructVF

func useNewlyNested(_: InnerInSwift4) {}
// CHECK-SILGEN-3: sil hidden @$S9versioned14useNewlyNestedyySo13InnerInSwift4VF
#endif

func useSwift4Name(_: VeryImportantCStruct) {}
// CHECK-SILGEN: sil hidden @$S9versioned13useSwift4NameyySo11SomeCStructVF



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
