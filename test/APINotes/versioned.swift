// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test  -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -print-regular-comments -swift-version 5 | %FileCheck -check-prefix=CHECK-SWIFT-5 %s

// RUN: %target-swift-ide-test  -F %S/Inputs/custom-frameworks -print-module -source-filename %s -module-to-print=APINotesFrameworkTest -function-definitions=false -print-regular-comments -swift-version 4 | %FileCheck -check-prefix=CHECK-SWIFT-4 %s

// CHECK-SWIFT-5: func jumpTo(x: Double, y: Double, z: Double)
// CHECK-SWIFT-4: func jumpTo(x: Double, y: Double, z: Double)

// CHECK-SWIFT-5: func accept(_ ptr: UnsafeMutablePointer<Double>)
// CHECK-SWIFT-4: func acceptPointer(_ ptr: UnsafeMutablePointer<Double>?)

// CHECK-SWIFT-5: func normallyUnchanged()
// CHECK-SWIFT-5: @available(swift, obsoleted: 4.2, renamed: "normallyUnchanged()")
// CHECK-SWIFT-5-NEXT: func normallyUnchangedButChangedInSwift4()
// CHECK-SWIFT-4: @available(swift, obsoleted: 3, renamed: "normallyUnchangedButChangedInSwift4()")
// CHECK-SWIFT-4-NEXT: func normallyUnchanged()
// CHECK-SWIFT-4: func normallyUnchangedButChangedInSwift4()


// CHECK-SWIFT-5: func normallyChanged()
// CHECK-SWIFT-5-NEXT: @available(swift, obsoleted: 4.2, renamed: "normallyChanged()")
// CHECK-SWIFT-5-NEXT: func normallyChangedButSpecialInSwift4()
// CHECK-SWIFT-5-NEXT: @available(swift, obsoleted: 3, renamed: "normallyChanged()")
// CHECK-SWIFT-5-NEXT: func normallyChangedOriginal()
// CHECK-SWIFT-4: @available(swift, introduced: 4.2, renamed: "normallyChangedButSpecialInSwift4()")
// CHECK-SWIFT-4-NEXT: func normallyChanged()
// CHECK-SWIFT-4-NEXT: func normallyChangedButSpecialInSwift4()
// CHECK-SWIFT-4-NEXT: @available(swift, obsoleted: 3, renamed: "normallyChangedButSpecialInSwift4()")
// CHECK-SWIFT-4-NEXT: func normallyChangedOriginal()

// CHECK-SWIFT-5: @available(swift, obsoleted: 4.2, renamed: "NormallyUnchangedWrapper")
// CHECK-SWIFT-5-NEXT: typealias NormallyUnchangedButChangedInSwift4Wrapper = NormallyUnchangedWrapper
// CHECK-SWIFT-5: struct NormallyUnchangedWrapper {
// CHECK-SWIFT-4: typealias NormallyUnchangedButChangedInSwift4Wrapper = NormallyUnchangedWrapper
// CHECK-SWIFT-4-NEXT: struct NormallyUnchangedWrapper {

// CHECK-SWIFT-5: @available(swift, obsoleted: 4.2, renamed: "NormallyChangedWrapper")
// CHECK-SWIFT-5-NEXT: typealias NormallyChangedButSpecialInSwift4Wrapper = NormallyChangedWrapper
// CHECK-SWIFT-5: @available(swift, obsoleted: 3, renamed: "NormallyChangedWrapper")
// CHECK-SWIFT-5-NEXT: typealias NormallyChangedOriginalWrapper = NormallyChangedWrapper
// CHECK-SWIFT-5: struct NormallyChangedWrapper {
// CHECK-SWIFT-4: typealias NormallyChangedButSpecialInSwift4Wrapper = NormallyChangedWrapper
// CHECK-SWIFT-4-NEXT: @available(swift, obsoleted: 3, renamed: "NormallyChangedButSpecialInSwift4Wrapper")
// CHECK-SWIFT-4-NEXT: typealias NormallyChangedOriginalWrapper = NormallyChangedButSpecialInSwift4Wrapper
// CHECK-SWIFT-4-NEXT: struct NormallyChangedWrapper {


// RUN: not %target-swift-frontend -typecheck -F %S/Inputs/custom-frameworks -swift-version 5 %s 2>&1 | %FileCheck -check-prefix=CHECK-DIAGS -check-prefix=CHECK-DIAGS-5 %s
// RUN: not %target-swift-frontend -typecheck -F %S/Inputs/custom-frameworks -swift-version 4 %s 2>&1 | %FileCheck -check-prefix=CHECK-DIAGS -check-prefix=CHECK-DIAGS-4 %s

// RUN: %target-swift-frontend -emit-silgen -F %S/Inputs/custom-frameworks -swift-version 4 %s -DSILGEN 2>&1 | %FileCheck -check-prefix=CHECK-SILGEN -check-prefix=CHECK-SILGEN-4 %s
// RUN: %target-swift-frontend -emit-silgen -F %S/Inputs/custom-frameworks -swift-version 5 %s -DSILGEN 2>&1 | %FileCheck -check-prefix=CHECK-SILGEN -check-prefix=CHECK-SILGEN-5 %s

import APINotesFrameworkTest

#if !SILGEN
func testRenamedTopLevelDiags() {
  var value = 0.0

  // CHECK-DIAGS-5-NOT: versioned.swift:[[@LINE+1]]:
  accept(&value)
  // CHECK-DIAGS-4: versioned.swift:[[@LINE-1]]:3: error: 'accept' has been renamed to 'acceptPointer(_:)'
  // CHECK-DIAGS-4: note: 'accept' was introduced in Swift 4.2

  // CHECK-DIAGS-4-NOT: versioned.swift:[[@LINE+1]]:
  acceptPointer(&value)
  // CHECK-DIAGS-5: versioned.swift:[[@LINE-1]]:3: error: 'acceptPointer' has been renamed to 'accept(_:)'
  // CHECK-DIAGS-5: note: 'acceptPointer' was obsoleted in Swift 4.2

  acceptDoublePointer(&value)
  // CHECK-DIAGS: versioned.swift:[[@LINE-1]]:3: error: 'acceptDoublePointer' has been renamed to
  // CHECK-DIAGS-5-SAME: 'accept(_:)'
  // CHECK-DIAGS-4-SAME: 'acceptPointer(_:)'
  // CHECK-DIAGS: note: 'acceptDoublePointer' was obsoleted in Swift 3

  oldAcceptDoublePointer(&value)
  // CHECK-DIAGS: versioned.swift:[[@LINE-1]]:3: error: 'oldAcceptDoublePointer' has been renamed to
  // CHECK-DIAGS-5-SAME: 'accept(_:)'
  // CHECK-DIAGS-4-SAME: 'acceptPointer(_:)'
  // CHECK-DIAGS: note: 'oldAcceptDoublePointer' has been explicitly marked unavailable here

  _ = SomeCStruct()
  // CHECK-DIAGS: versioned.swift:[[@LINE-1]]:7: error: 'SomeCStruct' has been renamed to
  // CHECK-DIAGS-5-SAME: 'VeryImportantCStruct'
  // CHECK-DIAGS-4-SAME: 'ImportantCStruct'
  // CHECK-DIAGS: note: 'SomeCStruct' was obsoleted in Swift 3

  // CHECK-DIAGS-4-NOT: versioned.swift:[[@LINE+1]]:
  _ = ImportantCStruct()
  // CHECK-DIAGS-5: versioned.swift:[[@LINE-1]]:7: error: 'ImportantCStruct' has been renamed to 'VeryImportantCStruct'
  // CHECK-DIAGS-5: note: 'ImportantCStruct' was obsoleted in Swift 4.2

  // CHECK-DIAGS-5-NOT: versioned.swift:[[@LINE+1]]:
  _ = VeryImportantCStruct()
  // CHECK-DIAGS-4-NOTE: versioned.swift:[[@LINE-1]]:

  // CHECK-DIAGS-4-NOT: versioned.swift:[[@LINE+1]]:
  let s = InnerInSwift5()
  // CHECK-DIAGS-5: versioned.swift:[[@LINE-1]]:11: error: 'InnerInSwift5' has been renamed to 'Outer.Inner'
  // CHECK-DIAGS-5: note: 'InnerInSwift5' was obsoleted in Swift 4.2
  _ = s.value
  // CHECK-DIAGS-5-NOT: versioned.swift:[[@LINE-1]]:

  // CHECK-DIAGS-5-NOT: versioned.swift:[[@LINE+1]]:
  let t = Outer.Inner()
  // CHECK-DIAGS-4-NOT: versioned.swift:[[@LINE-1]]:
  _ = s.value
  // CHECK-DIAGS-4-NOT: versioned.swift:[[@LINE-1]]:
}

func testAKA(structValue: ImportantCStruct, aliasValue: ImportantCAlias) {
  let _: Int = structValue
  // CHECK-DIAGS-4: versioned.swift:[[@LINE-1]]:16: error: cannot convert value of type 'ImportantCStruct' to specified type 'Int'

  let _: Int = aliasValue
  // CHECK-DIAGS-4: versioned.swift:[[@LINE-1]]:16: error: cannot convert value of type 'ImportantCAlias' (aka 'Int32') to specified type 'Int'

  let optStructValue: Optional = structValue
  let _: Int = optStructValue
  // CHECK-DIAGS-4: versioned.swift:[[@LINE-1]]:16: error: cannot convert value of type 'ImportantCStruct?' to specified type 'Int'

  let optAliasValue: Optional = aliasValue
  let _: Int = optAliasValue
  // CHECK-DIAGS-4: versioned.swift:[[@LINE-1]]:16: error: cannot convert value of type 'ImportantCAlias?' (aka 'Optional<Int32>') to specified type 'Int'
}

func testRenamedEnumConstants() {
  _ = AnonymousEnumValue // okay

  // CHECK-DIAGS-5: [[@LINE+1]]:7: error: 'AnonymousEnumRenamed' has been renamed to 'AnonymousEnumRenamedSwiftUnversioned'
  _ = AnonymousEnumRenamed
  // CHECK-DIAGS-4: [[@LINE-1]]:7: error: 'AnonymousEnumRenamed' has been renamed to 'AnonymousEnumRenamedSwift4'

  // CHECK-DIAGS-5-NOT: :[[@LINE+1]]:7:
  _ = AnonymousEnumRenamedSwiftUnversioned
  // CHECK-DIAGS-4: [[@LINE-1]]:7: error: 'AnonymousEnumRenamedSwiftUnversioned' has been renamed to 'AnonymousEnumRenamedSwift4'

  // CHECK-DIAGS-5: [[@LINE+1]]:7: error: 'AnonymousEnumRenamedSwift4' has been renamed to 'AnonymousEnumRenamedSwiftUnversioned'
  _ = AnonymousEnumRenamedSwift4
  // CHECK-DIAGS-4-NOT: :[[@LINE-1]]:7:
}

func testRenamedUnknownEnum() {
  _ = UnknownEnumValue // okay

  // CHECK-DIAGS-5: [[@LINE+1]]:7: error: 'UnknownEnumRenamed' has been renamed to 'UnknownEnumRenamedSwiftUnversioned'
  _ = UnknownEnumRenamed
  // CHECK-DIAGS-4: [[@LINE-1]]:7: error: 'UnknownEnumRenamed' has been renamed to 'UnknownEnumRenamedSwift4'

  // CHECK-DIAGS-5-NOT: :[[@LINE+1]]:7:
  _ = UnknownEnumRenamedSwiftUnversioned
  // CHECK-DIAGS-4: [[@LINE-1]]:7: error: 'UnknownEnumRenamedSwiftUnversioned' has been renamed to 'UnknownEnumRenamedSwift4'

  // CHECK-DIAGS-5: [[@LINE+1]]:7: error: 'UnknownEnumRenamedSwift4' has been renamed to 'UnknownEnumRenamedSwiftUnversioned'
  _ = UnknownEnumRenamedSwift4
  // CHECK-DIAGS-4-NOT: :[[@LINE-1]]:7:
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

  // CHECK-DIAGS-5: [[@LINE+1]]:16: error: 'Renamed' has been renamed to 'renamedSwiftUnversioned'
  _ = TrueEnum.Renamed
  // CHECK-DIAGS-4: [[@LINE-1]]:16: error: 'Renamed' has been renamed to 'renamedSwift4'

  // CHECK-DIAGS: [[@LINE+1]]:7: error: type 'TrueEnum' has no member 'renamed'
  _ = TrueEnum.renamed

  // CHECK-DIAGS-5-NOT: :[[@LINE+1]]:16:
  _ = TrueEnum.renamedSwiftUnversioned
  // CHECK-DIAGS-4: [[@LINE-1]]:16: error: 'renamedSwiftUnversioned' has been renamed to 'renamedSwift4'

  // CHECK-DIAGS-5: [[@LINE+1]]:16: error: 'renamedSwift4' has been renamed to 'renamedSwiftUnversioned'
  _ = TrueEnum.renamedSwift4
  // CHECK-DIAGS-4-NOT: :[[@LINE-1]]:16:

  // CHECK-DIAGS: [[@LINE+1]]:7: error: use of unresolved identifier 'TrueEnumAliasRenamed'
  _ = TrueEnumAliasRenamed

  // CHECK-DIAGS: [[@LINE+1]]:7: error: type 'TrueEnum' has no member 'TrueEnumAliasRenamed'
  _ = TrueEnum.TrueEnumAliasRenamed

  // CHECK-DIAGS-5: [[@LINE+1]]:16: error: 'AliasRenamed' has been renamed to 'aliasRenamedSwiftUnversioned'
  _ = TrueEnum.AliasRenamed
  // CHECK-DIAGS-4: [[@LINE-1]]:16: error: 'AliasRenamed' has been renamed to 'aliasRenamedSwift4'

  // CHECK-DIAGS: [[@LINE+1]]:7: error: type 'TrueEnum' has no member 'aliasRenamed'
  _ = TrueEnum.aliasRenamed

  // CHECK-DIAGS-5-NOT: :[[@LINE+1]]:16:
  _ = TrueEnum.aliasRenamedSwiftUnversioned
  // CHECK-DIAGS-4: [[@LINE-1]]:16: error: 'aliasRenamedSwiftUnversioned' has been renamed to 'aliasRenamedSwift4'

  // CHECK-DIAGS-5: [[@LINE+1]]:16: error: 'aliasRenamedSwift4' has been renamed to 'aliasRenamedSwiftUnversioned'
  _ = TrueEnum.aliasRenamedSwift4
  // CHECK-DIAGS-4-NOT: :[[@LINE-1]]:16:
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

  // CHECK-DIAGS-5: [[@LINE+1]]:19: error: 'Renamed' has been renamed to 'renamedSwiftUnversioned'
  _ = OptionyEnum.Renamed
  // CHECK-DIAGS-4: [[@LINE-1]]:19: error: 'Renamed' has been renamed to 'renamedSwift4'

  // CHECK-DIAGS: [[@LINE+1]]:7: error: type 'OptionyEnum' has no member 'renamed'
  _ = OptionyEnum.renamed

  // CHECK-DIAGS-5-NOT: :[[@LINE+1]]:19:
  _ = OptionyEnum.renamedSwiftUnversioned
  // CHECK-DIAGS-4: [[@LINE-1]]:19: error: 'renamedSwiftUnversioned' has been renamed to 'renamedSwift4'

  // CHECK-DIAGS-5: [[@LINE+1]]:19: error: 'renamedSwift4' has been renamed to 'renamedSwiftUnversioned'
  _ = OptionyEnum.renamedSwift4
  // CHECK-DIAGS-4-NOT: :[[@LINE-1]]:19:
}

#endif

#if !swift(>=5)

func useSwift4Name(_: ImportantCStruct) {}
// CHECK-SILGEN-4: sil hidden @$S9versioned13useSwift4NameyySo11SomeCStructVF

func useNewlyNested(_: InnerInSwift5) {}
// CHECK-SILGEN-4: sil hidden @$S9versioned14useNewlyNestedyySo13InnerInSwift5VF
#endif

func useSwift5Name(_: VeryImportantCStruct) {}
// CHECK-SILGEN: sil hidden @$S9versioned13useSwift5NameyySo11SomeCStructVF



#if swift(>=5)
func testSwiftWrapperInSwift5() {
  _ = EnclosingStruct.Identifier.member
  let _: EnclosingStruct.Identifier = .member
}

#else
func testSwiftWrapperInSwift4() {
  _ = EnclosingStruct.Identifier.member
  let _: EnclosingStruct.Identifier = .member
}
#endif
