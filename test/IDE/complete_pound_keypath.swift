// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=AFTER_POUND | %FileCheck -check-prefix=CHECK-AFTER_POUND %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=KEYPATH_ARG | %FileCheck -check-prefix=CHECK-KEYPATH_ARG %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_KEYPATH_1 | %FileCheck -check-prefix=CHECK-IN_KEYPATH %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_KEYPATH_2 | %FileCheck -check-prefix=CHECK-IN_KEYPATH %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_KEYPATH_3 | %FileCheck -check-prefix=CHECK-IN_KEYPATH_BRIDGED_STRING %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_KEYPATH_4 | %FileCheck -check-prefix=CHECK-IN_KEYPATH_BRIDGED_STRING %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_KEYPATH_5 | %FileCheck -check-prefixes=CHECK-IN_KEYPATH,CHECK-IN_KEYPATH_OPT %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_KEYPATH_6 | %FileCheck -check-prefixes=CHECK-IN_KEYPATH,CHECK-IN_KEYPATH_OPT %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_KEYPATH_7 | %FileCheck -check-prefixes=CHECK-IN_KEYPATH_BRIDGED_STRING %s


// REQUIRES: objc_interop

import Foundation

{
  if ##^AFTER_POUND^#
}

func acceptKeyPath(_ keyPath: String) { }

func selectorArg1(obj: NSObject) {
  acceptKeyPath(#^KEYPATH_ARG^#
}

@objcMembers class ObjCClass : NSObject {
  var prop1: String = ""
  var prop2: ObjCClass?
  var prop3: [ObjCClass]? = []
  var prop4: [String: String] = [:]

  func completeInKeyPath1() {
    _ = #keyPath(#^IN_KEYPATH_1^#
  }
}

func completeInKeyPath2() {
  _ = #keyPath(ObjCClass.#^IN_KEYPATH_2^#
}

func completeInKeyPath3() {
   _ = #keyPath(ObjCClass.prop1.#^IN_KEYPATH_3^#
}
func completeInKeyPath3() {
     _ = #keyPath(String.#^IN_KEYPATH_4^#
}

func completeInKeyPath4() {
  _ = #keyPath(ObjCClass.prop2.#^IN_KEYPATH_5^#
}

func completeInKeyPath5() {
  _ = #keyPath(ObjCClass.prop3.#^IN_KEYPATH_6^#
}

func completeInKeyPath6() {
  _ = #keyPath(ObjCClass.prop4.anythingHere.#^IN_KEYPATH_7^#
}

// CHECK-AFTER_POUND-NOT: keyPath

// CHECK-KEYPATH_ARG: Keyword/None/TypeRelation[Convertible]: #keyPath({#@objc property sequence#})[#String#]; name=#keyPath()

// CHECK-IN_KEYPATH: Decl[InstanceVar]/CurrNominal:      prop1[#String#]; name=prop1
// CHECK-IN_KEYPATH: Decl[InstanceVar]/CurrNominal:      prop2[#ObjCClass?#]; name=prop2
// CHECK-IN_KEYPATH: Decl[InstanceVar]/CurrNominal:      prop3[#[ObjCClass]?#]; name=prop3
// CHECK-IN_KEYPATH: Decl[InstanceVar]/Super/IsSystem:   hashValue[#Int#]; name=hashValue

// Make sure we unwrap optionals (members of Optional itself are invalid in this context)
//
// CHECK-IN_KEYPATH_OPT-NOT: name=map

// Make sure we handle bridged types (i.e. show NSString members rather than String members)
//
// CHECK-IN_KEYPATH_BRIDGED_STRING: Decl[InstanceVar]/CurrNominal/IsSystem: urlsInText[#[URL]#]; name=urlsInText
// CHECK-IN_KEYPATH_BRIDGED_STRING: Decl[InstanceVar]/CurrNominal/IsSystem: uppercased[#String!#]; name=uppercased
// CHECK-IN_KEYPATH_BRIDGED_STRING-NOT: name=count


