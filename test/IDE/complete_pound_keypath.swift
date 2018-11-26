// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=AFTER_POUND | %FileCheck -check-prefix=CHECK-AFTER_POUND %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=KEYPATH_ARG | %FileCheck -check-prefix=CHECK-KEYPATH_ARG %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_KEYPATH_1 | %FileCheck -check-prefix=CHECK-IN_KEYPATH %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_KEYPATH_2 | %FileCheck -check-prefix=CHECK-IN_KEYPATH %s


// REQUIRES: objc_interop

import Foundation

{
  if ##^AFTER_POUND^#
}

func acceptKeyPath(_ keyPath: String) { }

func selectorArg1(obj: NSObject) {
  acceptKeyPath(#^KEYPATH_ARG^#
}

class ObjCClass : NSObject {
  var prop1: String = ""
  var prop2: ObjCClass?

  func completeInKeyPath1() {
    _ = #keyPath(#^IN_KEYPATH_1^#
  }
}

func completeInKeyPath2() {
  _ = #keyPath(ObjCClass.#^IN_KEYPATH_2^#
}

// CHECK-AFTER_POUND: Keyword/ExprSpecific:               keyPath({#@objc property sequence#}); name=keyPath(@objc property sequence)

// CHECK-KEYPATH_ARG: Keyword/None:                       #keyPath({#@objc property sequence#}); name=#keyPath(@objc property sequence)

// CHECK-IN_KEYPATH: Decl[InstanceVar]/CurrNominal:      prop1[#String#]; name=prop1
// CHECK-IN_KEYPATH: Decl[InstanceVar]/CurrNominal:      prop2[#ObjCClass?#]; name=prop2
// CHECK-IN_KEYPATH: Decl[InstanceVar]/Super:            hashValue[#Int#]; name=hashValue


