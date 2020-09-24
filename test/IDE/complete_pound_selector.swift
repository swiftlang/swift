// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=AFTER_POUND | %FileCheck -check-prefix=CHECK-AFTER_POUND %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CONTEXT_SELECTOR | %FileCheck -check-prefix=CHECK-CONTEXT_SELECTOR %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=SELECTOR_ARG1 | %FileCheck -check-prefix=CHECK-CONTEXT_SELECTOR %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=SELECTOR_ARG2 | %FileCheck -check-prefix=CHECK-CONTEXT_SELECTOR %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=SELECTOR_BASIC | %FileCheck -check-prefix=CHECK-SELECTOR_BASIC %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_SELECTOR1 | %FileCheck -check-prefix=CHECK-IN_SELECTOR %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_SELECTOR2 | %FileCheck -check-prefix=CHECK-IN_SELECTOR %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_SELECTOR3 | %FileCheck -check-prefix=CHECK-IN_SELECTOR %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_SELECTOR4 | %FileCheck -check-prefix=CHECK-IN_SELECTOR %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_SELECTOR5 | %FileCheck -check-prefix=CHECK-IN_SUPER_SELECTOR %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_SELECTOR6 | %FileCheck -check-prefix=CHECK-IN_SUPER_SELECTOR %s

// REQUIRES: objc_interop

import Foundation

do {
  if ##^AFTER_POUND^#
}

do {
  let _: Selector = #^CONTEXT_SELECTOR^#
}

func selectorArg1(obj: NSObject) {
  obj.`do`(#^SELECTOR_ARG1^#
}

func selectorArg2(obj: NSObject) {
  obj.messageSomeObject(obj, selector:#^SELECTOR_ARG2^#
}

func inSelectorBasic() {
  _ = #selector(#^SELECTOR_BASIC^#)
}

func inSelector1() {
  _ = #selector(NSObject.#^IN_SELECTOR1^#)
}

func inSelector2() {
  _ = #selector(NSObject#^IN_SELECTOR2^#)
}

func inSelector3() {
  _ = #selector(getter: NSObject#^IN_SELECTOR3^#)
}

func inSelector4() {
  _ = #selector(setter: NSObject#^IN_SELECTOR4^#)
}

class Subclass : NSObject {
  func inSelector3() {
    _ = #selector(super.#^IN_SELECTOR5^#)
  }

  func inSelector4() {
    _ = #selector(super#^IN_SELECTOR6^#)
  }
}


// CHECK-AFTER_POUND-NOT: selector
// CHECK-AFTER_POUND: Keyword/ExprSpecific:               available({#Platform...#}, *); name=available(Platform..., *)

// CHECK-CONTEXT_SELECTOR: Keyword/None/TypeRelation[Identical]: #selector({#@objc method#})[#Selector#]; name=#selector(@objc method)

// CHECK-SELECTOR_BASIC: Keyword/None:                       getter: {#@objc property#}; name=getter: @objc property
// CHECK-SELECTOR_BASIC: Keyword/None:                       setter: {#@objc property#}; name=setter: @objc property

// CHECK-IN_SELECTOR-NOT: getter:
// CHECK-IN_SELECTOR: Decl[Constructor]/CurrNominal/IsSystem:    {{.?}}init[#(NSObject.Type) -> () -> NSObject#]; name=init
// CHECK-IN_SELECTOR: Decl[StaticMethod]/CurrNominal/IsSystem:   {{.?}}perform(_:with:)[#(Selector?, Any?) -> Unmanaged<AnyObject>?#]; name=perform(_:with:)
// CHECK-IN_SELECTOR: Decl[InstanceMethod]/CurrNominal/IsSystem: {{.?}}perform(_:with:)[#(NSObject) -> (Selector?, Any?) -> Unmanaged<AnyObject>?#]; name=perform(_:with:)
// CHECK-IN_SELECTOR: Decl[InstanceMethod]/CurrNominal/IsSystem: {{.?}}myClass[#(NSObject) -> () -> AnyClass?#]; name=myClass
// CHECK-IN_SELECTOR: Decl[StaticMethod]/CurrNominal/IsSystem:   {{.?}}description[#() -> Any#]; name=description
// CHECK-IN_SELECTOR: Decl[StaticMethod]/CurrNominal/IsSystem:   {{.?}}isEqual(_:)[#(NSObject?) -> Bool#]; name=isEqual(_:)
// CHECK-IN_SELECTOR: Decl[InstanceMethod]/CurrNominal/IsSystem: {{.?}}isEqual(_:)[#(NSObject) -> (NSObject?) -> Bool#]; name=isEqual(_:)

// CHECK-IN_SUPER_SELECTOR: Decl[InstanceMethod]/CurrNominal/IsSystem: {{.?}}perform(_:with:)[#(Selector?, Any?) -> Unmanaged<AnyObject>?#]; name=perform(_:with:)
// CHECK-IN_SUPER_SELECTOR: Decl[InstanceMethod]/CurrNominal/IsSystem: {{.?}}myClass[#() -> AnyClass?#]; name=myClass
// CHECK-IN_SUPER_SELECTOR: Decl[InstanceMethod]/CurrNominal/IsSystem: {{.?}}isEqual(_:)[#(NSObject?) -> Bool#]; name=isEqual(_:)
