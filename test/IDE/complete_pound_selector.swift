// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=AFTER_POUND | FileCheck -check-prefix=CHECK-AFTER_POUND %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=SELECTOR_ARG1 | FileCheck -check-prefix=CHECK-SELECTOR_ARG %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=SELECTOR_ARG2 | FileCheck -check-prefix=CHECK-SELECTOR_ARG %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_SELECTOR1 | FileCheck -check-prefix=CHECK-IN_SELECTOR %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_SELECTOR2 | FileCheck -check-prefix=CHECK-IN_SELECTOR %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_SELECTOR3 | FileCheck -check-prefix=CHECK-IN_SUPER_SELECTOR %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=IN_SELECTOR4 | FileCheck -check-prefix=CHECK-IN_SUPER_SELECTOR %s

// REQUIRES: objc_interop

import Foundation

{
  if ##^AFTER_POUND^#
}

func selectorArg1(obj: NSObject) {
  obj.doSelector(#^SELECTOR_ARG1^#
}

func selectorArg2(obj: NSObject) {
  obj.messageSomeObject(obj, selector:#^SELECTOR_ARG2^#
}

func inSelector1() {
  _ = #selector(NSObject.#^IN_SELECTOR1^#)
}

func inSelector2() {
  _ = #selector(NSObject#^IN_SELECTOR2^#)
}

class Subclass : NSObject {
  func inSelector3() {
    _ = #selector(super.#^IN_SELECTOR3^#)
  }

  func inSelector4() {
    _ = #selector(super#^IN_SELECTOR4^#)
  }
}


// CHECK-AFTER_POUND: Keyword/ExprSpecific:               available({#Platform...#}, *); name=available(Platform..., *)
// CHECK-AFTER_POUND: Keyword/ExprSpecific:               selector({#@objc method#}); name=selector(@objc method)

// CHECK-SELECTOR_ARG: Keyword/ExprSpecific:               #selector({#@objc method#}); name=#selector(@objc method)

// CHECK-IN_SELECTOR: Decl[Constructor]/CurrNominal:      {{.?}}init; name=init
// CHECK-IN_SELECTOR: Decl[StaticMethod]/CurrNominal:     {{.?}}performSelector(_:withObject:); name=performSelector(_:withObject:)
// CHECK-IN_SELECTOR: Decl[InstanceMethod]/CurrNominal:   {{.?}}performSelector(_:withObject:); name=performSelector(_:withObject:)
// CHECK-IN_SELECTOR: Decl[InstanceMethod]/CurrNominal:   {{.?}}myClass; name=myClass
// CHECK-IN_SELECTOR: Decl[StaticMethod]/CurrNominal:     {{.?}}description; name=description
// CHECK-IN_SELECTOR: Decl[StaticMethod]/CurrNominal:     {{.?}}isEqual(_:); name=isEqual(_:)
// CHECK-IN_SELECTOR: Decl[InstanceMethod]/CurrNominal:   {{.?}}isEqual(_:); name=isEqual(_:)

// CHECK-IN_SUPER_SELECTOR: Decl[InstanceMethod]/CurrNominal:   {{.?}}performSelector(_:withObject:); name=performSelector(_:withObject:)
// CHECK-IN_SUPER_SELECTOR: Decl[InstanceMethod]/CurrNominal:   {{.?}}myClass; name=myClass
// CHECK-IN_SUPER_SELECTOR: Decl[InstanceMethod]/CurrNominal:   {{.?}}isEqual(_:); name=isEqual(_:)
