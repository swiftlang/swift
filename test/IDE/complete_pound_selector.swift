// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=AFTER_POUND | FileCheck -check-prefix=CHECK-AFTER_POUND %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=SELECTOR_ARG1 | FileCheck -check-prefix=CHECK-SELECTOR_ARG %s

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=SELECTOR_ARG2 | FileCheck -check-prefix=CHECK-SELECTOR_ARG %s

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

// CHECK-AFTER_POUND: Keyword/ExprSpecific:               available({#Platform...#}, *); name=available(Platform..., *)
// CHECK-AFTER_POUND: Keyword/ExprSpecific:               selector({#@objc method#}); name=selector(@objc method)

// CHECK-SELECTOR_ARG: Keyword/ExprSpecific:               #selector({#@objc method#}); name=#selector(@objc method)
