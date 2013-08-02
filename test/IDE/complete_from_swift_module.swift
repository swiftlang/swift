// RUN: %swift-ide-test -code-completion -source-filename %s -I %S/custom-modules -code-completion-token=MODULE_QUALIFIED_1 | FileCheck %s -check-prefix=MODULE_QUALIFIED_1

import foo_swift_module

func testCompleteModuleQualified1() {
  foo_swift_module.#^MODULE_QUALIFIED_1^#
// Check that we don't include references to operators.
// MODULE_QUALIFIED_1-NOT: SwiftDecl: %%%
}

