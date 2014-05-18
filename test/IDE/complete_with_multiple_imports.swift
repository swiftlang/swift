// RUN: rm -rf %t
// RUN: mkdir -p %t

import Foo
import FooHelper

// This checks that we don't provide duplicate results because FooHelper is also imported through Foo.
// RUN: %swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -module-cache-path %t/clang-module-cache -code-completion-token=TOP_COMPLETIONS | FileCheck %s -check-prefix=CHECK-TOP

func testClangModule() {
  #^TOP_COMPLETIONS^#
}

// CHECK-TOP:     fooHelperFunc1
// CHECK-TOP-NOT: fooHelperFunc1
