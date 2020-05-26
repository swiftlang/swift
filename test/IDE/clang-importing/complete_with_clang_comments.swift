// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=TOP -code-completion-comments=true \
// RUN:    -import-objc-header %S/Inputs/bridge.h -I %S/Inputs/somemod1 -I %S/Inputs/somemod2 | %FileCheck %s -check-prefix=CHECK-TOP
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=TOP -code-completion-comments=true \
// RUN:    -import-objc-header %S/Inputs/bridge.h -pch-output-dir %t.pch -I %S/Inputs/somemod1 -I %S/Inputs/somemod2 | %FileCheck %s -check-prefix=CHECK-TOP

// REQUIRES: objc_interop

import somemod2

#^TOP^#
// CHECK-TOP: name=some_func11(); comment=some_func11 is cool function.
// CHECK-TOP: name=some_func12(); comment=some_func12 is cool function.
// CHECK-TOP: name=some_func21(); comment=some_func21 is cool function.
// CHECK-TOP: name=some_func22(); comment=some_func22 is cool function.
