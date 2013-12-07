// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -module-name FooClangModule -o %t -I %S/Inputs/custom-modules -module-cache-path=%t/clang-module-cache %s
// RUN: %swift-ide-test -print-module -source-filename %s -I %t -I %S/Inputs/custom-modules -module-cache-path=%t/clang-module-cache -module-to-print=FooClangModule > %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_COMMON -strict-whitespace < %t.printed.txt

import FooClangModule

func overlay_func() {}

// Check that given a module with an overlay, AST printer prints declarations
// from both of them.

// PASS_COMMON: @objc class FooClass {
// PASS_COMMON: func overlay_func()

