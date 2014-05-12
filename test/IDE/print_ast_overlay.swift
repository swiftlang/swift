// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -module-name Foo -o %t -F %S/Inputs/mock-sdk -module-cache-path %t/clang-module-cache %s
//
// RUN: %swift-ide-test -print-module -source-filename %s -I %t -F %S/Inputs/mock-sdk -module-cache-path %t/clang-module-cache -module-to-print=Foo > %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_WITH_OVERLAY -strict-whitespace < %t.printed.txt
//
// RUN: %swift-ide-test -print-module -source-filename %s -I %t -F %S/Inputs/mock-sdk -module-cache-path %t/clang-module-cache -module-to-print=Foo.FooSub > %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_WITHOUT_OVERLAY -strict-whitespace < %t.printed.txt

@exported import Foo

func overlay_func() {}

class FooOverlayClassBase {
  func f() {}
}
class FooOverlayClassDerived : FooOverlayClassBase {
  override func f() {}
}

// Check that given a top-level module with an overlay, AST printer prints
// declarations from both of them.

// PASS_WITH_OVERLAY-LABEL: {{^}}class FooClassBase {
// PASS_WITH_OVERLAY-LABEL: {{^}}class FooOverlayClassDerived : FooOverlayClassBase {
// PASS_WITH_OVERLAY-NEXT:  {{^}}  override func f()
// PASS_WITH_OVERLAY: {{^}}func overlay_func(){{$}}

// But when printing a submodule, AST printer should not print the overlay,
// because overlay declarations are logically in the top-level module.

// PASS_WITHOUT_OVERLAY-NOT: overlay_func

