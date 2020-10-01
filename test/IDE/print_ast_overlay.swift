// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module -module-name Foo -o %t -F %S/Inputs/mock-sdk %s -Xfrontend -enable-objc-interop -Xfrontend -disable-objc-attr-requires-foundation-module
//
// RUN: %target-swift-ide-test -print-module -source-filename %s -I %t -F %S/Inputs/mock-sdk -module-to-print=Foo -access-filter-public -enable-objc-interop -disable-objc-attr-requires-foundation-module > %t.printed.txt
// RUN: %FileCheck %s -check-prefix=PASS_WITH_OVERLAY -strict-whitespace < %t.printed.txt
// RUN: %FileCheck %s -check-prefix=PASS_NO_INTERNAL -strict-whitespace < %t.printed.txt
//
// RUN: %target-swift-ide-test -print-module -source-filename %s -I %t -F %S/Inputs/mock-sdk -module-to-print=Foo.FooSub -enable-objc-interop -disable-objc-attr-requires-foundation-module > %t.printed.txt
// RUN: %FileCheck %s -check-prefix=PASS_WITHOUT_OVERLAY -strict-whitespace < %t.printed.txt

// RUN: %target-swift-ide-test -print-module -source-filename %s -I %t -F %S/Inputs/mock-sdk -module-to-print=Foo -access-filter-public -annotate-print -enable-objc-interop -disable-objc-attr-requires-foundation-module > %t.annotated.txt
// RUN: %FileCheck %s -check-prefix=PASS_ANNOTATED -strict-whitespace < %t.annotated.txt

// REQUIRES: executable_test
// REQUIRES: swift_tools_extra

@_exported import Foo

public func overlay_func() {}
internal func overlay_func_internal() {}

public class FooOverlayClassBase {
  public func f() {}
}
public class FooOverlayClassDerived : FooOverlayClassBase {
  override public func f() {}
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

// PASS_NO_INTERNAL-NOT: overlay_func_internal

// PASS_ANNOTATED: <decl:Import>@_exported import <ref:module>Foo</ref></decl>
// PASS_ANNOTATED: <decl:Import>@_exported import <ref:module>Foo</ref>.<ref:module>FooSub</ref></decl>
// PASS_ANNOTATED: <decl:Import>@_exported import <ref:module>FooHelper</ref></decl>
