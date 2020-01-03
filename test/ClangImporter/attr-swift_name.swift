// RUN: %empty-directory(%t.mcp)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -Xcc -w -typecheck %s -module-cache-path %t.mcp -disable-named-lazy-member-loading 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

// CHECK-NOT: warning:

import ObjCIRExtras

func test(_ i: Int) {
  let t = SwiftNameTest()
  
  t.theMethod(number: i)

  _ = t.renamedSomeProp
  _ = type(of: t).renamedClassProp

  // We only see these two warnings because Clang can catch the other invalid
  // cases, and marks the attribute as invalid ahead of time.
  
  // CHECK: warning: too few parameters in swift_name attribute (expected 2; got 1)
  // CHECK: + (instancetype)g:(id)x outParam:(int *)foo SWIFT_NAME(init(g:));
  // CHECK-NOT: warning:
  // CHECK: note: please report this issue to the owners of 'ObjCIRExtras'
  // CHECK-NOT: warning:

  // CHECK: warning: too few parameters in swift_name attribute (expected 2; got 1)
  // CHECK: + (instancetype)testW:(id)x out:(id *)outObject SWIFT_NAME(ww(_:));
  // CHECK-NOT: warning:
  // CHECK: note: please report this issue to the owners of 'ObjCIRExtras'
  // CHECK-NOT: warning:
}
