// RUN: %target-swift-frontend -typecheck %s -import-objc-header %S/Inputs/enum-inferred-exhaustivity.h -swift-version 4 2>&1 | %FileCheck -check-prefix=CHECK-4 %s
// RUN: not %target-swift-frontend -typecheck %s -import-objc-header %S/Inputs/enum-inferred-exhaustivity.h -swift-version 5 2>&1 | %FileCheck -check-prefix=CHECK-5 %s

// This is testing what happens with a CF_ENUM definition that doesn't include
// any enum_extensibility attributes, like those predating macOS High Sierra. As
// such, the test deliberately avoids importing anything that might pull in
// CoreFoundation, even from the mock SDK.

func test(_ value: EnumWithDefaultExhaustivity) {
  // CHECK-4: [[@LINE+2]]:{{[0-9]+}}: warning: switch must be exhaustive
  // CHECK-5: [[@LINE+1]]:{{[0-9]+}}: error: switch must be exhaustive
  switch value {
  case .loneCase: break
  }
}
