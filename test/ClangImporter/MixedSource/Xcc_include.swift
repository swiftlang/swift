// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -Xcc -include -Xcc %S/Inputs/Xcc_include.h -typecheck %s 2>&1 | %FileCheck -check-prefix=CHECK-INCLUDE-ONLY %s
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -Xcc -include -Xcc %S/Inputs/Xcc_include.h -enable-objc-interop -import-objc-header %S/../../Inputs/empty.swift -typecheck %s 2>&1 | %FileCheck -check-prefix=CHECK-INCLUDE-PLUS-BRIDGING %s
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -Xcc -include -Xcc %S/Inputs/Xcc_include.h -F %S/Inputs/mixed-target/ -module-name Mixed -import-underlying-module -typecheck %s -enable-objc-interop -disable-objc-attr-requires-foundation-module 2>&1 | %FileCheck -check-prefix=CHECK-INCLUDE-FRAMEWORK %s
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -Xcc -include -Xcc %S/Inputs/this_header_does_not_exist.h -typecheck %s 2>&1 | %FileCheck -check-prefix=CHECK-INCLUDE-MISSING %s

// CHECK-INCLUDE-MISSING: error: '{{.*}}this_header_does_not_exist.h' file not found

func test() {
  // CHECK-INCLUDE-ONLY: error: use of unresolved identifier 'includedConst'
  // CHECK-INCLUDE-PLUS-BRIDGING-NOT: unresolved identifier 'includedConst'
  // CHECK-INCLUDE-FRAMEWORK: error: use of unresolved identifier 'includedConst'
  _ = includedConst
  
  // CHECK-INCLUDE-ONLY: error: use of unresolved identifier 'errSecSuccess'
  // CHECK-INCLUDE-PLUS-BRIDGING: error: use of unresolved identifier 'errSecSuccess'
  // CHECK-INCLUDE-FRAMEWORK: error: use of unresolved identifier 'errSecSuccess'
  _ = errSecSuccess

#if FRAMEWORK
  // CHECK-INCLUDE-FRAMEWORK-NOT: error: unresolved identifier 'Base'
  _ = Base()
#endif
}
