// REQUIRES: objc_interop

// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../../test/Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../../test/Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift -disable-objc-attr-requires-foundation-module
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../../test/Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/Inputs/reintroduced-new.swift -swift-version 4 -disable-objc-attr-requires-foundation-module -module-name main
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../../test/Inputs/clang-importer-sdk -I %t) -parse-as-library %t/main.swiftmodule -typecheck -emit-objc-header-path %t/generated.h -disable-objc-attr-requires-foundation-module -swift-version 4
// RUN: not %clang -fsyntax-only -x objective-c %s -include %t/generated.h -fobjc-arc -fmodules -Werror -F %clang-importer-sdk-path/frameworks -isysroot %S/../../test/Inputs/clang-importer-sdk 2>&1 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-4 %s

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../../test/Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/Inputs/reintroduced-new.swift -disable-objc-attr-requires-foundation-module -module-name main -swift-version 5
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../../test/Inputs/clang-importer-sdk -I %t) -parse-as-library %t/main.swiftmodule -typecheck -emit-objc-header-path %t/generated.h -disable-objc-attr-requires-foundation-module -swift-version 5
// RUN: not %clang -fsyntax-only -x objective-c %s -include %t/generated.h -fobjc-arc -fmodules -Werror -F %clang-importer-sdk-path/frameworks -isysroot %S/../../test/Inputs/clang-importer-sdk 2>&1 | %FileCheck  -check-prefix=CHECK -check-prefix=CHECK-5 %s

// CHECK-NOT: error:

void test() {
  // CHECK: :[[@LINE+1]]:{{[0-9]+}}: error: 'init' is unavailable
  (void)[[Base alloc] init];
   // CHECK-NOT: error:
  (void)[[Sub alloc] init];
  // CHECK-4: :[[@LINE+2]]:{{[0-9]+}}: error: 'new' is deprecated: -init is unavailable
  // CHECK-5: :[[@LINE+1]]:{{[0-9]+}}: error: 'new' is unavailable: -init is unavailable
  (void)[Base new];
   // CHECK-NOT: error:
  (void)[Sub new];
}

// CHECK-NOT: error:
