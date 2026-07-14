
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t.module-cache)
// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule %s -parse-as-library -serialized-path-obfuscate /FOO=/CHANGED_FOO -I /FOO/contents -module-name Foo -serialize-debugging-options -deterministic-sourceinfo
// RUN: %llvm-bcanalyzer -dump %t/Foo.swiftsourceinfo | %FileCheck %s --check-prefix=CHECK-SOURCEINFO

public class A {}

// CHECK-SOURCEINFO: <DECL_LOCS_BLOCK 
// CHECK-SOURCEINFO: <TEXT_DATA abbrevid={{[0-9]+}} op0=13/> blob data = '/CHANGED_FOO/'
