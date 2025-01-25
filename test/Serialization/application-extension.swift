// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name AppExtSafe -emit-module-path %t/AppExtSafe.swiftmodule -parse-as-library -serialize-debugging-options -application-extension %s

// Check the serialized flags paths.
// RUN: llvm-bcanalyzer -dump %t/AppExtSafe.swiftmodule > %t/AppExtSafe.swiftmodule.txt
// RUN: %FileCheck %s < %t/AppExtSafe.swiftmodule.txt

// CHECK-LABEL: <OPTIONS_BLOCK
// CHECK:      <XCC abbrevid={{[0-9]+}}/> blob data = '-fapplication-extension'
// CHECK: </OPTIONS_BLOCK>
