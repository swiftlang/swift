// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -module-name=A -debug-module-path %t/MY_MODULE_PATH.swiftmodule -emit-ir -o - | %FileCheck %s

// CHECK: DIModule(scope: null, name: "A", includePath: "{{.*}}MY_MODULE_PATH.swiftmodule")

