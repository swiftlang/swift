// RUN: %target-swift-frontend -primary-file %s %S/../Inputs/empty.swift -emit-ir -g -module-name multi | FileCheck %s
// RUN: %target-swift-frontend %S/../Inputs/empty.swift -primary-file %s -emit-ir -g -module-name multi | FileCheck %s
// RUN: %target-swift-frontend -primary-file %S/../Inputs/empty.swift %s -emit-ir -g -module-name multi | FileCheck %s --check-prefix=CHECK-OTHER
// RUN: %target-swift-frontend %s -primary-file %S/../Inputs/empty.swift -emit-ir -g -module-name multi | FileCheck %s --check-prefix=CHECK-OTHER
// CHECK: !DICompileUnit({{.*}}file: ![[FILE:[0-9]+]]
// CHECK: ![[FILE]] = !DIFile(filename: "multi-file.swift"
// CHECK-OTHER: !DICompileUnit({{.*}}file: ![[FILE:[0-9]+]]
// CHECK-OTHER: ![[FILE]] = !DIFile(filename: "empty.swift"
