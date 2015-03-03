// RUN: %target-swift-frontend -primary-file %s %S/../Inputs/empty.swift -emit-ir -g -module-name multi | FileCheck %s
// RUN: %target-swift-frontend %S/../Inputs/empty.swift -primary-file %s -emit-ir -g -module-name multi | FileCheck %s
// RUN: %target-swift-frontend -primary-file %S/../Inputs/empty.swift %s -emit-ir -g -module-name multi | FileCheck %s --check-prefix=CHECK-OTHER
// RUN: %target-swift-frontend %s -primary-file %S/../Inputs/empty.swift -emit-ir -g -module-name multi | FileCheck %s --check-prefix=CHECK-OTHER
// CHECK: !MDCompileUnit({{.*}}file: ![[FILE:[0-9]+]]
// CHECK: ![[FILE]] = !MDFile(filename: "multi-file.swift"
// CHECK-OTHER: !MDCompileUnit({{.*}}file: ![[FILE:[0-9]+]]
// CHECK-OTHER: ![[FILE]] = !MDFile(filename: "empty.swift"
