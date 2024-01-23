// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %t/main.swift -module-name Test -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -I %t/include -swift-version 4

// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps.json Test directDependencies | %FileCheck %s

// CHECK-DAG: "clang": "C"
// CHECK-DAG: "clang": "ClangTest"
// CHECK-DAG: "swift": "A"
// CHECK-DAG: "swift": "F"
// CHECK-NOT: "swift": "G"
// CHECK-NOT: "swift": "B"
// CHECK-NOT: "Missing"

//--- main.swift

#if canImport(Missing)
import G
#endif

#if canImport(C)
import A
#else
import B
#endif

#if !canImport(F)
import Missing
#endif

// B is not dependency
#if false && canImport(B)
import Missing
#endif

// B is short circuited
#if canImport(C) || canImport(B)
#endif

// Check clang submodule, this should import ClangTest, not ClangTest.Sub
#if canImport(ClangTest.Sub)
#endif

//--- include/module.modulemap
module ClangTest {
  module Sub {
    header "sub.h"
    export *
  }
}

//--- include/sub.h
void notused(void);
