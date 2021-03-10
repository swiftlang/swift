// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -o %t/singlemissing.swiftmodule -experimental-allow-module-with-compiler-errors missing.swift 2>&1 | %FileCheck -check-prefix=CHECK-SINGLEMISSING %s
// CHECK-SINGLEMISSING: error opening input file 'missing.swift'
// RUN: llvm-bcanalyzer %t/singlemissing.swiftmodule | %FileCheck -check-prefix=CHECK-BC %s

// RUN: %target-swift-frontend -emit-module -o %t/multimissingwmo.swiftmodule -experimental-allow-module-with-compiler-errors -whole-module-optimization missing.swift missing2.swift 2>&1 | %FileCheck -check-prefix=CHECK-MULTIMISSINGWMO %s
// CHECK-MULTIMISSINGWMO-DAG: error opening input file 'missing.swift'
// CHECK-MULTIMISSINGWMO-DAG: error opening input file 'missing2.swift'
// RUN: llvm-bcanalyzer %t/multimissingwmo.swiftmodule | %FileCheck -check-prefix=CHECK-BC %s

// RUN: %target-swift-frontend -emit-module -o %t/singlemissingwmo.swiftmodule -experimental-allow-module-with-compiler-errors -whole-module-optimization %s missing.swift  2>&1 | %FileCheck -check-prefix=CHECK-SINGLEMISSINGWMO %s
// CHECK-SINGLEMISSINGWMO: error opening input file 'missing.swift'
// RUN: llvm-bcanalyzer %t/singlemissingwmo.swiftmodule | %FileCheck -check-prefix=CHECK-BC %s

// RUN: %target-swift-frontend -emit-module -o %t/nonprimarymissing.swiftmodule -experimental-allow-module-with-compiler-errors -primary-file %s missing.swift 2>&1 | %FileCheck -check-prefix=CHECK-NONPRIMARYMISSING %s
// CHECK-NONPRIMARYMISSING: error opening input file 'missing.swift'
// RUN: llvm-bcanalyzer %t/nonprimarymissing.swiftmodule | %FileCheck -check-prefix=CHECK-BC %s

// RUN: %target-swift-frontend -emit-module -o %t/primarymissing.swiftmodule -experimental-allow-module-with-compiler-errors -primary-file missing.swift %s 2>&1 | %FileCheck -check-prefix=CHECK-PRIMARYMISSING %s
// CHECK-PRIMARYMISSING: error opening input file 'missing.swift'
// RUN: llvm-bcanalyzer %t/primarymissing.swiftmodule | %FileCheck -check-prefix=CHECK-BC %s

func foo() -> Int { return 0 }

// CHECK-BC-NOT: UnknownCode
