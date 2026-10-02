// RUN: not %target-swiftc_driver -scan-dependencies %s -o %t/deps.json 2>&1 | %FileCheck %s
// RUN: not %target-swiftc_driver -typecheck -explicit-module-build -nonlib-dependency-scanner %s -Xfrontend -diagnostic-style -Xfrontend llvm > %t/output.txt 2>&1
// RUN: %FileCheck %s --check-prefix=FALLBACK --input-file=%t/output.txt --implicit-check-not=jobFailedWithNonzeroExitCode

import invalid_module_that_never_exists

// CHECK: error: unable to resolve module dependency: 'invalid_module_that_never_exists'

// FALLBACK: error: dependency scan command failed with exit code 1
// FALLBACK-NEXT: {{.*}}scan-dependencies-missing-import.swift:{{[0-9]+}}:8: error: unable to resolve module dependency: 'invalid_module_that_never_exists'
// FALLBACK-NEXT: import invalid_module_that_never_exists
// FALLBACK-NEXT: ^
// FALLBACK-NEXT: {{.*}}scan-dependencies-missing-import.swift:{{[0-9]+}}:8: note: a dependency of main module
