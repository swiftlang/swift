// RUN: %empty-directory(%t/out)
// RUN: split-file %s %t

// rdar://129337999 - Make sure we can link without issues.

// The legacy driver does not support doing this in one invocation, so split it
// up for compatibility with both the legacy and new driver.
// RUN: %target-build-swift -c %t/a.swift -profile-generate -profile-coverage-mapping -parse-as-library -module-name A -o %t/out/a.swift.o
// RUN: %target-build-swift -emit-module %t/a.swift -profile-generate -profile-coverage-mapping -parse-as-library -module-name A -emit-module-path %t/out/A.swiftmodule

// RUN: %target-build-swift %t/b.swift -profile-generate -profile-coverage-mapping -o %t/main -module-name B -I %t/out %t/out/a.swift.o

// RUN: %target-codesign %t/main
// RUN: env %env-LLVM_PROFILE_FILE=%t/default.profraw %target-run %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata

// The implicit-check-not here ensures we match all recorded line counts.
// RUN: %llvm-cov show %t/main -instr-profile=%t/default.profdata | %FileCheck --implicit-check-not "{{[ ]*\|[ ]*[0-9]+}}" %s

// REQUIRES: profile_runtime
// REQUIRES: executable_test

// FIXME: Currently fails on non-Darwin (https://github.com/swiftlang/swift/issues/75240)
// REQUIRES: OS=macosx

//--- a.swift
@_transparent
public func foo() {}
// CHECK: 1|public func foo() {}

//--- b.swift
import A

foo()
// CHECK: 1|foo()
