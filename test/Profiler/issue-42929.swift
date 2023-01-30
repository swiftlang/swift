// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -profile-generate -profile-coverage-mapping -o %t/main

// This unusual use of 'sh' allows the path of the profraw file to be
// substituted by %target-run.
// RUN: %target-codesign %t/main
// RUN: %target-run sh -c 'env LLVM_PROFILE_FILE=$1 $2' -- %t/default.profraw %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %llvm-cov export -summary-only %t/main -instr-profile=%t/default.profdata | %FileCheck %s

// REQUIRES: profile_runtime
// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// https://github.com/apple/swift/issues/42929

// There are actually only 14 lines here, but llvm-cov doesn't handle closures
// properly (rdar://90348983).
// CHECK: "lines":{"count":18,"covered":18{{.*}}"functions":{"count":7,"covered":7

import Foundation

func foo(_ x: @autoclosure () -> Bool) { _ = x() }

func bar() {
  foo(.random())
  foo(.random())

  class C : NSObject {
    class D : NSObject {
    }
  }

  foo(.random())
  foo(.random())
}
bar()
