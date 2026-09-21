// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -Xllvm -sil-disable-pass=generic-specializer -module-name=test %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

// Regression test: ConstantCapturePropagation used to crash the compiler
// when specializing a partial_apply whose constant-captured closure argument's
// declared parameter type has a generic parameter, as
// happens with the reabstraction thunk synthesized for `withUnsafeBytes`'s
// generic `Result`.

import Foundation

@inline(never)
func repro() -> Data {
    let bytes = [UInt8](repeating: 42, count: 4)
    return bytes.withUnsafeBytes { body in
        Data(body)
    }
}

// CHECK: [42, 42, 42, 42]
print(Array(repro()))
