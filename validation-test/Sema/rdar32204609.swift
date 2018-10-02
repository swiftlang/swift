// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test

let x: Int! = nil
let y: Int! = 1

print(x == y)
