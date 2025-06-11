// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test

import POD

extension Empty {
   public static func == (lhs: Empty, rhs: Empty) -> Bool {
        Unmanaged.passUnretained(lhs).toOpaque() == Unmanaged.passUnretained(rhs).toOpaque()
   }
}

let x = Empty.create()
let y = Empty.create()

print(x == y)
// CHECK: false

print(x == x)
// CHECK: true
