// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -whole-module-optimization -emit-module-path %t/UsingObjCStuff.swiftmodule -c -o %t/UsingObjCStuff.o -module-name UsingObjCStuff -I %t -I %S/Inputs/mixed_mode -swift-version 5 -parse-as-library %S/Inputs/mixed_mode/UsingObjCStuff.swift
// RUN: %target-build-swift -o %t/a.out.v4 -I %t -I %S/Inputs/mixed_mode -module-name main -swift-version 4 %t/main.swift %t/UsingObjCStuff.o
// RUN: %target-build-swift -o %t/a.out.v5 -I %t -I %S/Inputs/mixed_mode -module-name main -swift-version 5 %t/main.swift %t/UsingObjCStuff.o
// RUN: %target-codesign %t/a.out.v4
// RUN: %target-codesign %t/a.out.v5
// RUN: %target-run %t/a.out.v4 | %FileCheck %s
// RUN: %target-run %t/a.out.v5 | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: executable_test

import UsingObjCStuff

print("Let's go") // CHECK: Let's go

let holder = ButtHolder()
holder.x += 17
print(holder.x) // CHECK-NEXT: 17
holder.x += 38
print(holder.x) // CHECK-NEXT: 55
holder.z += "hello"
print(holder.z) // CHECK-NEXT: hello
holder.z += " world"
print(holder.z) // CHECK-NEXT: hello world

holder.virtual() // CHECK-NEXT: 55 [:] hello world

class SubButtHolder: ButtHolder {
  final var w: Double = 0

  override func virtual() {
    print("~* ", terminator: "")
    super.virtual()
    print(" \(w) *~")
  }

  func subVirtual() {
    print("~* \(x) ~*~ \(z) ~*~ \(w) *~")
  }
}

class SubSubButtHolder: SubButtHolder {
  override func virtual() {
    print("!!! ", terminator: "")
    super.virtual()
    print(" !!!")
  }
  override func subVirtual() {
    print("!!! ", terminator: "")
    super.subVirtual()
    print(" !!!")
  }
}

@inline(never)
func exerciseSubHolder(with subHolder: SubButtHolder) {
  subHolder.x = 679
  subHolder.z = "goodbye folks"
  subHolder.w = 90.5

  subHolder.virtual() // CHECK-NEXT: !!! ~* 679 [:] goodbye folks
                      // CHECK-NEXT: 90.5 *~
                      // CHECK-NEXT: !!!
  subHolder.subVirtual() // CHECK-NEXT: !!! ~* 679 ~*~ goodbye folks ~*~ 90.5 *~
                         // CHECK-NEXT: !!!
}

exerciseSubHolder(with: SubSubButtHolder())


print("OK that's it") // CHECK-NEXT: OK that's it
