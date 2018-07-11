// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name alias -emit-module -o %t %S/Inputs/alias.swift
// RUN: %target-build-swift -I %t %s -module-name typealias -emit-module-path %t/typealias.swiftmodule -o %t/typealias.o
// RUN: llvm-bcanalyzer %t/alias.swiftmodule | %FileCheck %s
// RUN: %target-build-swift -I %t %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck -check-prefix=OUTPUT %s
// REQUIRES: executable_test

// CHECK-NOT: UnknownCode

import alias

var i : MyInt64
i = 42
var j : AnotherInt64 = i
print("\(j)\n", terminator: "")

// OUTPUT: 42

var both : TwoInts
both = (i, j)

var named : ThreeNamedInts
named.b = 64
print("\(named.b)\n", terminator: "")

// OUTPUT: 64

var none : None
none = ()

func doNothing() {}
var nullFn : NullFunction = doNothing

func negate(x: MyInt64) -> AnotherInt64 {
  return -x
}
var monadic : IntFunction = negate
print("\(monadic(i))\n", terminator: "")

// OUTPUT: -42

func subtract(_ t : (x: MyInt64, y: MyInt64)) -> MyInt64 {
  return t.x - t.y
}
var dyadic : TwoIntFunction = subtract
print("\(dyadic((named.b, i))) \(dyadic(both))\n", terminator: "")

// OUTPUT: 22 0

// Used for tests that only need to be type-checked.
func check(_: BaseAlias) {
}

let x: GG<Int> = 0
let x2: GInt = 1

public typealias TestUnbound = UnboundAlias
