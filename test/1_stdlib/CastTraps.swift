// These tests should crash.
// RUN: mkdir -p %t
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/CatchCrashes.cpp -c -o %t/CatchCrashes.o
// RUN: %target-build-swift %s -Xlinker %t/CatchCrashes.o -o %t/a.out
// 
// RUN: %target-run %t/a.out ClassToClass 2>&1 | FileCheck %s -check-prefix=ClassToClass
// RUN: %target-run %t/a.out ClassToStruct 2>&1 | FileCheck %s -check-prefix=ClassToStruct
// RUN: %target-run %t/a.out StructToClass 2>&1 | FileCheck %s -check-prefix=StructToClass

// FIXME: Casting.cpp has dozens of places to fail a cast. This test does not 
// attempt to enumerate them all.

class C1 { }
class C2 { }
struct S1 { }

let arg = Process.arguments[1]

switch (arg) {

case "ClassToClass":
  (C1() as Any) as C2
  // ClassToClass: Could not cast value of type 'a.C1' (0x{{[0-9a-fA-F]+}}) to 'a.C2' (0x{{[0-9a-fA-F]+}}).
  // ClassToClass: CRASHED: SIG{{ILL|TRAP|ABRT}}

case "ClassToStruct":
  (C1() as Any) as S1
  // ClassToStruct: Could not cast value of type 'a.C1' (0x{{[0-9a-fA-F]+}}) to 'a.S1' (0x{{[0-9a-fA-F]+}}).
  // ClassToStruct: CRASHED: SIG{{ILL|TRAP|ABRT}}

case "StructToClass":
  (S1() as Any) as C2
  // StructToClass: Could not cast value of type 'a.S1' (0x{{[0-9a-fA-F]+}}) to 'a.C2' (0x{{[0-9a-fA-F]+}}).
  // StructToClass: CRASHED: SIG{{ILL|TRAP|ABRT}}

default:
  println("wut")
}

println("should have crashed")
