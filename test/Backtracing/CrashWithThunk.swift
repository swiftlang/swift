// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Onone -g -o %t/CrashWithThunk
// RUN: %target-codesign %t/CrashWithThunk
// RUN: (env SWIFT_BACKTRACE=enable=yes,cache=no %target-run %t/CrashWithThunk || true) | %FileCheck %s
// RUN: (env SWIFT_BACKTRACE=preset=friendly,enable=yes,cache=no %target-run %t/CrashWithThunk || true) | %FileCheck %s --check-prefix FRIENDLY

// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx

struct Foo<T> {
  var value: T
}

func crash() {
  print("I'm going to crash here")
  let ptr = UnsafeMutablePointer<Int>(bitPattern: 4)!
  ptr.pointee = 42
}

@main
struct CrashWithThunk {
  static func main() {
    let foo = Foo(value: crash)

    foo.value()
  }
}

// CHECK: *** Program crashed: Bad pointer dereference at 0x{{0+}}4 ***

// CHECK: Thread 0 crashed:

// CHECK: 0                       0x{{[0-9a-f]+}} crash() + {{[0-9]+}} in CrashWithThunk at {{.*}}/CrashWithThunk.swift:18:15
// CHECK-NEXT: 1 [ra] [thunk] [system] 0x{{[0-9a-f]+}} thunk for @escaping @callee_guaranteed () -> () + {{[0-9]+}} in CrashWithThunk at {{.*}}/Backtracing/<compiler-generated>
// CHECK-NEXT: 2 [ra]                  0x{{[0-9a-f]+}} static CrashWithThunk.main() + {{[0-9]+}} in CrashWithThunk at {{.*}}/CrashWithThunk.swift:26:9
// CHECK-NEXT: 3 [ra] [system]         0x{{[0-9a-f]+}} static CrashWithThunk.$main() + {{[0-9]+}} in CrashWithThunk at {{.*}}/CrashWithThunk.swift:21:1
// CHECK-NEXT: 4 [ra] [system]         0x{{[0-9a-f]+}} main + {{[0-9]+}} in CrashWithThunk at {{.*}}/CrashWithThunk.swift

// CHECK: Registers:

// CHECK: Images ({{[0-9]+}} omitted):

// CHECK: {{0x[0-9a-f]+}}–{{0x[0-9a-f]+}}{{ +}}{{[0-9a-f]+}}{{ +}}CrashWithThunk{{ +}}{{.*}}/CrashWithThunk

// FRIENDLY: *** Program crashed: Bad pointer dereference at 0x{{0+}}4 ***

// FRIENDLY: Thread 0 crashed:

// FRIENDLY: 0 crash() + {{[0-9]+}} in CrashWithThunk at {{.*}}/CrashWithThunk.swift:18:15

// FRIENDLY: 16|   print("I'm going to crash here")
// FRIENDLY-NEXT: 17|   let ptr = UnsafeMutablePointer<Int>(bitPattern: 4)!
// FRIENDLY-NEXT: 18|   ptr.pointee = 42
// FRIENDLY-NEXT:   |               ^
// FRIENDLY-NEXT: 19| }
// FRIENDLY-NEXT: 20|

// FRIENDLY: 1 static CrashWithThunk.main() + {{[0-9]+}} in CrashWithThunk at {{.*}}/CrashWithThunk.swift:26:9

// FRIENDLY: 24|     let foo = Foo(value: crash)
// FRIENDLY-NEXT: 25|
// FRIENDLY-NEXT: 26|     foo.value()
// FRIENDLY-NEXT:   |         ^
// FRIENDLY-NEXT: 27|   }
// FRIENDLY-NEXT: 28| }
