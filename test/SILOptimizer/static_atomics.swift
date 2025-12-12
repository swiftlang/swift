// RUN: %target-build-swift -parse-as-library -Xfrontend -disable-availability-checking -O %s -module-name=test -emit-sil | %FileCheck %s

// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -parse-as-library -Xfrontend -disable-availability-checking -O -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// REQUIRES: executable_test,swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: synchronization
// UNSUPPORTED: back_deployment_runtime

import Synchronization


struct TwoAtomics: ~Copyable {
  let a: Atomic<UInt8>
  let b: Atomic<UInt8>
}

// CHECK-LABEL: sil_global hidden @$s4test7atomic1AA10TwoAtomicsVSgvp : $Optional<TwoAtomics> = {
var atomic1: TwoAtomics? = nil

// CHECK-LABEL: sil_global hidden @$s4test7atomic2AA10TwoAtomicsVvp : $TwoAtomics = {
let atomic2: TwoAtomics = TwoAtomics(a: Atomic(29), b: Atomic(30))

// TODO: this is not initialized statically, because missing IRGen support
var atomic3: TwoAtomics? = TwoAtomics(a: Atomic(27), b: Atomic(28))

// CHECK-LABEL: sil_global hidden @$s4test5mutex15Synchronization5MutexVySiGvp : $Mutex<Int> = {
let mutex = Mutex<Int>(123)

@main
struct Main {
  static func main() {

    precondition(atomic1 == nil)

    // CHECK-OUTPUT: atomic2: 29 30
    print("atomic2:", atomic2.a.load(ordering: .relaxed), atomic2.b.load(ordering: .relaxed))

    // CHECK-OUTPUT: atomic3: 27 28
    switch atomic3 {
    case .some(let a):
      print("atomic3:", a.a.load(ordering: .relaxed), a.b.load(ordering: .relaxed))
    case .none:
      break
    }

    atomic1 = TwoAtomics(a: Atomic(1), b: Atomic(2))

    // CHECK-OUTPUT: atomic2: 29 30
    print("atomic2:", atomic2.a.load(ordering: .relaxed), atomic2.b.load(ordering: .relaxed))


    mutex.withLock {
      $0 = $0 + 1
    }

    // CHECK-OUTPUT: mutex: 124
    mutex.withLock {
      print("mutex:", $0)
    }
  }
}


