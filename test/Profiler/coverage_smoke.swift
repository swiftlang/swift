// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -profile-generate -profile-coverage-mapping -Xfrontend -disable-incremental-llvm-codegen -o %t/main

// This unusual use of 'sh' allows the path of the profraw file to be
// substituted by %target-run.
// RUN: %target-run sh -c 'env LLVM_PROFILE_FILE=$1 $2' -- %t/default.profraw %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %llvm-profdata show %t/default.profdata -function=f_internal | %FileCheck %s --check-prefix=CHECK-INTERNAL
// RUN: %llvm-profdata show %t/default.profdata -function=f_private | %FileCheck %s --check-prefix=CHECK-PRIVATE
// RUN: %llvm-profdata show %t/default.profdata -function=f_public | %FileCheck %s --check-prefix=CHECK-PUBLIC
// RUN: %llvm-profdata show %t/default.profdata -function=main | %FileCheck %s --check-prefix=CHECK-MAIN
// RUN: %llvm-cov show %t/main -instr-profile=%t/default.profdata | %FileCheck %s --check-prefix=CHECK-COV
// RUN: %llvm-cov report %t/main -instr-profile=%t/default.profdata -show-functions %s | %FileCheck %s --check-prefix=CHECK-REPORT
// RUN: rm -rf %t

// REQUIRES: profile_runtime
// REQUIRES: executable_test
// REQUIRES: OS=macosx

// CHECK-INTERNAL: Functions shown: 1
// CHECK-COV: {{ *}}[[@LINE+1]]|{{ *}}1{{.*}}func f_internal
internal func f_internal() {}

// CHECK-PRIVATE: Functions shown: 1
// CHECK-COV: {{ *}}[[@LINE+1]]|{{ *}}1{{.*}}func f_private
private func f_private() { f_internal() }

// CHECK-PUBLIC: Functions shown: 1
// CHECK-COV: {{ *}}[[@LINE+1]]|{{ *}}1{{.*}}func f_public
public func f_public() { f_private() }

class Class1 {
  var Field1 = 0

// CHECK-COV: {{ *}}[[@LINE+1]]|{{ *}}1{{.*}}init
  init() {}

// CHECK-COV: {{ *}}[[@LINE+1]]|{{ *}}1{{.*}}deinit
  deinit {}
}

// CHECK-MAIN: Maximum function count: 4
func main() {
// CHECK-COV: {{ *}}[[@LINE+1]]|{{ *}}1{{.*}}f_public
  f_public()

// CHECK-COV: {{ *}}[[@LINE+1]]|{{ *}}1{{.*}}if (true)
  if (true) {}

  var x : Int32 = 0
  while (x < 10) {
// CHECK-COV: {{ *}}[[@LINE+1]]|{{ *}}10{{.*}}x += 1
    x += 1
  }

// CHECK-COV: {{ *}}[[@LINE+1]]|{{ *}}1{{.*}}Class1
  let _ = Class1()
}

// rdar://problem/22761498 - enum declaration suppresses coverage
func foo() {
  var x : Int32 = 0   // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  enum ETy { case A } // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  repeat {            // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
    x += 1            // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  } while x == 0      // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  x += 1              // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
}

// rdar://problem/27874041 - top level code decls get no coverage
var g1 : Int32 = 0   // CHECK-COV: {{ *}}[[@LINE]]|
repeat {             // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  g1 += 1            // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
} while g1 == 0      // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1

func call_closure() { // CHECK-COV: {{ *}}[[@LINE]]|
  var x : Int32 = 0   // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  ({ () -> () in      // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
    x += 1            // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  })()                // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
}

func call_auto_closure() {
  func use_auto_closure(_ x: @autoclosure () -> Bool) -> Bool {
    return x() && // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
           x() && // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
           x()    // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  }
  let _ = use_auto_closure(true) // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}3
}

class Class2 {
  var field: Int
  init(field: Int) {
    if field > 0 {
      self.field = 0 // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
    } else {
      self.field = 1 // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}3
    }
  }
}

extension Class2 {
  convenience init() {
    self.init(field: 0) // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  }
}

class SubClass1: Class2 {
  override init(field: Int) {
    super.init(field: field) // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  }
}

struct Struct1 {
  var field: Int
  init(field: Int) {
    if field > 0 {
      self.field = 0 // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
    } else {
      self.field = 1 // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
    }
  }
}

extension Struct1 {
  init() {
    self.init(field: 0) // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  }
}

var g2: Int = 0

class Class3 {
  var m1 = g2 == 0
             ? "false" // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
             : "true"; // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
}

// rdar://34244637: Wrong coverage for do/catch sequence
enum CustomError : Error {
  case Err
}
func throwError(_ b: Bool) throws {
  if b {
    throw CustomError.Err
  }
}
func catchError(_ b: Bool) -> Int {
  do {
    try throwError(b) // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}2
  } catch {           // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}2
    return 1          // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  }                   // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  let _ = 1 + 1       // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  return 0            // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
}
let _ = catchError(true)
let _ = catchError(false)

func catchError2(_ b: Bool) -> Int {
  do {
    throw CustomError.Err // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}2
  } catch {
    if b {                // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}2
      return 1            // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
    }
  }
  return 0                // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
}
let _ = catchError2(true)
let _ = catchError2(false)

main() // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
foo()  // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
call_closure()
call_auto_closure()

let _ = Class2(field: 0)
let _ = Class2(field: 1)
let _ = Class2()
let _ = SubClass1(field: 0)

let _ = Class3()
g2 = 1
let _ = Class3()

let _ = Struct1(field: 1)
let _ = Struct1()

// CHECK-REPORT: TOTAL {{.*}} 100.00% {{.*}} 100.00%
