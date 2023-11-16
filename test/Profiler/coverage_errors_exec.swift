// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -profile-generate -profile-coverage-mapping -o %t/main

// This unusual use of 'sh' allows the path of the profraw file to be
// substituted by %target-run.
// RUN: %target-codesign %t/main
// RUN: %target-run sh -c 'env LLVM_PROFILE_FILE=$1 $2' -- %t/default.profraw %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %llvm-cov show %t/main -instr-profile=%t/default.profdata | %FileCheck %s

// REQUIRES: profile_runtime
// REQUIRES: executable_test
// REQUIRES: OS=macosx

struct Err: Error {}

struct S {
  static subscript() -> Int {
    get throws { throw Err() }
  }
  func throwingMethod() throws -> Int { throw Err() }
}

func noThrowingFn() throws -> Int { 0 }

@discardableResult
func throwingFn() throws -> Int { throw Err() }

var throwingProp: Int {
  get throws { throw Err() }
}

var throwingS: S {
  get throws { throw Err() }
}

var noThrowingS: S {
  get throws { S() }
}

func test1() -> Int {        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  do {                       // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    let x = try throwingFn() // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return x                 // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  } catch {                  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 0                 // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = test1()                  // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test2() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = try throwingFn()   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                   // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test2()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test3() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = try throwingProp   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                   // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test3()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test4() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = try S[]            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                   // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test4()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

// Note we don't emit a region after the call since it would be empty.
func test5() -> Int {        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  do {                       // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return try throwingFn()  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  } catch {                  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 0                 // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = test5()                  // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func takesInts(_ x: Int, _ y: Int) {} // CHECK: {{ *}}[[@LINE]]|{{ *}}0

// The throwing expr is nested here, the region starts after the throwing expr.
func test6() throws {            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  takesInts(try throwingFn(), 0) // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                                // CHECK: {{ *}}[[@LINE]]|{{ *}}0
try? test6()                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test7() throws {            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  takesInts(                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn(),            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    0                            // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  )                              // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                                // CHECK: {{ *}}[[@LINE]]|{{ *}}0
try? test7()                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test8() throws -> Int {             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = try throwingS.throwingMethod() // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                               // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                                        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test8()

func test9() throws -> Int {               // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = try noThrowingS.throwingMethod() // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                                 // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                                          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test9()                           // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test10() throws { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  takesInts(           // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn(),  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()   // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  )                    // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                      // CHECK: {{ *}}[[@LINE]]|{{ *}}0
try? test10()          // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test11() throws {  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  takesInts(            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try noThrowingFn(), // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  )                     // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                       // CHECK: {{ *}}[[@LINE]]|{{ *}}0
try? test11()           // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test21() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  try {                       // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    throw Err()               // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }()                         // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return 1                    // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test21()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

// rdar://100470244 - Make sure we don't underflow the counter here.
func test12() -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  do {                 // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 1           // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  } catch {            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 2           // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                      // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = test12()           // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test13() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  x: do {                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  } catch is Err {            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    break x                   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                           // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return 1                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test13()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test14() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  x: do {                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    do {                      // CHECK: {{ *}}[[@LINE]]|{{ *}}1
      try throwingFn()        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
      break x                 // CHECK: {{ *}}[[@LINE]]|{{ *}}0
    } catch is Err {          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    }                         // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 1                  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  } catch is Err {            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                           // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  return 2                    // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test14()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test15() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  x: do {                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    do {                      // CHECK: {{ *}}[[@LINE]]|{{ *}}1
      try noThrowingFn()      // CHECK: {{ *}}[[@LINE]]|{{ *}}1
      break x                 // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    } catch is Err {          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    }                         // CHECK: {{ *}}[[@LINE]]|{{ *}}0
    return 1                  // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  } catch is Err {            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
                              // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  }                           // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return 2                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test15()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
