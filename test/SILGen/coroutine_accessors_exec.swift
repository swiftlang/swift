// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out \
// RUN:    -enable-experimental-feature CoroutineAccessors
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// Test again with the `CoroutineAccessorsUnwindOnCallerError`.
// This should have no effect.  It was put in before we decided on
// the final error behavior and never really did anything.

// RUN: %target-build-swift %s -o %t/a-unwind.out \
// RUN:    -enable-experimental-feature CoroutineAccessors \
// RUN:    -enable-experimental-feature CoroutineAccessorsUnwindOnCallerError
// RUN: %target-codesign %t/a-unwind.out
// RUN: %target-run %t/a-unwind.out | %FileCheck %s

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: swift_feature_CoroutineAccessorsUnwindOnCallerError
// REQUIRES: executable_test

struct NCInt: ~Copyable {
  var value: Int = 0
}

protocol P: ~Copyable {
  var i: NCInt { yielding borrow set }
}

struct S: ~Copyable, P {
  private var _i: NCInt
  var i: NCInt {
    yielding borrow {
      print("Yielding Borrow: Before")
      yield _i
      print("Yielding Borrow: After")
    }
    yielding mutate {
      print("Yielding Mutate: Before")
      yield &_i
      print("Yielding Mutate: After")
    }
  }
  public init() { _i = NCInt() }
}

// Exercise basic calling with no error handling
// Check that coroutine runs both before and after
// the access.
func basic() {
  var s = S()

  print("Basic Test 1")
  print("\(s.i.value)")
  // CHECK: Basic Test 1
  // CHECK-NEXT: Yielding Borrow: Before
  // CHECK-NEXT: Yielding Borrow: After
  // CHECK-NEXT: 0

  print("Basic Test 2")
  s.i.value += 1
  // CHECK: Basic Test 2
  // CHECK-NEXT: Yielding Mutate: Before
  // CHECK-NEXT: Yielding Mutate: After

  print("Basic Test 3")
  print("\(s.i.value)")
  // CHECK: Basic Test 3
  // CHECK-NEXT: Yielding Borrow: Before
  // CHECK-NEXT: Yielding Borrow: After
  // CHECK-NEXT: 1

  print("Basic Test Done")
  // CHECK: Basic Test Done
}

enum MyError: Error {
  case failure
}

func throwingReader(_ value: borrowing NCInt) throws -> Int {
  print("Reader:\(value.value)")
  throw MyError.failure
  // return value // Unreachable
}

func throwingUpdater(_ update: inout NCInt) throws -> Int {
  update.value += 1
  print("Updated:\(update.value)")
  throw MyError.failure
  // return update // Unreachable
}

// If an error is thrown during the access, the
// access must still be completed.
func continueAfterThrow() {
  var s = S()
  print("Not Basic Test 1")
  do {
    _ = try throwingReader(s.i)
  } catch {
    print("Caught!")
  }
  // CHECK: Not Basic Test 1
  // CHECK-NEXT: Yielding Borrow: Before
  // CHECK-NEXT: Reader:0
  // ... throw happens here
  // CHECK-NEXT: Yielding Borrow: After
  // CHECK-NEXT: Caught!

  print("Not Basic Test 2")
  do {
    _ = try throwingUpdater(&s.i)
  } catch {
    print("Caught!")
  }
  // CHECK: Not Basic Test 2
  // CHECK-NEXT: Yielding Mutate: Before
  // CHECK-NEXT: Updated:1
  // ... throw happens here
  // CHECK-NEXT: Yielding Mutate: After
  // CHECK-NEXT: Caught!
}

// Same as `notBasic()`, but all the accesses
// go through a protocol existential.
func viaProtocol() {
  var p: any P & ~Copyable = S()

  print("Protocol Test 1")
  do {
    _ = try throwingReader(p.i)
  } catch {
    print("Caught!")
  }
  // CHECK: Protocol Test 1
  // CHECK-NEXT: Yielding Borrow: Before
  // CHECK-NEXT: Reader:0
  // ... throw happens here
  // CHECK-NEXT: Yielding Borrow: After
  // CHECK-NEXT: Caught!

  // Note: The protocol specifies `set`, but
  // the standard witness table also exports
  // coroutine accessors for `set`, so the
  // following does in fact use the coroutine accessor...
  print("Protocol Test 2")
  do {
    _ = try throwingUpdater(&p.i)
  } catch {
    print("Caught!")
  }
  // CHECK: Protocol Test 2
  // CHECK-NEXT: Yielding Mutate: Before
  // CHECK-NEXT: Updated:1
  // ... throw happens here
  // CHECK-NEXT: Yielding Mutate: After
  // CHECK-NEXT: Caught!
}


basic()
continueAfterThrow()
viaProtocol()
