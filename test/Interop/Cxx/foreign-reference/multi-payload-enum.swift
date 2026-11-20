// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -Onone)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -O)

// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Enums with foreign reference type payloads must call each payload's own
// retain/release functions when copied or destroyed.

import MultiPayloadEnum
import StdlibUnittest

struct WrappedA { var a: A }

enum Either { case a(A), b(B) }
enum EitherOrNone { case a(A), b(B), none }
enum SameType { case x(A), y(A) }
enum WrappedEither { case a(WrappedA), b(B) }
enum Generic<T, U> { case t(T), u(U) }
enum SinglePayload { case a(A), c, d }

@inline(never)
func copyAndDrop<T>(_ value: T) {
  let copies = [value, value, value]
  let boxed: [Any] = copies
  _ = boxed
}

@inline(never)
func copyAndDropConcrete(_ value: Either) {
  let copies = [value, value, value]
  _ = copies
}

var EnumTestSuite = TestSuite("EnumsWithFRTPayloads")

EnumTestSuite.test("two FRT payloads") {
  let a = makeA()!
  let b = makeB()!
  copyAndDropConcrete(.a(a))
  copyAndDropConcrete(.b(b))
  copyAndDrop(Either.a(a))
  copyAndDrop(Either.b(b))
  expectEqual(a.refCount, 1)
  expectEqual(b.refCount, 1)
}

EnumTestSuite.test("two FRT payloads and an empty case") {
  let a = makeA()!
  let b = makeB()!
  copyAndDrop(EitherOrNone.a(a))
  copyAndDrop(EitherOrNone.b(b))
  copyAndDrop(EitherOrNone.none)
  expectEqual(a.refCount, 1)
  expectEqual(b.refCount, 1)
}

EnumTestSuite.test("same FRT in two payloads") {
  let a = makeA()!
  copyAndDrop(SameType.x(a))
  copyAndDrop(SameType.y(a))
  expectEqual(a.refCount, 1)
}

EnumTestSuite.test("FRT wrapped in a struct") {
  let a = makeA()!
  let b = makeB()!
  copyAndDrop(WrappedEither.a(WrappedA(a: a)))
  copyAndDrop(WrappedEither.b(b))
  expectEqual(a.refCount, 1)
  expectEqual(b.refCount, 1)
}

EnumTestSuite.test("generic enum instantiated with FRTs") {
  let a = makeA()!
  let b = makeB()!
  copyAndDrop(Generic<A, B>.t(a))
  copyAndDrop(Generic<A, B>.u(b))
  expectEqual(a.refCount, 1)
  expectEqual(b.refCount, 1)
}

EnumTestSuite.test("single FRT payload and two empty cases") {
  let a = makeA()!
  copyAndDrop(SinglePayload.a(a))
  copyAndDrop(SinglePayload.c)
  expectEqual(a.refCount, 1)
}

runAllTests()
