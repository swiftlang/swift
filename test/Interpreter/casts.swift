// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

protocol P : class { }
protocol C : class { }

class Foo : NSObject {}
var Casts = TestSuite("Casts")


@inline(never)
func castit<ObjectType>(_ o: NSObject?, _ t: ObjectType.Type) -> ObjectType? {
  return o as? ObjectType
}

@inline(never)
func castitExistential<ObjectType>(_ o: C?, _ t: ObjectType.Type) -> ObjectType? {
  return o as? ObjectType
}

Casts.test("cast optional<nsobject> to protocol") {
  if let obj = castit(nil, P.self) {
    print("fail")
    expectUnreachable()
  } else {
    print("success")
  }
}

Casts.test("cast optional<nsobject> to protocol meta") {
  if let obj = castit(nil, P.Type.self) {
    print("fail")
    expectUnreachable()
  } else {
    print("success")
  }
}
Casts.test("cast optional<protocol> to protocol") {
  if let obj = castitExistential(nil, P.self) {
    print("fail")
    expectUnreachable()
  } else {
    print("success")
  }
}

Casts.test("cast optional<protocol> to class") {
  if let obj = castitExistential(nil, Foo.self) {
    print("fail")
    expectUnreachable()
  } else {
    print("success")
  }
}

Casts.test("cast optional<protocol> to protocol meta") {
  if let obj = castitExistential(nil, P.Type.self) {
    expectUnreachable()
    print("fail")
  } else {
    print("success")
  }
}

Casts.test("cast optional<protocol> to class meta") {
  if let obj = castitExistential(nil, Foo.Type.self) {
    expectUnreachable()
    print("fail")
  } else {
    print("success")
  }
}

runAllTests()
