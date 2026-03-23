// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import Swift
import StdlibUnittest

let CopySubclassesTests = TestSuite("CopySubclasses")

// Set up a class hierarchy for testing.
class Base {}
class Sub1: Base {}
class Sub2: Base {}
class GrandChild: Sub1 {}
class Unrelated {}

class GenericBase<T> {}
class NongenericSub: GenericBase<Int> {}

class NongenericBase {}
class GenericSub<T>: NongenericBase {}

class NongenericWithGenericIntermediate: GenericSub<Int> {}

func objectIdentifierSet(_ types: [Any.Type]) -> Set<ObjectIdentifier> {
  Set(types.map{ ObjectIdentifier($0) })
}

@_silgen_name("_swift_copyNongenericSubclasses")
func _swift_copyNongenericSubclasses(
  _ superclass: UnsafeRawPointer
) -> UnsafePointer<UnsafeRawPointer?>

func copySubclasses(of type: AnyClass) -> Set<ObjectIdentifier> {
  let metadataPtr = unsafeBitCast(type, to: UnsafeRawPointer.self)
  let result = _swift_copyNongenericSubclasses(metadataPtr)
  var classes: [AnyClass] = []
  var i = 0
  while let ptr = result[i] {
    classes.append(unsafeBitCast(ptr, to: AnyClass.self))
    i += 1
  }
  result.deallocate()
  return objectIdentifierSet(classes)
}

CopySubclassesTests.test("direct subclasses") {
  let subclasses = copySubclasses(of: Base.self)
  expectEqual(subclasses, objectIdentifierSet([
      Sub1.self, Sub2.self, GrandChild.self
  ]))
}

CopySubclassesTests.test("transitive subclasses") {
  let subclasses = copySubclasses(of: Sub1.self)
  expectEqual(subclasses, objectIdentifierSet([
      GrandChild.self
  ]))
}

CopySubclassesTests.test("leaf class has no subclasses") {
  let subclasses = copySubclasses(of: GrandChild.self)
  expectEqual(subclasses, [])
}

CopySubclassesTests.test("unrelated class has no subclasses") {
  let subclasses = copySubclasses(of: Unrelated.self)
  expectEqual(subclasses, [])
}

CopySubclassesTests.test("generic superclass provides no subclasses") {
  let subclasses = copySubclasses(of: GenericBase<Int>.self)
  expectEqual(subclasses, [])
}

CopySubclassesTests.test("generic subclasses") {
  // A generic subclass of a non-generic class should not be returned. A
  // concrete subclass of a generic subclass of a non-generic class should also
  // not be returned.
  let subclasses = copySubclasses(of: NongenericBase.self)
  expectEqual(subclasses, [])
}

runAllTests()
