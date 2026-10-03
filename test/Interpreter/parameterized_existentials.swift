// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking)
// REQUIRES: executable_test

// This test requires the new existential shape metadata accessors which are 
// not available in on-device runtimes, or in the back-deployment runtime.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

var ParameterizedProtocolsTestSuite = TestSuite("ParameterizedProtocols")

protocol Holder<T> {
  associatedtype T
  var value: T { get }
}

struct IntHolder: Holder {
  var value: Int
}

struct GenericHolder<T>: Holder {
  var value: T
}

ParameterizedProtocolsTestSuite.test("basic") {
  let x: any Holder<Int> = IntHolder(value: 5)
  expectEqual(5, x.value)
}

func staticType<T>(of value: inout T) -> Any.Type {
  return T.self
}

func staticTypeForHolders<T>(of value: inout any Holder<T>) -> Any.Type {
  return (any Holder<T>).self
}

ParameterizedProtocolsTestSuite.test("metadataEquality") {
  var x: any Holder<Int> = IntHolder(value: 5)
  var typeOne = staticType(of: &x)
  var typeTwo = staticTypeForHolders(of: &x)
  expectEqual(typeOne, typeTwo)
}

ParameterizedProtocolsTestSuite.test("casting") {
  let a = GenericHolder(value: 5) as any Holder<Int>
  let b = GenericHolder(value: 5) as! any Holder<Int>
  expectEqual(a.value, b.value)
}

// rdar://96571508
struct ErasingHolder<T> {
  let box: any Holder<T>
}
ParameterizedProtocolsTestSuite.test("casting") {
  let a = ErasingHolder(box: IntHolder(value: 5))
  expectEqual(a.box.value, 5)
}

final class SuperclassLifetimeCounter {
  var destructions = 0
}

class HashableSuperclass<T: Hashable> {
  let value: T
  let lifetime: SuperclassLifetimeCounter?

  init(_ value: T, lifetime: SuperclassLifetimeCounter? = nil) {
    self.value = value
    self.lifetime = lifetime
  }

  deinit { lifetime?.destructions += 1 }
}

protocol OtherSuperclassProtocol {
  var otherValue: Int { get }
}

final class HashableSuperclassHolder<T: Hashable>:
  HashableSuperclass<T>, Holder, OtherSuperclassProtocol {
  var otherValue: Int { 84 }
}

typealias HashableIntHolder = HashableSuperclass<Int> & Holder<Int>

@inline(never)
func genericLayout<T>(_ type: T.Type) -> (Int, Int, Int) {
  return (MemoryLayout<T>.size, MemoryLayout<T>.stride,
          MemoryLayout<T>.alignment)
}

ParameterizedProtocolsTestSuite.test("genericSuperclassLayout") {
  let type: Any.Type = (any HashableIntHolder).self
  let layout = _openExistential(type, do: genericLayout)
  expectEqual(MemoryLayout<any HashableIntHolder>.size, layout.0)
  expectEqual(MemoryLayout<any HashableIntHolder>.stride, layout.1)
  expectEqual(MemoryLayout<any HashableIntHolder>.alignment, layout.2)

  let metatype: Any.Type = (any HashableIntHolder.Type).self
  let metatypeLayout = _openExistential(metatype, do: genericLayout)
  expectEqual(MemoryLayout<any HashableIntHolder.Type>.size, metatypeLayout.0)
  expectEqual(MemoryLayout<any HashableIntHolder.Type>.stride, metatypeLayout.1)
}

ParameterizedProtocolsTestSuite.test("genericSuperclassCasting") {
  let value: Any = HashableSuperclassHolder(42)
  expectTrue(value is any HashableIntHolder)
  expectFalse(value is any HashableSuperclass<String> & Holder<String>)
  expectEqual(42, (value as? any HashableIntHolder)?.value)
  expectNil(value as? any HashableSuperclass<String> & Holder<String>)
  expectNil(value as? any HashableSuperclass<Int> & Holder<String>)
  expectNil(HashableSuperclass(42) as? any HashableIntHolder)

  let composed = value as? any HashableIntHolder & OtherSuperclassProtocol
  expectEqual(42, composed?.value)
  expectEqual(84, composed?.otherValue)

  let type: Any = HashableSuperclassHolder<Int>.self
  expectTrue(type is any HashableIntHolder.Type)
  let castType = type as? any HashableIntHolder.Type
  expectNotNil(castType)
  expectEqual(ObjectIdentifier(HashableSuperclassHolder<Int>.self),
              ObjectIdentifier(castType!))
}

class CollectionSuperclass<C: Collection> where C.Element: Hashable {
  let elements: C
  init(_ elements: C) { self.elements = elements }
}

final class CollectionSuperclassHolder<C: Collection>:
  CollectionSuperclass<C>, Holder where C.Element: Hashable {
  var value: C.Element { elements.first! }
}

ParameterizedProtocolsTestSuite.test("genericSuperclassAssociatedConformance") {
  typealias Value = CollectionSuperclass<[Int]> & Holder<Int>
  let type: Any.Type = (any Value).self
  let layout = _openExistential(type, do: genericLayout)
  expectEqual(MemoryLayout<any Value>.size, layout.0)
  expectEqual(MemoryLayout<any Value>.stride, layout.1)

  let erased: Any = CollectionSuperclassHolder([42])
  expectEqual(42, (erased as? any Value)?.value)
  expectNil(erased as? any CollectionSuperclass<[String]> & Holder<String>)
}

ParameterizedProtocolsTestSuite.test("genericSuperclassCopying") {
  let lifetime = SuperclassLifetimeCounter()
  do {
    let value = HashableSuperclassHolder(42, lifetime: lifetime)
    var copies: [any HashableIntHolder] = [value, value]
    expectEqual(42, copies[0].value)
    copies.removeFirst()
    expectEqual(42, copies[0].value)
    withExtendedLifetime(copies) {
      expectEqual(0, lifetime.destructions)
    }
  }
  expectEqual(1, lifetime.destructions)
}

protocol LeftHolder<Element> {
  associatedtype Element
  var left: Element { get }
}

protocol RightHolder<Element> {
  associatedtype Element
  var right: Element { get }
}

final class SharedElementHolder<Element>: LeftHolder, RightHolder {
  let left: Element
  let right: Element
  init(_ left: Element, _ right: Element) {
    self.left = left
    self.right = right
  }
}

final class HashableSharedElementHolder:
  HashableSuperclass<Int>, LeftHolder, RightHolder {
  var left: Int { value }
  var right: Int { 84 }
}

ParameterizedProtocolsTestSuite.test("sharedAssociatedTypeLayoutAndCasting") {
  typealias Value = LeftHolder<Int> & RightHolder<Int>
  typealias ClassValue = LeftHolder<Int> & RightHolder<Int> & AnyObject
  let type: Any.Type = (any Value).self
  let layout = _openExistential(type, do: genericLayout)
  expectEqual(MemoryLayout<any Value>.size, layout.0)
  expectEqual(MemoryLayout<any Value>.stride, layout.1)

  let classType: Any.Type = (any ClassValue).self
  let classLayout = _openExistential(classType, do: genericLayout)
  expectEqual(MemoryLayout<any ClassValue>.size, classLayout.0)
  expectEqual(MemoryLayout<any ClassValue>.stride, classLayout.1)

  let metatype: Any.Type = (any ClassValue.Type).self
  let metatypeLayout = _openExistential(metatype, do: genericLayout)
  expectEqual(MemoryLayout<any ClassValue.Type>.size, metatypeLayout.0)

  let erased: Any = SharedElementHolder(42, 84)
  let value = erased as? any Value
  expectEqual(42, value?.left)
  expectEqual(84, value?.right)
  let classValue = erased as? any ClassValue
  expectEqual(42, classValue?.left)
  expectEqual(84, classValue?.right)
  expectNil(erased as? any LeftHolder<String> & RightHolder<String>)

  let erasedType: Any = SharedElementHolder<Int>.self
  let castType = erasedType as? any ClassValue.Type
  expectNotNil(castType)
  expectEqual(ObjectIdentifier(SharedElementHolder<Int>.self),
              ObjectIdentifier(castType!))

  typealias SuperclassValue =
    HashableSuperclass<Int> & LeftHolder<Int> & RightHolder<Int>
  let superclassType: Any.Type = (any SuperclassValue).self
  let superclassLayout = _openExistential(superclassType, do: genericLayout)
  expectEqual(MemoryLayout<any SuperclassValue>.size, superclassLayout.0)
  expectEqual(MemoryLayout<any SuperclassValue>.stride, superclassLayout.1)

  let superclassErased: Any = HashableSharedElementHolder(42)
  let superclassValue = superclassErased as? any SuperclassValue
  expectEqual(42, superclassValue?.left)
  expectEqual(84, superclassValue?.right)
  expectNil(superclassErased as?
            any HashableSuperclass<Int> & LeftHolder<String> & RightHolder<String>)
}

runAllTests()
