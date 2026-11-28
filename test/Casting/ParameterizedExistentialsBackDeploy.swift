// Test that parameterized existentials, including compositions with a generic
// superclass and existentials that suppress `Copyable` or `Escapable`, work on
// every runtime that supports them.

// RUN: %empty-directory(%t)
//
// RUN: %target-build-swift -target %target-swift-5.7-abi-triple -swift-version 5 -g -Onone -module-name a %s -o %t/a.Onone.out
// RUN: %target-codesign %t/a.Onone.out
// RUN: %target-run %t/a.Onone.out
//
// RUN: %target-build-swift -target %target-swift-5.7-abi-triple -swift-version 5 -g -O -module-name a %s -o %t/a.O.out
// RUN: %target-codesign %t/a.O.out
// RUN: %target-run %t/a.O.out
//
// RUN: %target-build-swift -target %target-swift-6.0-abi-triple -swift-version 5 -g -Onone -module-name a %s -o %t/a.6.0.Onone.out -D TARGET_SWIFT_6_0
// RUN: %target-codesign %t/a.6.0.Onone.out
// RUN: %target-run %t/a.6.0.Onone.out
//
// REQUIRES: executable_test

import StdlibUnittest

protocol Holder<T> {
  associatedtype T
}

final class Object {}

class HashableBase<Value: Hashable> {
  let value: Value
  init(_ value: Value) { self.value = value }
}

final class HashableDerived<Value: Hashable>: HashableBase<Value>, Holder {
  typealias T = Value
}

class ObjectBase<Value: AnyObject> {
  let value: Int
  init(_ value: Int) { self.value = value }
}

final class ObjectDerived<Value: AnyObject>: ObjectBase<Value>, Holder {
  typealias T = Int
}

class NoncopyableArgBase<Value: ~Copyable> {
  let value: Int
  init(_ value: Int) { self.value = value }
}

final class NoncopyableArgDerived<Value: ~Copyable>: NoncopyableArgBase<Value>, Holder {
  typealias T = Int
}

class ObjectNoncopyableArgBase<Value: AnyObject, Other: ~Copyable> {
  let value: Int
  init(_ value: Int) { self.value = value }
}

final class ObjectNoncopyableArgDerived<Value: AnyObject, Other: ~Copyable>: ObjectNoncopyableArgBase<Value, Other>, Holder {
  typealias T = Int
}

class HashableNoncopyableArgBase<Value: Hashable, Other: ~Copyable> {
  let value: Value
  init(_ value: Value) { self.value = value }
}

final class HashableNoncopyableArgDerived<Value: Hashable, Other: ~Copyable>: HashableNoncopyableArgBase<Value, Other>, Holder {
  typealias T = Value
}

struct GenericHolder<T>: Holder {}

class AssocHashableBase<U: Holder> where U.T: Hashable {
  let value: U.T
  init(_ value: U.T) { self.value = value }
}

final class AssocHashableDerived<U: Holder>: AssocHashableBase<U>, Holder where U.T: Hashable {
  typealias T = U.T
}

protocol CopyableCheck {}
struct CopyableBox<T: ~Copyable & ~Escapable> {}
extension CopyableBox: CopyableCheck where T: Copyable, T: ~Escapable {}

protocol EscapableCheck {}
struct EscapableBox<T: ~Copyable & ~Escapable> {}
extension EscapableBox: EscapableCheck where T: Escapable, T: ~Copyable {}

protocol Resource<T>: ~Copyable, ~Escapable {
  associatedtype T: ~Copyable
}

struct NoncopyableArg: ~Copyable {}

struct CopyableResource: Resource { typealias T = Int }
struct NoncopyableResource: Resource, ~Copyable { typealias T = Int }
enum NonescapableResource: Resource, ~Escapable { typealias T = Int }
enum NeitherResource: Resource, ~Copyable, ~Escapable { typealias T = Int }
struct CopyableResourceOfNoncopyable: Resource { typealias T = NoncopyableArg }
struct NoncopyableResourceOfNoncopyable: Resource, ~Copyable {
  typealias T = NoncopyableArg
}

typealias AnyMetatype = any (~Copyable & ~Escapable).Type

func resourceCasts(_ type: AnyMetatype) -> [Bool] {
  [type is any Resource<Int>.Type,
   type is any (Resource<Int> & ~Copyable).Type,
   type is any (Resource<Int> & ~Escapable).Type,
   type is any (Resource<Int> & ~Copyable & ~Escapable).Type]
}

func noncopyableArgResourceCasts(_ type: AnyMetatype) -> [Bool] {
  [type is any Resource<NoncopyableArg>.Type,
   type is any (Resource<NoncopyableArg> & ~Copyable).Type]
}

@available(SwiftStdlib 6.0, *)
func isCopyable<T: ~Copyable & ~Escapable>(_: T.Type) -> Bool {
  CopyableBox<T>() as Any is any CopyableCheck
}

@available(SwiftStdlib 6.0, *)
func isEscapable<T: ~Copyable & ~Escapable>(_: T.Type) -> Bool {
  EscapableBox<T>() as Any is any EscapableCheck
}

func genericSize<T>(_: T.Type) -> Int {
  MemoryLayout<T>.size
}

let tests = TestSuite("ParameterizedExistentialsBackDeploy")

if #available(SwiftStdlib 5.7, *) {
  tests.test("Generalization signature with a protocol requirement") {
    typealias E = any HashableBase<Int> & Holder<Int>
    let type: Any.Type = E.self
    expectEqual(MemoryLayout<E>.size, _openExistential(type, do: genericSize))

    let array: [E] = [HashableDerived(1), HashableDerived(2)]
    let erased: Any = array
    let cast = erased as? [E]
    expectNotNil(cast)
    expectEqual(2, cast!.count)
    expectEqual(2, cast![1].value)
  }

  tests.test("Generalization signature with a layout requirement") {
    typealias E = any ObjectBase<Object> & Holder<Int>
    let type: Any.Type = E.self
    expectEqual(MemoryLayout<E>.size, _openExistential(type, do: genericSize))

    let erased: Any = ObjectDerived<Object>(3)
    let cast = erased as? E
    expectNotNil(cast)
    expectEqual(3, cast!.value)
  }
}

if #available(SwiftStdlib 6.0, *) {
  tests.test("Generalized superclass with a noncopyable parameter") {
    typealias E = any NoncopyableArgBase<Int> & Holder<Int>
    let type: Any.Type = E.self
    expectEqual(MemoryLayout<E>.size, _openExistential(type, do: genericSize))

    let array: [E] = [NoncopyableArgDerived<Int>(5)]
    let erasedArray: Any = array
    expectEqual(1, (erasedArray as? [E])?.count)

    let box: Any = CopyableBox<E>()
    expectTrue(box is any CopyableCheck)
  }

  tests.test("Generalized superclass with layout and noncopyable parameters") {
    typealias E = any ObjectNoncopyableArgBase<Object, Int> & Holder<Int>
    let array: [E] = [ObjectNoncopyableArgDerived<Object, Int>(7)]
    let erasedArray: Any = array
    expectEqual(1, (erasedArray as? [E])?.count)
  }

// Below a Swift 6.0 deployment target, these types' metadata is referenced
// through accessor functions, and released runtimes call those with the wrong
// arguments while checking a cast's requirements.
#if TARGET_SWIFT_6_0
  tests.test("Cast to a generalized superclass with a noncopyable parameter") {
    let erased: Any = NoncopyableArgDerived<Int>(4)
    let cast = erased as? any NoncopyableArgBase<Int> & Holder<Int>
    expectNotNil(cast)
    expectEqual(4, cast!.value)

    let erased2: Any = ObjectNoncopyableArgDerived<Object, Int>(6)
    let cast2 = erased2 as? any ObjectNoncopyableArgBase<Object, Int> & Holder<Int>
    expectNotNil(cast2)
    expectEqual(6, cast2!.value)
  }
#endif

  tests.test("Self of a parameterized existential must be Copyable and Escapable unless suppressed") {
    expectEqual([true, true, true, true], resourceCasts(CopyableResource.self))
    expectEqual([false, true, false, true], resourceCasts(NoncopyableResource.self))
    expectEqual([false, false, true, true], resourceCasts(NonescapableResource.self))
    expectEqual([false, false, false, true], resourceCasts(NeitherResource.self))
    expectEqual([false, false, false, false],
                resourceCasts(CopyableResourceOfNoncopyable.self))
  }

  tests.test("Copyable and Escapable conformances of parameterized existentials") {
    expectTrue(isCopyable((any Resource<Int>).self))
    expectTrue(isEscapable((any Resource<Int>).self))
    expectFalse(isCopyable((any Resource<Int> & ~Copyable).self))
    expectTrue(isEscapable((any Resource<Int> & ~Copyable).self))
    expectTrue(isCopyable((any Resource<Int> & ~Escapable).self))
    expectFalse(isEscapable((any Resource<Int> & ~Escapable).self))
  }
}

if #available(SwiftStdlib 6.5, *) {
  tests.test("Cast to a generalization signature with a protocol requirement") {
    let erased: Any = HashableDerived(42)
    let cast = erased as? any HashableBase<Int> & Holder<Int>
    expectNotNil(cast)
    expectEqual(42, cast!.value)
    expectNil(erased as? any HashableBase<String> & Holder<String>)

    let erasedAssoc: Any = AssocHashableDerived<GenericHolder<Int>>(99)
    let castAssoc = erasedAssoc as? any AssocHashableBase<GenericHolder<Int>> & Holder<Int>
    expectNotNil(castAssoc)
    expectEqual(99, castAssoc!.value)

    let erasedNC: Any = HashableNoncopyableArgDerived<Int, Int>(123)
    let castNC = erasedNC as? any HashableNoncopyableArgBase<Int, Int> & Holder<Int>
    expectNotNil(castNC)
    expectEqual(123, castNC!.value)
  }

  tests.test("Cast to a generalized superclass with a noncopyable parameter, any deployment target") {
    let erased: Any = NoncopyableArgDerived<Int>(8)
    let cast = erased as? any NoncopyableArgBase<Int> & Holder<Int>
    expectNotNil(cast)
    expectEqual(8, cast!.value)
  }

  tests.test("Self of a parameterized existential with a noncopyable argument") {
    expectEqual([true, true],
                noncopyableArgResourceCasts(CopyableResourceOfNoncopyable.self))
    expectEqual([false, true],
                noncopyableArgResourceCasts(NoncopyableResourceOfNoncopyable.self))
    expectEqual([false, false],
                noncopyableArgResourceCasts(CopyableResource.self))

    expectTrue(isCopyable((any Resource<NoncopyableArg>).self))
    expectFalse(isCopyable((any Resource<NoncopyableArg> & ~Copyable).self))
    expectTrue(isEscapable((any Resource<NoncopyableArg> & ~Copyable).self))
  }

  tests.test("Generalized superclass with a noncopyable argument") {
    typealias E = any NoncopyableArgBase<NoncopyableArg> & Holder<Int>
    let erased: Any = NoncopyableArgDerived<NoncopyableArg>(9)
    let cast = erased as? E
    expectNotNil(cast)
    expectEqual(9, cast!.value)
    expectTrue(isCopyable(E.self))
  }
}

runAllTests()
