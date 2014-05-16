// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/SlurpFastEnumeration/SlurpFastEnumeration.m -c -o %t/SlurpFastEnumeration.o -g
// RUN: %target-build-swift %s -I %S/Inputs/SlurpFastEnumeration/ -Xlinker %t/SlurpFastEnumeration.o -o %t/Dictionary

// RUN: %target-run %t/Dictionary > %t.txt
// RUN: FileCheck %s < %t.txt
// RUN: FileCheck --check-prefix=CHECK-PTR%target-ptrsize %s < %t.txt

//===---
// Utilities.
//===---

class Canary {
  var i: Int
  init(i: Int) {
    self.i = i
    println("Canary.init(\(i))")
  }
  deinit {
    println("Canary(\(i)).deinit")
  }
}

var keyCount = 0
var keySerial = 0

// A wrapper class that can help us track allocations and find issues with
// object lifetime.
class TestKeyTy : Equatable, Hashable, Printable {
  init(_ value: Int) {
    ++keyCount
    serial = ++keySerial
    self.value = value
    self._hashValue = value
  }

  convenience init(value: Int, hashValue: Int) {
    self.init(value)
    self._hashValue = hashValue
  }

  deinit {
    assert(serial > 0, "double destruction")
    --keyCount
    serial = -serial
  }

  var description: String {
    assert(serial > 0, "dead TestKeyTy")
    return value.description
  }

  var hashValue: Int {
    return _hashValue
  }

  var value: Int
  var _hashValue: Int
  var serial: Int
}

func == (lhs: TestKeyTy, rhs: TestKeyTy) -> Bool {
  return lhs.value == rhs.value
}

var valueCount = 0
var valueSerial = 0

class TestValueTy : Printable {
  init(_ value: Int) {
    ++valueCount
    serial = ++valueSerial
    self.value = value
  }

  deinit {
    assert(serial > 0, "double destruction")
    --valueCount
    serial = -serial
  }

  var description: String {
    assert(serial > 0, "dead TestValueTy")
    return value.description
  }

  var value: Int
  var serial: Int
}

class TestEquatableValueTy : Equatable, Printable {
  init(_ value: Int) {
    ++valueCount
    serial = ++valueSerial
    self.value = value
  }

  deinit {
    assert(serial > 0, "double destruction")
    --valueCount
    serial = -serial
  }

  var description: String {
    assert(serial > 0, "dead TestEquatableValueTy")
    return value.description
  }

  var value: Int
  var serial: Int
}

func == (lhs: TestEquatableValueTy, rhs: TestEquatableValueTy) -> Bool {
  return lhs.value == rhs.value
}

func acceptsAnyDictionary<KeyTy : Hashable, ValueTy>(
    d: Dictionary<KeyTy, ValueTy>) {
}

func isNativeDictionary<KeyTy : Hashable, ValueTy>(
    d: Dictionary<KeyTy, ValueTy>) -> Bool {
  switch d._variantStorage {
  case .Native:
    return true
  case .Cocoa:
    return false
  }
}

func isCocoaDictionary<KeyTy : Hashable, ValueTy>(
    d: Dictionary<KeyTy, ValueTy>) -> Bool {
  return !isNativeDictionary(d)
}

// Compare two arrays as sets.
func equalsUnordered(lhs: Array<(Int, Int)>, rhs: Array<(Int, Int)>) -> Bool {
  func comparePair(lhs: (Int, Int), rhs: (Int, Int)) -> Bool {
    return lexicographicalCompare([ lhs.0, lhs.1 ], [ rhs.0, rhs.1 ])
  }
  return equal(sort(lhs, comparePair), sort(rhs, comparePair)) {
    $0.0 == $1.0 && $0.1 == $1.1
  }
}

func equalsUnordered(lhs: Array<Int>, rhs: Array<Int>) -> Bool {
  return equal(sort(lhs), sort(rhs))
}

//===---
// Tests.
//===---

func testDictionarySize() {
  var dict = [1: "meow", 2: "meow"]
  println("dict size \(sizeofValue(dict))")

  println("testDictionarySize done")
}
testDictionarySize()
// CHECK-PTR64: dict size 8
// CHECK-PTR32: dict size 4
// CHECK: testDictionarySize done

assert(keyCount == 0, "key leak")
assert(valueCount == 0, "value leak")


func testValueDestruction() {
  var d1 = Dictionary<Int, TestValueTy>()
  for i in 100...110 {
    d1[i] = TestValueTy(i)
  }

  var d2 = Dictionary<TestKeyTy, TestValueTy>()
  for i in 100...110 {
    d2[TestKeyTy(i)] = TestValueTy(i)
  }

  println("testValueDestruction done")
}
// CHECK: testValueDestruction done
testValueDestruction()

assert(keyCount == 0, "key leak")
assert(valueCount == 0, "value leak")


func testCOW_Smoke() {
  var d1 = Dictionary<TestKeyTy, TestValueTy>(minimumCapacity: 10)
  var identity1: Word = reinterpretCast(d1)

  d1[TestKeyTy(10)] = TestValueTy(1010)
  d1[TestKeyTy(20)] = TestValueTy(1020)
  d1[TestKeyTy(30)] = TestValueTy(1030)

  var d2 = d1
  acceptsAnyDictionary(d2)
  assert(identity1 == reinterpretCast(d2))

  d2[TestKeyTy(40)] = TestValueTy(2040)
  assert(identity1 != reinterpretCast(d2))

  d1[TestKeyTy(50)] = TestValueTy(1050)
  assert(identity1 == reinterpretCast(d1))

  // Keep variables alive.
  acceptsAnyDictionary(d1)
  acceptsAnyDictionary(d2)

  println("testCOW_Smoke done")
}
testCOW_Smoke()
// CHECK: testCOW_Smoke done

assert(keyCount == 0, "key leak")
assert(valueCount == 0, "value leak")

func getCOWFastDictionary() -> Dictionary<Int, Int> {
  var d = Dictionary<Int, Int>(minimumCapacity: 10)
  d[10] = 1010
  d[20] = 1020
  d[30] = 1030
  return d
}

func getCOWSlowDictionary() -> Dictionary<TestKeyTy, TestValueTy> {
  var d = Dictionary<TestKeyTy, TestValueTy>(minimumCapacity: 10)
  d[TestKeyTy(10)] = TestValueTy(1010)
  d[TestKeyTy(20)] = TestValueTy(1020)
  d[TestKeyTy(30)] = TestValueTy(1030)
  return d
}

func getCOWSlowEquatableDictionary()
    -> Dictionary<TestKeyTy, TestEquatableValueTy> {
  var d = Dictionary<TestKeyTy, TestEquatableValueTy>(minimumCapacity: 10)
  d[TestKeyTy(10)] = TestEquatableValueTy(1010)
  d[TestKeyTy(20)] = TestEquatableValueTy(1020)
  d[TestKeyTy(30)] = TestEquatableValueTy(1030)
  return d
}


func testCOW_Fast_IndexesDontAffectUniquenessCheck() {
  var d = getCOWFastDictionary()
  var identity1: Word = reinterpretCast(d)

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex != endIndex)
  assert(identity1 == reinterpretCast(d))

  d[40] = 2040
  assert(identity1 == reinterpretCast(d))

  // Keep indexes alive during the calls above.
  withExtendedLifetime(startIndex) { () }
  withExtendedLifetime(endIndex) { () }

  println("testCOW_Fast_IndexesDontAffectUniquenessCheck done")
}
testCOW_Fast_IndexesDontAffectUniquenessCheck()
// CHECK: testCOW_Fast_IndexesDontAffectUniquenessCheck done

func testCOW_Slow_IndexesDontAffectUniquenessCheck() {
  var d = getCOWSlowDictionary()
  var identity1: Word = reinterpretCast(d)

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex != endIndex)
  assert(identity1 == reinterpretCast(d))

  d[TestKeyTy(40)] = TestValueTy(2040)
  assert(identity1 == reinterpretCast(d))

  // Keep indexes alive during the calls above.
  withExtendedLifetime(startIndex) { () }
  withExtendedLifetime(endIndex) { () }

  println("testCOW_Slow_IndexesDontAffectUniquenessCheck done")
}
testCOW_Slow_IndexesDontAffectUniquenessCheck()
// CHECK: testCOW_Slow_IndexesDontAffectUniquenessCheck done


func testCOW_Fast_SubscriptWithIndexDoesNotReallocate() {
  var d = getCOWFastDictionary()
  var identity1: Word = reinterpretCast(d)

  var startIndex = d.startIndex
  assert(identity1 == reinterpretCast(d))

  assert(d[startIndex].1 != 0)
  assert(identity1 == reinterpretCast(d))

  println("testCOW_Fast_SubscriptWithIndexDoesNotReallocate done")
}
testCOW_Fast_SubscriptWithIndexDoesNotReallocate()
// CHECK: testCOW_Fast_SubscriptWithIndexDoesNotReallocate done

func testCOW_Slow_SubscriptWithIndexDoesNotReallocate() {
  var d = getCOWSlowDictionary()
  var identity1: Word = reinterpretCast(d)

  var startIndex = d.startIndex
  assert(identity1 == reinterpretCast(d))

  assert(d[startIndex].1.value != 0)
  assert(identity1 == reinterpretCast(d))

  println("testCOW_Slow_SubscriptWithIndexDoesNotReallocate done")
}
testCOW_Slow_SubscriptWithIndexDoesNotReallocate()
// CHECK: testCOW_Slow_SubscriptWithIndexDoesNotReallocate done


func testCOW_Fast_SubscriptWithKeyDoesNotReallocate() {
  var d = getCOWFastDictionary()
  var identity1: Word = reinterpretCast(d)

  assert(d[10]! == 1010)
  assert(identity1 == reinterpretCast(d))

  // Insert a new key-value pair.
  d[40] = 2040
  assert(identity1 == reinterpretCast(d))
  assert(d.count == 4)
  assert(d[10]! == 1010)
  assert(d[20]! == 1020)
  assert(d[30]! == 1030)
  assert(d[40]! == 2040)

  // Overwrite a value in existing binding.
  d[10] = 2010
  assert(identity1 == reinterpretCast(d))
  assert(d.count == 4)
  assert(d[10]! == 2010)
  assert(d[20]! == 1020)
  assert(d[30]! == 1030)
  assert(d[40]! == 2040)

  // Delete an existing key.
  d[10] = nil
  assert(identity1 == reinterpretCast(d))
  assert(d.count == 3)
  assert(d[20]! == 1020)
  assert(d[30]! == 1030)
  assert(d[40]! == 2040)

  // Try to delete a key that does not exist.
  d[42] = nil
  assert(identity1 == reinterpretCast(d))
  assert(d.count == 3)
  assert(d[20]! == 1020)
  assert(d[30]! == 1030)
  assert(d[40]! == 2040)

  println("testCOW_Fast_SubscriptWithKeyDoesNotReallocate done")
}
testCOW_Fast_SubscriptWithKeyDoesNotReallocate()
// CHECK: testCOW_Fast_SubscriptWithKeyDoesNotReallocate done

func testCOW_Slow_SubscriptWithKeyDoesNotReallocate() {
  var d = getCOWSlowDictionary()
  var identity1: Word = reinterpretCast(d)

  assert(d[TestKeyTy(10)]!.value == 1010)
  assert(identity1 == reinterpretCast(d))

  // Insert a new key-value pair.
  d[TestKeyTy(40)] = TestValueTy(2040)
  assert(identity1 == reinterpretCast(d))
  assert(d.count == 4)
  assert(d[TestKeyTy(10)]!.value == 1010)
  assert(d[TestKeyTy(20)]!.value == 1020)
  assert(d[TestKeyTy(30)]!.value == 1030)
  assert(d[TestKeyTy(40)]!.value == 2040)

  // Overwrite a value in existing binding.
  d[TestKeyTy(10)] = TestValueTy(2010)
  assert(identity1 == reinterpretCast(d))
  assert(d.count == 4)
  assert(d[TestKeyTy(10)]!.value == 2010)
  assert(d[TestKeyTy(20)]!.value == 1020)
  assert(d[TestKeyTy(30)]!.value == 1030)
  assert(d[TestKeyTy(40)]!.value == 2040)

  // Delete an existing key.
  d[TestKeyTy(10)] = nil
  assert(identity1 == reinterpretCast(d))
  assert(d.count == 3)
  assert(d[TestKeyTy(20)]!.value == 1020)
  assert(d[TestKeyTy(30)]!.value == 1030)
  assert(d[TestKeyTy(40)]!.value == 2040)

  // Try to delete a key that does not exist.
  d[TestKeyTy(42)] = nil
  assert(identity1 == reinterpretCast(d))
  assert(d.count == 3)
  assert(d[TestKeyTy(20)]!.value == 1020)
  assert(d[TestKeyTy(30)]!.value == 1030)
  assert(d[TestKeyTy(40)]!.value == 2040)

  println("testCOW_Slow_SubscriptWithKeyDoesNotReallocate done")
}
testCOW_Slow_SubscriptWithKeyDoesNotReallocate()
// CHECK: testCOW_Slow_SubscriptWithKeyDoesNotReallocate done

func testCOW_Fast_UpdateValueForKeyDoesNotReallocate() {
  if true {
    var d1 = getCOWFastDictionary()
    var identity1: Word = reinterpretCast(d1)

    // Insert a new key-value pair.
    assert(d1.updateValue(2040, forKey: 40) == .None)
    assert(identity1 == reinterpretCast(d1))
    assert(d1[40]! == 2040)

    // Overwrite a value in existing binding.
    assert(d1.updateValue(2010, forKey: 10)! == 1010)
    assert(identity1 == reinterpretCast(d1))
    assert(d1[10]! == 2010)
  }

  if true {
    var d1 = getCOWFastDictionary()
    var identity1: Word = reinterpretCast(d1)

    var d2 = d1
    assert(identity1 == reinterpretCast(d1))
    assert(identity1 == reinterpretCast(d2))

    // Insert a new key-value pair.
    d2.updateValue(2040, forKey: 40)
    assert(identity1 == reinterpretCast(d1))
    assert(identity1 != reinterpretCast(d2))

    assert(d1.count == 3)
    assert(d1[10]! == 1010)
    assert(d1[20]! == 1020)
    assert(d1[30]! == 1030)
    assert(d1[40] == .None)

    assert(d2.count == 4)
    assert(d2[10]! == 1010)
    assert(d2[20]! == 1020)
    assert(d2[30]! == 1030)
    assert(d2[40]! == 2040)

    // Keep variables alive.
    acceptsAnyDictionary(d1)
    acceptsAnyDictionary(d2)
  }

  if true {
    var d1 = getCOWFastDictionary()
    var identity1: Word = reinterpretCast(d1)

    var d2 = d1
    assert(identity1 == reinterpretCast(d1))
    assert(identity1 == reinterpretCast(d2))

    // Overwrite a value in existing binding.
    d2.updateValue(2010, forKey: 10)
    assert(identity1 == reinterpretCast(d1))
    assert(identity1 != reinterpretCast(d2))

    assert(d1.count == 3)
    assert(d1[10]! == 1010)
    assert(d1[20]! == 1020)
    assert(d1[30]! == 1030)

    assert(d2.count == 3)
    assert(d2[10]! == 2010)
    assert(d2[20]! == 1020)
    assert(d2[30]! == 1030)

    // Keep variables alive.
    acceptsAnyDictionary(d1)
    acceptsAnyDictionary(d2)
  }

  println("testCOW_Fast_UpdateValueForKeyDoesNotReallocate done")
}
testCOW_Fast_UpdateValueForKeyDoesNotReallocate()
// CHECK: testCOW_Fast_UpdateValueForKeyDoesNotReallocate done

func testCOW_Slow_AddDoesNotReallocate() {
  if true {
    var d1 = getCOWSlowDictionary()
    var identity1: Word = reinterpretCast(d1)

    // Insert a new key-value pair.
    assert(!d1.updateValue(TestValueTy(2040), forKey: TestKeyTy(40)))
    assert(identity1 == reinterpretCast(d1))
    assert(d1.count == 4)
    assert(d1[TestKeyTy(40)]!.value == 2040)

    // Overwrite a value in existing binding.
    assert(d1.updateValue(TestValueTy(2010), forKey: TestKeyTy(10))!.value == 1010)
    assert(identity1 == reinterpretCast(d1))
    assert(d1.count == 4)
    assert(d1[TestKeyTy(10)]!.value == 2010)
  }

  if true {
    var d1 = getCOWSlowDictionary()
    var identity1: Word = reinterpretCast(d1)

    var d2 = d1
    assert(identity1 == reinterpretCast(d1))
    assert(identity1 == reinterpretCast(d2))

    // Insert a new key-value pair.
    d2.updateValue(TestValueTy(2040), forKey: TestKeyTy(40))
    assert(identity1 == reinterpretCast(d1))
    assert(identity1 != reinterpretCast(d2))

    assert(d1.count == 3)
    assert(d1[TestKeyTy(10)]!.value == 1010)
    assert(d1[TestKeyTy(20)]!.value == 1020)
    assert(d1[TestKeyTy(30)]!.value == 1030)
    assert(!d1[TestKeyTy(40)])

    assert(d2.count == 4)
    assert(d2[TestKeyTy(10)]!.value == 1010)
    assert(d2[TestKeyTy(20)]!.value == 1020)
    assert(d2[TestKeyTy(30)]!.value == 1030)
    assert(d2[TestKeyTy(40)]!.value == 2040)

    // Keep variables alive.
    acceptsAnyDictionary(d1)
    acceptsAnyDictionary(d2)
  }

  if true {
    var d1 = getCOWSlowDictionary()
    var identity1: Word = reinterpretCast(d1)

    var d2 = d1
    assert(identity1 == reinterpretCast(d1))
    assert(identity1 == reinterpretCast(d2))

    // Overwrite a value in existing binding.
    d2.updateValue(TestValueTy(2010), forKey: TestKeyTy(10))
    assert(identity1 == reinterpretCast(d1))
    assert(identity1 != reinterpretCast(d2))

    assert(d1.count == 3)
    assert(d1[TestKeyTy(10)]!.value == 1010)
    assert(d1[TestKeyTy(20)]!.value == 1020)
    assert(d1[TestKeyTy(30)]!.value == 1030)

    assert(d2.count == 3)
    assert(d2[TestKeyTy(10)]!.value == 2010)
    assert(d2[TestKeyTy(20)]!.value == 1020)
    assert(d2[TestKeyTy(30)]!.value == 1030)

    // Keep variables alive.
    acceptsAnyDictionary(d1)
    acceptsAnyDictionary(d2)
  }

  println("testCOW_Slow_AddDoesNotReallocate done")
}
testCOW_Slow_AddDoesNotReallocate()
// CHECK: testCOW_Slow_AddDoesNotReallocate done


func testCOW_Fast_FindDoesNotReallocate() {
  var d = getCOWFastDictionary()
  var identity1: Word = reinterpretCast(d)

  var found = d.find(10)
  assert(found == 1010)
  assert(identity1 == reinterpretCast(d))

  println("testCOW_Fast_FindDoesNotReallocate done")
}
testCOW_Fast_FindDoesNotReallocate()
// CHECK: testCOW_Fast_FindDoesNotReallocate done

func testCOW_Slow_FindDoesNotReallocate() {
  var d = getCOWSlowDictionary()
  var identity1: Word = reinterpretCast(d)

  var found = d.find(TestKeyTy(10))
  assert(found!.value == 1010)
  assert(identity1 == reinterpretCast(d))

  println("testCOW_Slow_FindDoesNotReallocate done")
}
testCOW_Slow_FindDoesNotReallocate()
// CHECK: testCOW_Slow_FindDoesNotReallocate done


func testCOW_Fast_DeleteKeyDoesNotReallocate() {
  var d1 = getCOWFastDictionary()
  var identity1: Word = reinterpretCast(d1)

  var deleted = d1._deleteKey(0)
  assert(deleted == false)
  assert(identity1 == reinterpretCast(d1))

  deleted = d1._deleteKey(10)
  assert(deleted == true)
  assert(identity1 == reinterpretCast(d1))

  var d2 = d1
  deleted = d2._deleteKey(0)
  assert(deleted == false)
  assert(identity1 == reinterpretCast(d1))
  assert(identity1 == reinterpretCast(d2))

  deleted = d2._deleteKey(20)
  assert(deleted == true)
  assert(identity1 == reinterpretCast(d1))
  assert(identity1 != reinterpretCast(d2))

  // Keep variables alive.
  acceptsAnyDictionary(d1)
  acceptsAnyDictionary(d2)

  println("testCOW_Fast_DeleteKeyDoesNotReallocate done")
}
testCOW_Fast_DeleteKeyDoesNotReallocate()
// CHECK: testCOW_Fast_DeleteKeyDoesNotReallocate done

func testCOW_Slow_DeleteKeyDoesNotReallocate() {
  var d1 = getCOWSlowDictionary()
  var identity1: Word = reinterpretCast(d1)

  var deleted = d1._deleteKey(TestKeyTy(10))
  assert(deleted == true)
  assert(identity1 == reinterpretCast(d1))

  var d2 = d1
  deleted = d2._deleteKey(TestKeyTy(0))
  assert(deleted == false)
  assert(identity1 == reinterpretCast(d1))
  assert(identity1 == reinterpretCast(d2))

  deleted = d2._deleteKey(TestKeyTy(20))
  assert(deleted == true)
  assert(identity1 == reinterpretCast(d1))
  assert(identity1 != reinterpretCast(d2))

  // Keep variables alive.
  acceptsAnyDictionary(d1)
  acceptsAnyDictionary(d2)

  println("testCOW_Slow_DeleteKeyDoesNotReallocate done")
}
testCOW_Slow_DeleteKeyDoesNotReallocate()
// CHECK: testCOW_Slow_DeleteKeyDoesNotReallocate done


func testCOW_Fast_CountDoesNotReallocate() {
  var d = getCOWFastDictionary()
  var identity1: Word = reinterpretCast(d)

  assert(d.count == 3)
  assert(identity1 == reinterpretCast(d))

  println("testCOW_Fast_CountDoesNotReallocate done")
}
testCOW_Fast_CountDoesNotReallocate()
// CHECK: testCOW_Fast_CountDoesNotReallocate done

func testCOW_Slow_CountDoesNotReallocate() {
  var d = getCOWSlowDictionary()
  var identity1: Word = reinterpretCast(d)

  assert(d.count == 3)
  assert(identity1 == reinterpretCast(d))

  println("testCOW_Slow_CountDoesNotReallocate done")
}
testCOW_Slow_CountDoesNotReallocate()
// CHECK: testCOW_Slow_CountDoesNotReallocate done


func testCOW_Fast_GenerateDoesNotReallocate() {
  var d = getCOWFastDictionary()
  var identity1: Word = reinterpretCast(d)

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value) = gen.next() {
    pairs += (key, value)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
  assert(identity1 == reinterpretCast(d))

  println("testCOW_Fast_GenerateDoesNotReallocate done")
}
testCOW_Fast_GenerateDoesNotReallocate()
// CHECK: testCOW_Fast_GenerateDoesNotReallocate done

func testCOW_Slow_GenerateDoesNotReallocate() {
  var d = getCOWSlowDictionary()
  var identity1: Word = reinterpretCast(d)

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value) = gen.next() {
    pairs += (key.value, value.value)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
  assert(identity1 == reinterpretCast(d))

  println("testCOW_Slow_GenerateDoesNotReallocate done")
}
testCOW_Slow_GenerateDoesNotReallocate()
// CHECK: testCOW_Slow_GenerateDoesNotReallocate done


func testCOW_Fast_EqualityTestDoesNotReallocate() {
  var d1 = getCOWFastDictionary()
  var identity1: Word = reinterpretCast(d1)

  var d2 = getCOWFastDictionary()
  var identity2: Word = reinterpretCast(d2)

  assert(d1 == d2)
  assert(identity1 == reinterpretCast(d1))
  assert(identity2 == reinterpretCast(d2))

  d2[40] = 2040
  assert(d1 != d2)
  assert(identity1 == reinterpretCast(d1))
  assert(identity2 == reinterpretCast(d2))

  println("testCOW_Fast_EqualityTestDoesNotReallocate done")
}
testCOW_Fast_EqualityTestDoesNotReallocate()
// CHECK: testCOW_Fast_EqualityTestDoesNotReallocate done

func testCOW_Slow_EqualityTestDoesNotReallocate() {
  var d1 = getCOWSlowEquatableDictionary()
  var identity1: Word = reinterpretCast(d1)

  var d2 = getCOWSlowEquatableDictionary()
  var identity2: Word = reinterpretCast(d2)

  assert(d1 == d2)
  assert(identity1 == reinterpretCast(d1))
  assert(identity2 == reinterpretCast(d2))

  d2[TestKeyTy(40)] = TestEquatableValueTy(2040)
  assert(d1 != d2)
  assert(identity1 == reinterpretCast(d1))
  assert(identity2 == reinterpretCast(d2))

  println("testCOW_Slow_EqualityTestDoesNotReallocate done")
}
testCOW_Slow_EqualityTestDoesNotReallocate()
// CHECK: testCOW_Slow_EqualityTestDoesNotReallocate done


//===---
// Native dictionary tests.
//===---

func helperDeleteThree(k1: TestKeyTy, k2: TestKeyTy, k3: TestKeyTy) {
  var d1 = Dictionary<TestKeyTy, TestValueTy>(minimumCapacity: 10)

  d1[k1] = TestValueTy(1010)
  d1[k2] = TestValueTy(1020)
  d1[k3] = TestValueTy(1030)

  assert(d1[k1]!.value == 1010)
  assert(d1[k2]!.value == 1020)
  assert(d1[k3]!.value == 1030)

  d1[k1] = nil
  assert(d1[k2]!.value == 1020)
  assert(d1[k3]!.value == 1030)

  d1[k2] = nil
  assert(d1[k3]!.value == 1030)

  d1[k3] = nil
  assert(d1.count == 0)
}

func testDeleteChainCollision() {
  var k1 = TestKeyTy(value: 10, hashValue: 0)
  var k2 = TestKeyTy(value: 20, hashValue: 0)
  var k3 = TestKeyTy(value: 30, hashValue: 0)

  helperDeleteThree(k1, k2, k3)

  println("testDeleteChainCollision done")
}
testDeleteChainCollision()
// CHECK: testDeleteChainCollision done

func testDeleteChainNoCollision() {
  var k1 = TestKeyTy(value: 10, hashValue: 0)
  var k2 = TestKeyTy(value: 20, hashValue: 1)
  var k3 = TestKeyTy(value: 30, hashValue: 2)

  helperDeleteThree(k1, k2, k3)

  println("testDeleteChainNoCollision done")
}
testDeleteChainNoCollision()
// CHECK: testDeleteChainNoCollision done


//===---
// NSDictionary -> Dictionary bridging tests.
//===---

import Foundation

func isNativeNSDictionary(d: NSDictionary) -> Bool {
  var className: NSString = NSStringFromClass(d.dynamicType)
  return className.rangeOfString("_NativeDictionaryStorageOwner").length > 0
}

func isCocoaNSDictionary(d: NSDictionary) -> Bool {
  var className: NSString = NSStringFromClass(d.dynamicType)
  return className.rangeOfString("NSDictionary").length > 0
}

var objcKeyCount = 0
var objcKeySerial = 0

class TestObjCKeyTy : NSObject, NSCopying, Printable {
  init(_ value: Int) {
    ++keyCount
    serial = ++keySerial
    self.value = value
    self._hashValue = value
  }

  convenience init(value: Int, hashValue: Int) {
    self.init(value)
    self._hashValue = hashValue
  }

  deinit {
    assert(serial > 0, "double destruction")
    --keyCount
    serial = -serial
  }

  @objc
  func copyWithZone(zone: NSZone) -> AnyObject {
    return TestObjCKeyTy(value)
  }

  override var description: String {
    assert(serial > 0, "dead TestObjCKeyTy")
    return value.description
  }

  override func isEqual(object: AnyObject!) -> Bool {
    if let other: AnyObject = object {
      if let otherObjcKey = other as TestObjCKeyTy {
        return self.value == otherObjcKey.value
      }
    }
    return false
  }

  override var hash : Int {
    return _hashValue
  }

  func bridgeToObjectiveC() -> TestObjCKeyTy {
    return self
  }

  var value: Int
  var _hashValue: Int
  var serial: Int
}

var objcValueCount = 0
var objcValueSerial = 0

class TestObjCValueTy : NSObject, Printable {
  init(_ value: Int) {
    ++valueCount
    serial = ++valueSerial
    self.value = value
  }

  deinit {
    assert(serial > 0, "double destruction")
    --valueCount
    serial = -serial
  }

  override var description: String {
    assert(serial > 0, "dead TestObjCValueTy")
    return value.description
  }

  var value: Int
  var serial: Int
}

func slurpFastEnumeration(
    d: NSDictionary, fe: NSFastEnumeration
) -> Array<(Int, Int)> {
  var state = NSFastEnumerationState()
  let stackBufLength = 3
  var stackBuf = HeapBuffer<(), AnyObject?>(
      HeapBufferStorageBase.self, (), stackBufLength)

  var pairs = Array<(Int, Int)>()
  while true {
    var returnedCount = fe.countByEnumeratingWithState(
        &state, objects: stackBuf.elementStorage,
        count: stackBufLength)
    assert(state.state != 0)
    assert(state.mutationsPtr != .null())
    if returnedCount == 0 {
      break
    }
    for i in 0..returnedCount {
      let key: AnyObject = state.itemsPtr[i]!
      let value: AnyObject = d.objectForKey(key)
      pairs += ((key as TestObjCKeyTy)!.value, (value as TestObjCValueTy)!.value)
    }
  }

  for i in 0..3 {
    let returnedCount = fe.countByEnumeratingWithState(
        &state, objects: stackBuf.elementStorage,
        count: stackBufLength)
    assert(returnedCount == 0)
  }

  return pairs
}

import SlurpFastEnumeration

func slurpFastEnumerationFromObjC(
    d: NSDictionary, fe: NSFastEnumeration
) -> Array<(Int, Int)> {
  var objcPairs = NSMutableArray()
  slurpFastEnumerationFromObjCImpl(d, fe, objcPairs)
  var pairs = Array<(Int, Int)>()
  // Cast to NSArray to work around
  // <rdar://problem/16865289> type 'NSMutableArray' does not conform to
  // protocol 'Sequence'
  for pairAnyObject: AnyObject in (objcPairs as NSArray) {
    let pair = (pairAnyObject as NSArray)!
    let key = (pair[0] as TestObjCKeyTy)!.value
    let value = (pair[1] as TestObjCValueTy)!.value
    pairs += (key, value)
  }
  return pairs
}

func getBridgedDictionary() -> Dictionary<NSObject, AnyObject> {
  let keys = NSMutableArray()
  keys.addObject(TestObjCKeyTy(10))
  keys.addObject(TestObjCKeyTy(20))
  keys.addObject(TestObjCKeyTy(30))

  let values = NSMutableArray()
  values.addObject(TestObjCValueTy(1010))
  values.addObject(TestObjCValueTy(1020))
  values.addObject(TestObjCValueTy(1030))

  var nsd = NSDictionary(objects: values, forKeys: keys)

  return _convertNSDictionaryToDictionary(nsd)
}

func getEmptyBridgedDictionary() -> Dictionary<NSObject, AnyObject> {
  let keys = NSMutableArray()
  let values = NSMutableArray()

  var nsd = NSDictionary(objects: values, forKeys: keys)

  return _convertNSDictionaryToDictionary(nsd)
}

func getHugeBridgedDictionary() -> Dictionary<NSObject, AnyObject> {
  let keys = NSMutableArray()
  let values = NSMutableArray()
  for i in 1...32 {
    keys.addObject(TestObjCKeyTy(i))
    values.addObject(TestObjCValueTy(1000 + i))
  }

  var nsd = NSDictionary(objects: values, forKeys: keys)

  return _convertNSDictionaryToDictionary(nsd)
}

/// A mock dictionary that stores its keys and values in parallel arrays, which
/// allows it to return inner pointers to the keys array in fast enumeration.
@objc
class ParallelArrayDictionary : NSDictionary {
  struct Keys {
    var key0: AnyObject = TestObjCKeyTy(10)
    var key1: AnyObject = TestObjCKeyTy(20)
    var key2: AnyObject = TestObjCKeyTy(30)
    var key3: AnyObject = TestObjCKeyTy(40)
  }
  var keys = [ Keys() ]
  var value: AnyObject = TestObjCValueTy(1111)

  init() {
    keys += Keys()
    super.init()
  }

  override func countByEnumeratingWithState(
      state: CMutablePointer<NSFastEnumerationState>,
      objects: ObjCMutablePointer<AnyObject?>, count: Int) -> Int {
    var stateUP = UnsafePointer(state)
    var theState = stateUP.pointee
    if theState.state == 0 {
      theState.state = 1
      theState.itemsPtr = UnsafePointer(keys.elementStorage)
      stateUP.pointee = theState
      return 4
    }
    return 0
  }

  override func objectForKey(aKey: AnyObject?) -> AnyObject? {
    return value
  }
}

func getParallelArrayBridgedDictionary() -> Dictionary<NSObject, AnyObject> {
  var nsd: NSDictionary = ParallelArrayDictionary()
  return _convertNSDictionaryToDictionary(nsd)
}

func test_BridgedFromObjC_SubscriptWithIndex() {
  var d = getBridgedDictionary()
  var identity1: Word = reinterpretCast(d)
  assert(isCocoaDictionary(d))

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex != endIndex)
  assert(identity1 == reinterpretCast(d))

  var pairs = Array<(Int, Int)>()
  for var i = startIndex; i != endIndex; ++i {
    var (key, value: AnyObject) = d[i]
    pairs += ((key as TestObjCKeyTy)!.value, (value as TestObjCValueTy)!.value)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
  assert(identity1 == reinterpretCast(d))

  // Keep indexes alive during the calls above.
  withExtendedLifetime(startIndex) { () }
  withExtendedLifetime(endIndex) { () }

  println("test_BridgedFromObjC_SubscriptWithIndex done")
}
test_BridgedFromObjC_SubscriptWithIndex()
// CHECK: test_BridgedFromObjC_SubscriptWithIndex done

func test_BridgedFromObjC_SubscriptWithIndex_Empty() {
  var d = getEmptyBridgedDictionary()
  var identity1: Word = reinterpretCast(d)
  assert(isCocoaDictionary(d))

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex == endIndex)
  assert(identity1 == reinterpretCast(d))

  // Keep indexes alive during the calls above.
  withExtendedLifetime(startIndex) { () }
  withExtendedLifetime(endIndex) { () }

  println("test_BridgedFromObjC_SubscriptWithIndex_Empty done")
}
test_BridgedFromObjC_SubscriptWithIndex_Empty()
// CHECK: test_BridgedFromObjC_SubscriptWithIndex_Empty done


func test_BridgedFromObjC_SubscriptWithKey() {
  var d = getBridgedDictionary()
  var identity1: Word = reinterpretCast(d)
  assert(isCocoaDictionary(d))

  // Read existing key-value pairs.
  var v = d[TestObjCKeyTy(10)] as TestObjCValueTy
  assert(v!.value == 1010)

  v = d[TestObjCKeyTy(20)] as TestObjCValueTy
  assert(v!.value == 1020)

  v = d[TestObjCKeyTy(30)] as TestObjCValueTy
  assert(v!.value == 1030)

  assert(identity1 == reinterpretCast(d))

  // Insert a new key-value pair.
  d[TestObjCKeyTy(40)] = TestObjCValueTy(2040)
  var identity2: Word = reinterpretCast(d)
  assert(identity1 != identity2)
  assert(isNativeDictionary(d))
  assert(d.count == 4)

  v = d[TestObjCKeyTy(10)] as TestObjCValueTy
  assert(v!.value == 1010)

  v = d[TestObjCKeyTy(20)] as TestObjCValueTy
  assert(v!.value == 1020)

  v = d[TestObjCKeyTy(30)] as TestObjCValueTy
  assert(v!.value == 1030)

  v = d[TestObjCKeyTy(40)] as TestObjCValueTy
  assert(v!.value == 2040)

  // Overwrite value in existing binding.
  d[TestObjCKeyTy(10)] = TestObjCValueTy(2010)
  assert(identity2 == reinterpretCast(d))
  assert(isNativeDictionary(d))
  assert(d.count == 4)

  v = d[TestObjCKeyTy(10)] as TestObjCValueTy
  assert(v!.value == 2010)

  v = d[TestObjCKeyTy(20)] as TestObjCValueTy
  assert(v!.value == 1020)

  v = d[TestObjCKeyTy(30)] as TestObjCValueTy
  assert(v!.value == 1030)

  v = d[TestObjCKeyTy(40)] as TestObjCValueTy
  assert(v!.value == 2040)

  println("test_BridgedFromObjC_SubscriptWithKey done")
}
test_BridgedFromObjC_SubscriptWithKey()
// CHECK: test_BridgedFromObjC_SubscriptWithKey done


func test_BridgedFromObjC_UpdateValueForKey() {
  // Insert a new key-value pair.
  if true {
    var d = getBridgedDictionary()
    var identity1: Word = reinterpretCast(d)
    assert(isCocoaDictionary(d))

    var oldValue: AnyObject? =
        d.updateValue(TestObjCValueTy(2040), forKey: TestObjCKeyTy(40))
    assert(!oldValue)
    var identity2: Word = reinterpretCast(d)
    assert(identity1 != identity2)
    assert(isNativeDictionary(d))
    assert(d.count == 4)

    assert(d.find(TestObjCKeyTy(10))!.value == 1010)
    assert(d.find(TestObjCKeyTy(20))!.value == 1020)
    assert(d.find(TestObjCKeyTy(30))!.value == 1030)
    assert(d.find(TestObjCKeyTy(40))!.value == 2040)
  }

  // Overwrite a value in existing binding.
  if true {
    var d = getBridgedDictionary()
    var identity1: Word = reinterpretCast(d)
    assert(isCocoaDictionary(d))

    var oldValue: AnyObject? =
        d.updateValue(TestObjCValueTy(2010), forKey: TestObjCKeyTy(10))
    assert((oldValue as TestObjCValueTy)!.value == 1010)

    var identity2: Word = reinterpretCast(d)
    assert(identity1 != identity2)
    assert(isNativeDictionary(d))
    assert(d.count == 3)

    assert(d.find(TestObjCKeyTy(10))!.value == 2010)
    assert(d.find(TestObjCKeyTy(20))!.value == 1020)
    assert(d.find(TestObjCKeyTy(30))!.value == 1030)
  }

  println("test_BridgedFromObjC_UpdateValueForKey done")
}
test_BridgedFromObjC_UpdateValueForKey()
// CHECK: test_BridgedFromObjC_UpdateValueForKey done


func test_BridgedFromObjC_Find() {
  var d = getBridgedDictionary()
  var identity1: Word = reinterpretCast(d)
  assert(isCocoaDictionary(d))

  assert(d.find(TestObjCKeyTy(10))!.value == 1010)
  assert(d.find(TestObjCKeyTy(20))!.value == 1020)
  assert(d.find(TestObjCKeyTy(30))!.value == 1030)
  assert(!d.find(TestObjCKeyTy(40)))
  assert(identity1 == reinterpretCast(d))

  println("test_BridgedFromObjC_Find done")
}
test_BridgedFromObjC_Find()
// CHECK: test_BridgedFromObjC_Find done


func test_BridgedFromObjC_DeleteKey() {
  var d = getBridgedDictionary()
  var identity1: Word = reinterpretCast(d)
  assert(isCocoaDictionary(d))

  var deleted = d._deleteKey(TestObjCKeyTy(0))
  assert(deleted == false)
  assert(identity1 == reinterpretCast(d))
  assert(isCocoaDictionary(d))

  deleted = d._deleteKey(TestObjCKeyTy(10))
  assert(deleted == true)
  var identity2: Word = reinterpretCast(d)
  assert(identity1 != identity2)
  assert(isNativeDictionary(d))
  assert(d.count == 2)

  assert(!d.find(TestObjCKeyTy(10)))
  assert(d.find(TestObjCKeyTy(20))!.value == 1020)
  assert(d.find(TestObjCKeyTy(30))!.value == 1030)
  assert(identity2 == reinterpretCast(d))

  println("test_BridgedFromObjC_DeleteKey done")
}
test_BridgedFromObjC_DeleteKey()
// CHECK: test_BridgedFromObjC_DeleteKey done


func test_BridgedFromObjC_Count() {
  var d = getBridgedDictionary()
  var identity1: Word = reinterpretCast(d)
  assert(isCocoaDictionary(d))

  assert(d.count == 3)
  assert(identity1 == reinterpretCast(d))

  println("test_BridgedFromObjC_Count done")
}
test_BridgedFromObjC_Count()
// CHECK: test_BridgedFromObjC_Count done


func test_BridgedFromObjC_Generate() {
  var d = getBridgedDictionary()
  var identity1: Word = reinterpretCast(d)
  assert(isCocoaDictionary(d))

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value: AnyObject) = gen.next() {
    pairs += ((key as TestObjCKeyTy)!.value, (value as TestObjCValueTy)!.value)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
  // The following is not required by the Generator protocol, but
  // it is a nice QoI.
  assert(!gen.next())
  assert(!gen.next())
  assert(!gen.next())
  assert(identity1 == reinterpretCast(d))

  println("test_BridgedFromObjC_Generate done")
}
test_BridgedFromObjC_Generate()
// CHECK: test_BridgedFromObjC_Generate done

func test_BridgedFromObjC_Generate_Empty() {
  var d = getEmptyBridgedDictionary()
  var identity1: Word = reinterpretCast(d)
  assert(isCocoaDictionary(d))

  var gen = d.generate()
  // Can not write code below because of
  // <rdar://problem/16811736> Optional tuples are broken as optionals regarding == comparison
  // assert(gen.next() == .None)
  assert(!gen.next())
  // The following is not required by the Generator protocol, but
  // it is a nice QoI.
  assert(!gen.next())
  assert(!gen.next())
  assert(!gen.next())
  assert(identity1 == reinterpretCast(d))

  println("test_BridgedFromObjC_Generate_Empty done")
}
test_BridgedFromObjC_Generate_Empty()
// CHECK: test_BridgedFromObjC_Generate_Empty done

assert(objcKeyCount == 0, "key leak")
assert(objcValueCount == 0, "value leak")

func test_BridgedFromObjC_Generate_Huge() {
  var d = getHugeBridgedDictionary()
  var identity1: Word = reinterpretCast(d)
  assert(isCocoaDictionary(d))

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value: AnyObject) = gen.next() {
    pairs += ((key as TestObjCKeyTy)!.value, (value as TestObjCValueTy)!.value)
  }
  var expectedPairs = Array<(Int, Int)>()
  for i in 1...32 {
    expectedPairs += (i, 1000 + i)
  }
  assert(equalsUnordered(pairs, expectedPairs))
  // The following is not required by the Generator protocol, but
  // it is a nice QoI.
  assert(!gen.next())
  assert(!gen.next())
  assert(!gen.next())
  assert(identity1 == reinterpretCast(d))

  println("test_BridgedFromObjC_Generate_Huge done")
}
test_BridgedFromObjC_Generate_Huge()
// CHECK: test_BridgedFromObjC_Generate_Huge done

assert(objcKeyCount == 0, "key leak")
assert(objcValueCount == 0, "value leak")

func test_BridgedFromObjC_Generate_ParallelArray() {
  var d = getParallelArrayBridgedDictionary()
  var identity1: Word = reinterpretCast(d)
  assert(isCocoaDictionary(d))

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value: AnyObject) = gen.next() {
    pairs += ((key as TestObjCKeyTy)!.value, (value as TestObjCValueTy)!.value)
  }
  var expectedPairs = [ (10, 1111), (20, 1111), (30, 1111), (40, 1111) ]
  assert(equalsUnordered(pairs, expectedPairs))
  // The following is not required by the Generator protocol, but
  // it is a nice QoI.
  assert(!gen.next())
  assert(!gen.next())
  assert(!gen.next())
  assert(identity1 == reinterpretCast(d))

  println("test_BridgedFromObjC_Generate_ParallelArray done")
}
test_BridgedFromObjC_Generate_ParallelArray()
// CHECK: test_BridgedFromObjC_Generate_ParallelArray done

assert(objcKeyCount == 0, "key leak")
assert(objcValueCount == 0, "value leak")

#if false
func test_BridgedFromObjC_EqualityTest_Empty() {
  var d1 = getEmptyBridgedDictionary()
  var identity1: Word = reinterpretCast(d1)
  assert(isCocoaDictionary(d1))

  var d2 = getEmptyBridgedDictionary()
  var identity2: Word = reinterpretCast(d2)
  assert(isCocoaDictionary(d2))

  assert(d1 == d2)
  assert(identity1 == reinterpretCast(d1))
  assert(identity2 == reinterpretCast(d2))

  d2.add(TestObjCKeyTy(10), TestObjCValueTy(2010))
  assert(isNativeDictionary(d2))
  assert(identity2 != reinterpretCast(d2))
  identity2 = reinterpretCast(d2)

  assert(d1 != d2)
  assert(identity1 == reinterpretCast(d1))
  assert(identity2 == reinterpretCast(d2))

  println("test_BridgedFromObjC_EqualityTest_Empty done")
}
test_BridgedFromObjC_EqualityTest_Empty()
// CHECK_FIXME: test_BridgedFromObjC_EqualityTest_Empty done
#endif


//===---
// Dictionary -> NSDictionary bridging tests.
//
// KeyType and ValueType are bridged verbatim.
//===---

func getBridgedNSDictionaryOfRefTypesBridgedVerbatim() -> NSDictionary {
  assert(isBridgedVerbatimToObjectiveC(TestObjCKeyTy.self))
  assert(isBridgedVerbatimToObjectiveC(TestObjCValueTy.self))

  var d = Dictionary<TestObjCKeyTy, TestObjCValueTy>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  let bridged =
      reinterpretCast(_convertDictionaryToNSDictionary(d)) as NSDictionary
  assert(isNativeNSDictionary(bridged))

  return bridged
}

func getBridgedEmptyNSDictionary() -> NSDictionary {
  var d = Dictionary<TestObjCKeyTy, TestObjCValueTy>()

  let bridged =
      reinterpretCast(_convertDictionaryToNSDictionary(d)) as NSDictionary
  assert(isNativeNSDictionary(bridged))

  return bridged
}


func test_BridgedToObjC_Count() {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()

  assert(d.count == 3)

  println("test_BridgedToObjC_Count done")
}
test_BridgedToObjC_Count()
// CHECK: test_BridgedToObjC_Count done

func test_BridgedToObjC_ObjectForKey() {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()

  assert(!d.objectForKey(nil))

  var v: AnyObject? = d.objectForKey(TestObjCKeyTy(10))
  assert((v as TestObjCValueTy)!.value == 1010)

  v = d.objectForKey(TestObjCKeyTy(20))
  assert((v as TestObjCValueTy)!.value == 1020)

  v = d.objectForKey(TestObjCKeyTy(30))
  assert((v as TestObjCValueTy)!.value == 1030)

  assert(!d.objectForKey(TestObjCKeyTy(40)))

  println("test_BridgedToObjC_ObjectForKey done")
}
test_BridgedToObjC_ObjectForKey()
// CHECK: test_BridgedToObjC_ObjectForKey done

func test_BridgedToObjC_KeyEnumerator_NextObject() {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()
  let enumerator = d.keyEnumerator()

  var pairs = Array<(Int, Int)>()
  while let key: AnyObject = enumerator.nextObject() {
    let value: AnyObject = d.objectForKey(key)
    pairs += ((key as TestObjCKeyTy)!.value, (value as TestObjCValueTy)!.value)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  assert(!enumerator.nextObject())
  assert(!enumerator.nextObject())
  assert(!enumerator.nextObject())

  println("test_BridgedToObjC_KeyEnumerator_NextObject done")
}
test_BridgedToObjC_KeyEnumerator_NextObject()
// CHECK: test_BridgedToObjC_KeyEnumerator_NextObject done

func test_BridgedToObjC_KeyEnumerator_NextObject_Empty() {
  let d = getBridgedEmptyNSDictionary()
  let enumerator = d.keyEnumerator()

  assert(!enumerator.nextObject())
  assert(!enumerator.nextObject())
  assert(!enumerator.nextObject())

  println("test_BridgedToObjC_KeyEnumerator_NextObject_Empty done")
}
test_BridgedToObjC_KeyEnumerator_NextObject_Empty()
// CHECK: test_BridgedToObjC_KeyEnumerator_NextObject_Empty done

func test_BridgedToObjC_KeyEnumerator_FastEnumeration() {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()

  var pairs = slurpFastEnumeration(d, d.keyEnumerator())
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  pairs = slurpFastEnumerationFromObjC(d, d.keyEnumerator())
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  println("test_BridgedToObjC_KeyEnumerator_FastEnumeration done")
}
test_BridgedToObjC_KeyEnumerator_FastEnumeration()
// CHECK: test_BridgedToObjC_KeyEnumerator_FastEnumeration done

func test_BridgedToObjC_KeyEnumerator_FastEnumeration_Empty() {
  let d = getBridgedEmptyNSDictionary()

  var pairs = slurpFastEnumeration(d, d.keyEnumerator())
  assert(equalsUnordered(pairs, []))

  pairs = slurpFastEnumerationFromObjC(d, d.keyEnumerator())
  assert(equalsUnordered(pairs, []))

  println("test_BridgedToObjC_KeyEnumerator_FastEnumeration_Empty done")
}
test_BridgedToObjC_KeyEnumerator_FastEnumeration_Empty()
// CHECK: test_BridgedToObjC_KeyEnumerator_FastEnumeration_Empty done


func test_BridgedToObjC_FastEnumeration() {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()

  var pairs = slurpFastEnumeration(d, d)
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  pairs = slurpFastEnumerationFromObjC(d, d)
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  println("test_BridgedToObjC_FastEnumeration done")
}
test_BridgedToObjC_FastEnumeration()
// CHECK: test_BridgedToObjC_FastEnumeration done

func test_BridgedToObjC_FastEnumeration_Empty() {
  let d = getBridgedEmptyNSDictionary()

  var pairs = slurpFastEnumeration(d, d)
  assert(equalsUnordered(pairs, []))

  pairs = slurpFastEnumerationFromObjC(d, d)
  assert(equalsUnordered(pairs, []))

  println("test_BridgedToObjC_FastEnumeration_Empty done")
}
test_BridgedToObjC_FastEnumeration_Empty()
// CHECK: test_BridgedToObjC_FastEnumeration_Empty done


//===---
// Dictionary -> NSDictionary bridging tests.
//
// Key type and value type are bridged non-verbatim.
//===---

var bridgedKeyCount = 0
var bridgedKeySerial = 0

struct TestBridgedKeyTy : Equatable, Hashable, Printable, _BridgedToObjectiveC {
  init(_ value: Int) {
    ++bridgedKeyCount
    serial = ++bridgedKeySerial
    self.value = value
    self._hashValue = value
  }

  var description: String {
    assert(serial > 0, "dead TestBridgedKeyTy")
    return value.description
  }

  var hashValue: Int {
    return _hashValue
  }

  static func getObjectiveCType() -> Any.Type {
    return TestObjCKeyTy.self
  }

  func bridgeToObjectiveC() -> TestObjCKeyTy {
    return TestObjCKeyTy(value)
  }

  static func bridgeFromObjectiveC(x: TestObjCKeyTy) -> TestBridgedKeyTy? {
    fatal("implement")
  }

  var value: Int
  var _hashValue: Int
  var serial: Int
}

func == (lhs: TestBridgedKeyTy, rhs: TestBridgedKeyTy) -> Bool {
  return lhs.value == rhs.value
}

var bridgedValueCount = 0
var bridgedValueSerial = 0

struct TestBridgedValueTy : Printable, _BridgedToObjectiveC {
  init(_ value: Int) {
    ++bridgedValueCount
    serial = ++bridgedValueSerial
    self.value = value
  }

  var description: String {
    assert(serial > 0, "dead TestBridgedValueTy")
    return value.description
  }

  static func getObjectiveCType() -> Any.Type {
    return TestObjCValueTy.self
  }

  func bridgeToObjectiveC() -> TestObjCValueTy {
    return TestObjCValueTy(value)
  }

  static func bridgeFromObjectiveC(x: TestObjCValueTy) -> TestBridgedValueTy? {
    fatal("implement")
  }

  var value: Int
  var serial: Int
}

func getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged() -> NSDictionary {
  assert(!isBridgedVerbatimToObjectiveC(TestBridgedKeyTy.self))
  assert(!isBridgedVerbatimToObjectiveC(TestBridgedValueTy.self))

  var d = Dictionary<TestBridgedKeyTy, TestBridgedValueTy>()
  d[TestBridgedKeyTy(10)] = TestBridgedValueTy(1010)
  d[TestBridgedKeyTy(20)] = TestBridgedValueTy(1020)
  d[TestBridgedKeyTy(30)] = TestBridgedValueTy(1030)

  let bridged =
      reinterpretCast(_convertDictionaryToNSDictionary(d)) as NSDictionary
  assert(isCocoaNSDictionary(bridged))

  return bridged
}

func test_BridgedToObjC_KeyValue_ValueTypesCustomBridged() {
  let d = getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged()
  let enumerator = d.keyEnumerator()

  var pairs = Array<(Int, Int)>()
  while let key: AnyObject = enumerator.nextObject() {
    let value: AnyObject = d.objectForKey(key)
    pairs += ((key as TestObjCKeyTy)!.value, (value as TestObjCValueTy)!.value)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  println("test_BridgedToObjC_KeyValue_ValueTypesCustomBridged done")
}
test_BridgedToObjC_KeyValue_ValueTypesCustomBridged()
// CHECK: test_BridgedToObjC_KeyValue_ValueTypesCustomBridged done


func getBridgedNSDictionaryOfKey_ValueTypeCustomBridged() -> NSDictionary {
  assert(!isBridgedVerbatimToObjectiveC(TestBridgedKeyTy.self))
  assert(isBridgedVerbatimToObjectiveC(TestObjCValueTy.self))

  var d = Dictionary<TestBridgedKeyTy, TestObjCValueTy>()
  d[TestBridgedKeyTy(10)] = TestObjCValueTy(1010)
  d[TestBridgedKeyTy(20)] = TestObjCValueTy(1020)
  d[TestBridgedKeyTy(30)] = TestObjCValueTy(1030)

  let bridged =
      reinterpretCast(_convertDictionaryToNSDictionary(d)) as NSDictionary
  assert(isCocoaNSDictionary(bridged))

  return bridged
}

func test_BridgedToObjC_Key_ValueTypeCustomBridged() {
  let d = getBridgedNSDictionaryOfKey_ValueTypeCustomBridged()
  let enumerator = d.keyEnumerator()

  var pairs = Array<(Int, Int)>()
  while let key: AnyObject = enumerator.nextObject() {
    let value: AnyObject = d.objectForKey(key)
    pairs += ((key as TestObjCKeyTy)!.value, (value as TestObjCValueTy)!.value)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  println("test_BridgedToObjC_Key_ValueTypeCustomBridged done")
}
test_BridgedToObjC_Key_ValueTypeCustomBridged()
// CHECK: test_BridgedToObjC_Key_ValueTypeCustomBridged done


func getBridgedNSDictionaryOfValue_ValueTypeCustomBridged() -> NSDictionary {
  assert(isBridgedVerbatimToObjectiveC(TestObjCKeyTy.self))
  assert(!isBridgedVerbatimToObjectiveC(TestBridgedValueTy.self))

  var d = Dictionary<TestObjCKeyTy, TestBridgedValueTy>()
  d[TestObjCKeyTy(10)] = TestBridgedValueTy(1010)
  d[TestObjCKeyTy(20)] = TestBridgedValueTy(1020)
  d[TestObjCKeyTy(30)] = TestBridgedValueTy(1030)

  let bridged =
      reinterpretCast(_convertDictionaryToNSDictionary(d)) as NSDictionary
  assert(isCocoaNSDictionary(bridged))

  return bridged
}

func test_BridgedToObjC_Value_ValueTypeCustomBridged() {
  let d = getBridgedNSDictionaryOfValue_ValueTypeCustomBridged()
  let enumerator = d.keyEnumerator()

  var pairs = Array<(Int, Int)>()
  while let key: AnyObject = enumerator.nextObject() {
    let value: AnyObject = d.objectForKey(key)
    pairs += ((key as TestObjCKeyTy)!.value, (value as TestObjCValueTy)!.value)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  println("test_BridgedToObjC_Value_ValueTypeCustomBridged done")
}
test_BridgedToObjC_Value_ValueTypeCustomBridged()
// CHECK: test_BridgedToObjC_Value_ValueTypeCustomBridged done


//===---
// NSDictionary -> Dictionary -> NSDictionary bridging tests.
//===---

func getRoundtripBridgedNSDictionary() -> NSDictionary {
  let keys = NSMutableArray()
  keys.addObject(TestObjCKeyTy(10))
  keys.addObject(TestObjCKeyTy(20))
  keys.addObject(TestObjCKeyTy(30))

  let values = NSMutableArray()
  values.addObject(TestObjCValueTy(1010))
  values.addObject(TestObjCValueTy(1020))
  values.addObject(TestObjCValueTy(1030))

  var nsd = NSDictionary(objects: values, forKeys: keys)

  var d: Dictionary<NSObject, AnyObject> = _convertNSDictionaryToDictionary(nsd)

  let bridgedBack = _convertDictionaryToNSDictionary(d)
  assert(isCocoaNSDictionary(bridgedBack))

  return bridgedBack
}

func test_BridgingRoundtrip() {
  let d = getRoundtripBridgedNSDictionary()
  let enumerator = d.keyEnumerator()

  var pairs = Array<(Int, Int)>()
  while let key: AnyObject = enumerator.nextObject() {
    let value: AnyObject = d.objectForKey(key)
    pairs += ((key as TestObjCKeyTy)!.value, (value as TestObjCValueTy)!.value)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  println("test_BridgingRoundtrip done")
}
test_BridgingRoundtrip()
// CHECK: test_BridgingRoundtrip done

//===---
// NSDictionary -> Dictionary implicit conversion.
//===---

func test_NSDictionaryToDictionaryCoversion() {
  let keys = NSMutableArray()
  keys.addObject(TestObjCKeyTy(10))
  keys.addObject(TestObjCKeyTy(20))
  keys.addObject(TestObjCKeyTy(30))

  let values = NSMutableArray()
  values.addObject(TestObjCValueTy(1010))
  values.addObject(TestObjCValueTy(1020))
  values.addObject(TestObjCValueTy(1030))

  let nsd = NSDictionary(objects: values, forKeys: keys)

  let d: Dictionary = nsd

  var pairs = Array<(Int, Int)>()
  for (key, value: AnyObject) in d {
    pairs += ((key as TestObjCKeyTy)!.value, (value as TestObjCValueTy)!.value)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  println("test_NSDictionaryToDictionaryCoversion done")
}
test_NSDictionaryToDictionaryCoversion()
// CHECK: test_NSDictionaryToDictionaryCoversion done

func test_DictionaryToNSDictionaryCoversion() {
  var d = Dictionary<TestObjCKeyTy, TestObjCValueTy>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)
  let nsd: NSDictionary = d

  var pairs = slurpFastEnumeration(d, d)
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  println("test_DictionaryToNSDictionaryCoversion done")
}
test_DictionaryToNSDictionaryCoversion()
// CHECK: test_DictionaryToNSDictionaryCoversion done

//===---
// Tests for APIs implemented strictly based on public interface.  We only need
// to test them once, not for every storage type.
//===---

func getDerivedAPIsDictionary() -> Dictionary<Int, Int> {
  var d = Dictionary<Int, Int>(minimumCapacity: 10)
  d[10] = 1010
  d[20] = 1020
  d[30] = 1030
  return d
}

func testDerivedAPIs_keys() {
  if true {
    var empty = Dictionary<Int, Int>()
    var keys = Array(empty.keys)
    assert(equalsUnordered(keys, []))
  }
  if true {
    var d = getDerivedAPIsDictionary()
    var keys = Array(d.keys)
    assert(equalsUnordered(keys, [ 10, 20, 30 ]))
  }

  println("testDerivedAPIs_keys done")
}
testDerivedAPIs_keys()
// CHECK: testDerivedAPIs_keys done

func testDerivedAPIs_values() {
  if true {
    var empty = Dictionary<Int, Int>()
    var values = Array(empty.values)
    assert(equalsUnordered(values, []))
  }
  if true {
    var d = getDerivedAPIsDictionary()

    var values = Array(d.values)
    assert(equalsUnordered(values, [ 1010, 1020, 1030 ]))

    d[11] = 1010
    values = Array(d.values)
    assert(equalsUnordered(values, [ 1010, 1010, 1020, 1030 ]))
  }

  println("testDerivedAPIs_values done")
}
testDerivedAPIs_values()
// CHECK: testDerivedAPIs_values done

//===---
// Misc tests.
//===---

// Dictionary literal
var dict = ["Hello" : 1, "World" : 2]

// CHECK: testing...
println("testing...")

// Insertion
dict["Swift"] = 3

// Access
// CHECK-NEXT: "Hello" => 1
print("\"Hello\" => " + String(dict["Hello"]!) + "\n")
// CHECK-NEXT: "Swift" => 3
print("\"Swift\" => " + String(dict["Swift"]!) + "\n")
// CHECK-NEXT: "World" => 2
print("\"World\" => " + String(dict["World"]!) + "\n")
// CHECK-NEXT: "World" => true
print("\"World\" => " + String(dict["World"].getLogicValue()) + "\n")
// CHECK-NEXT: "Universe" => false
print("\"Universe\" => " + String(dict["Universe"].getLogicValue()) + "\n")

// Overwriting existing value
dict["Hello"] = 0
// CHECK-NEXT: "Hello" => 0
print("\"Hello\" => " + String(dict["Hello"]!) + "\n")
// CHECK-NEXT: "Swift" => 3
print("\"Swift\" => " + String(dict["Swift"]!) + "\n")
// CHECK-NEXT: "World" => 2
print("\"World\" => " + String(dict["World"]!) + "\n")

// Dictionaries with other types
var d2 = [1.2 : 1, 2.6 : 3]
d2[3.3] = 3
// CHECK-NEXT: d2[1.2] = 1
print("d2[1.2] = \(d2[1.2]!)\n")
// CHECK-NEXT: d2[2.6] = 3
print("d2[2.6] = \(d2[2.6]!)\n")
// CHECK-NEXT: d2[3.3] = 3
print("d2[3.3] = \(d2[3.3]!)\n")

var d = Dictionary<String, Int>(minimumCapacity: 13)
d["one"] = 1
d["two"] = 2
d["three"] = 3
d["four"] = 4
d["five"] = 5
// CHECK-NEXT: d: <12345>
print("d: <")
print(d["one"]!)
print(d["two"]!)
print(d["three"]!)
print(d["four"]!)
print(d["five"]!)
println(">")

// Iterate over (key, value) tuples as a silly copy
var d3 = Dictionary<String,Int>(minimumCapacity: 13)

for (k, v) in d {
  d3[k] = v
}

// CHECK-NEXT: d3: <12345>
print("d3: <")
print(d["one"]!)
print(d["two"]!)
print(d["three"]!)
print(d["four"]!)
print(d["five"]!)
println(">")

// CHECK-NEXT: four = 4
if let four = find(d.keys, "four") {
  println("four = \(d.values[four])")
}
else {
  println("four not found")
}

// CHECK-NEXT: 3 = three
if let three = find(d.values, 3) {
  println("3 = \(d.keys[three])")
}
else {
  println("three not found")
}
