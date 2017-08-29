// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// XFAIL: interpret

import StdlibUnittest

var OptionSetTests = TestSuite("OptionSetTests")

struct Days: OptionSet {
  let rawValue: Int
  
  static let sunday = Days(rawValue: 1 << 0)
  static let monday = Days(rawValue: 1 << 1)
  static let tuesday = Days(rawValue: 1 << 2)
  static let wednesday = Days(rawValue: 1 << 3)
  static let thursday = Days(rawValue: 1 << 4)
  static let friday = Days(rawValue: 1 << 5)
  static let saturday = Days(rawValue: 1 << 6)
}

// Test rawValue

OptionSetTests.test("RawValue.SingleOption") {
  let s = Days.friday
  
  assert(s.rawValue == 32)
}

OptionSetTests.test("RawValue.MultipleOptions") {
  let s: Days = [.monday, .tuesday, .wednesday, .thursday, .friday]
  
  assert(s.rawValue == 62)
}

OptionSetTests.test("RawValue.EmptySet") {
  var s: Days = []
  
  assert(s.rawValue == 0)
  
  s.insert(.monday)
  
  assert(s.rawValue == 2)
}

// Test isEmpty

OptionSetTests.test("IsEmpty.SingleOption") {
  let s = Days.friday
  
  assert(s.isEmpty == false)
}

OptionSetTests.test("IsEmpty.MultipleOptions") {
  let s: Days = [.monday, .tuesday, .wednesday, .thursday, .friday]
  
  assert(s.isEmpty == false)
}

OptionSetTests.test("IsEmpty.EmptySet") {
  var s: Days = []
  
  assert(s.isEmpty)
  
  s.insert(.monday)
  
  assert(s.isEmpty == false)
}

// Test equality operator

OptionSetTests.test("==.SingleOption") {
  let s1 = Days.monday
  let s2 = Days.monday
  let s3: Days = [.monday, .tuesday, .wednesday]

  expectEqual(s1, s2)
  expectNotEqual(s1, s3)
}

OptionSetTests.test("==.MultipleOptions") {
  let s1: Days = [.monday, .tuesday, .wednesday]
  let s2: Days = [.monday, .tuesday, .wednesday]
  let s3: Days = [.wednesday, .thursday, .friday]

  expectEqual(s1, s2)
  expectNotEqual(s1, s3)
}

OptionSetTests.test("==.EmptySet") {
  let s1: Days = []
  let s2: Days = []
  let s3: Days = [.monday, .tuesday, .wednesday]

  expectEqual(s1, s2)
  expectNotEqual(s1, s3)
}

// Test contains()

OptionSetTests.test("Contains.SingleOption") {
  let s = Days.friday
  
  assert(s.contains(.sunday) == false)
  assert(s.contains(.monday) == false)
  assert(s.contains(.tuesday) == false)
  assert(s.contains(.wednesday) == false)
  assert(s.contains(.thursday) == false)
  assert(s.contains(.friday))
  assert(s.contains(.saturday) == false)
}

OptionSetTests.test("Contains.MultipleOptions") {
  let s: Days = [.monday, .tuesday, .wednesday, .thursday, .friday]
  
  assert(s.contains(.sunday) == false)
  assert(s.contains(.monday))
  assert(s.contains(.tuesday))
  assert(s.contains(.wednesday))
  assert(s.contains(.thursday))
  assert(s.contains(.friday))
  assert(s.contains(.saturday) == false)
}

OptionSetTests.test("Contains.EmptySet") {
  let s: Days = []
  
  assert(s.contains(.sunday) == false)
  assert(s.contains(.monday) == false)
  assert(s.contains(.tuesday) == false)
  assert(s.contains(.wednesday) == false)
  assert(s.contains(.thursday) == false)
  assert(s.contains(.friday) == false)
  assert(s.contains(.saturday) == false)
}

// Test formItersection()

OptionSetTests.test("FormIntersection.SingleOption") {
  do {
    var s1 = Days.monday
    let s2: Days = [.monday, .tuesday, .wednesday]
  
    s1.formIntersection(s2)
    assert(s1.contains(.monday))
    assert(s1.contains(.tuesday) == false)
    assert(s1.contains(.sunday) == false)
  }
  do {
    var s1 = Days.monday
    let s2: Days = [.tuesday, .wednesday, .friday]
  
    s1.formIntersection(s2)
    assert(s1.contains(.monday) == false)
    assert(s1.contains(.tuesday) == false)
    assert(s1.contains(.sunday) == false)
  }
}

OptionSetTests.test("FormIntersection.MultipleOptions") {
  do {
    var s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.wednesday, .thursday, .friday]
  
    s1.formIntersection(s2)
    assert(s1.contains(.wednesday))
    assert(s1.contains(.tuesday) == false)
    assert(s1.contains(.sunday) == false)
  }
  do {
    var s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.thursday, .friday, .saturday]
  
    s1.formIntersection(s2)
    assert(s1.contains(.wednesday) == false)
    assert(s1.contains(.thursday) == false)
    assert(s1.contains(.sunday) == false)
  }
}

OptionSetTests.test("FormIntersection.EmptySet") {
  var s1: Days = []
  let s2: Days = [.monday, .tuesday, .wednesday]

  s1.formIntersection(s2)
  assert(s1.contains(.wednesday) == false)
  assert(s1.contains(.thursday) == false)
}

// Test formSymmetricDifference()

OptionSetTests.test("FormSymmetricDifference.SingleOption") {
  do {
    var s1 = Days.monday
    let s2: Days = [.monday, .tuesday, .wednesday]
  
    s1.formSymmetricDifference(s2)
    assert(s1.contains(.monday) == false)
    assert(s1.contains(.tuesday))
    assert(s1.contains(.sunday) == false)
  }
  do {
    var s1 = Days.monday
    let s2: Days = [.tuesday, .wednesday, .friday]
  
    s1.formSymmetricDifference(s2)
    assert(s1.contains(.monday))
    assert(s1.contains(.tuesday))
    assert(s1.contains(.sunday) == false)
  }
}

OptionSetTests.test("FormSymmetricDifference.MultipleOptions") {
  do {
    var s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.wednesday, .thursday, .friday]
  
    s1.formSymmetricDifference(s2)
    assert(s1.contains(.wednesday) == false)
    assert(s1.contains(.tuesday))
    assert(s1.contains(.sunday) == false)
  }
  do {
    var s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.thursday, .friday, .saturday]
  
    s1.formSymmetricDifference(s2)
    assert(s1.contains(.wednesday))
    assert(s1.contains(.thursday))
    assert(s1.contains(.sunday) == false)
  }
}

OptionSetTests.test("FormSymmetricDifference.EmptySet") {
  var s1: Days = []
  let s2: Days = [.monday, .tuesday, .wednesday]

  s1.formSymmetricDifference(s2)
  assert(s1.contains(.wednesday))
  assert(s1.contains(.thursday) == false)
}

// Test formUnion()

OptionSetTests.test("FormUnion.SingleOption") {
  do {
    var s1 = Days.monday
    let s2: Days = [.monday, .tuesday, .wednesday]
  
    s1.formUnion(s2)
    assert(s1.contains(.monday))
    assert(s1.contains(.tuesday))
    assert(s1.contains(.sunday) == false)
  }
  do {
    var s1 = Days.monday
    let s2: Days = [.tuesday, .wednesday, .friday]
  
    s1.formUnion(s2)
    assert(s1.contains(.monday))
    assert(s1.contains(.tuesday))
    assert(s1.contains(.sunday) == false)
  }
}

OptionSetTests.test("FormUnion.MultipleOptions") {
  do {
    var s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.wednesday, .thursday, .friday]
  
    s1.formUnion(s2)
    assert(s1.contains(.wednesday))
    assert(s1.contains(.tuesday))
    assert(s1.contains(.sunday) == false)
  }
  do {
    var s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.thursday, .friday, .saturday]
  
    s1.formUnion(s2)
    assert(s1.contains(.wednesday))
    assert(s1.contains(.thursday))
    assert(s1.contains(.sunday) == false)
  }
}

OptionSetTests.test("FormUnion.EmptySet") {
  var s1: Days = []
  let s2: Days = [.monday, .tuesday, .wednesday]

  s1.formUnion(s2)
  assert(s1.contains(.wednesday))
  assert(s1.contains(.thursday) == false)
}

// Test insert()

OptionSetTests.test("Insert.SingleOption") {
  var s = Days.monday
  
  assert(s.contains(.tuesday) == false)
  
  let (inserted1, member1) = s.insert(.tuesday)

  assert(s.contains(.monday))
  assert(s.contains(.tuesday))
  assert(s.contains(.sunday) == false)
  assert(inserted1)
  expectEqual(member1, .tuesday)
  
  let (inserted2, member2) = s.insert(.tuesday)
  assert(inserted2 == false)
  expectEqual(member2, .tuesday)
}

OptionSetTests.test("Insert.MultipleOptions") {
  var s: Days = [.monday, .tuesday, .wednesday]
  
  assert(s.contains(.friday) == false)
  
  let (inserted1, member1) = s.insert(.friday)
  
  assert(s.contains(.monday))
  assert(s.contains(.friday))
  assert(s.contains(.sunday) == false)
  assert(inserted1)
  expectEqual(member1, .friday)
  
  let (inserted2, member2) = s.insert(.friday)
  assert(inserted2 == false)
  expectEqual(member2, .friday)
}

OptionSetTests.test("Insert.EmptySet") {
  var s: Days = []

  assert(s.contains(.monday) == false)
  
  let (inserted1, member1) = s.insert(.monday)
  
  assert(s.contains(.monday))
  assert(inserted1)
  expectEqual(member1, .monday)
  
  let (inserted2, member2) = s.insert(.monday)
  assert(inserted2 == false)
  expectEqual(member2, .monday)
}

// Test intersection()

OptionSetTests.test("Intersection.SingleOption") {
  do {
    let s1 = Days.monday
    let s2: Days = [.monday, .tuesday, .wednesday]
  
    let intersection = s1.intersection(s2)
    assert(intersection.contains(.monday))
    assert(intersection.contains(.tuesday) == false)
    assert(intersection.contains(.sunday) == false)
  }
  do {
    let s1 = Days.monday
    let s2: Days = [.tuesday, .wednesday, .friday]
  
    let intersection = s1.intersection(s2)
    assert(intersection.contains(.monday) == false)
    assert(intersection.contains(.tuesday) == false)
    assert(intersection.contains(.sunday) == false)
  }
}

OptionSetTests.test("Intersection.MultipleOptions") {
  do {
    let s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.wednesday, .thursday, .friday]
  
    let intersection = s1.intersection(s2)
    assert(intersection.contains(.wednesday))
    assert(intersection.contains(.tuesday) == false)
    assert(intersection.contains(.sunday) == false)
  }
  do {
    let s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.thursday, .friday, .saturday]
  
    let intersection = s1.intersection(s2)
    assert(intersection.contains(.wednesday) == false)
    assert(intersection.contains(.thursday) == false)
    assert(intersection.contains(.sunday) == false)
  }
}

OptionSetTests.test("Intersection.EmptySet") {
  let s1: Days = []
  let s2: Days = [.monday, .tuesday, .wednesday]

  let intersection = s1.intersection(s2)
  assert(intersection.contains(.wednesday) == false)
  assert(intersection.contains(.thursday) == false)
}

// Test isDisjointUnion(with:)

OptionSetTests.test("IsDisjointWith.SingleOption") {
  let s1 = Days.monday
  let s2: Days = [.monday, .tuesday, .wednesday]
  let s3: Days = [.tuesday, .wednesday]

  assert(s1.isDisjoint(with: s2) == false)
  assert(s1.isDisjoint(with: s3))
}

OptionSetTests.test("IsDisjointWith.MultipleOptions") {
  let s1: Days = [.monday, .tuesday, .wednesday]
  let s2: Days = [.tuesday, .wednesday]
  let s3: Days = [.thursday, .friday, .saturday]

  assert(s1.isDisjoint(with: s2) == false)
  assert(s1.isDisjoint(with: s3))
}

OptionSetTests.test("IsDisjointWith.EmptySet") {
  let s1: Days = []
  let s2: Days = [.tuesday, .wednesday]

  assert(s1.isDisjoint(with: s2))
}

// Test isSubset(of:)

OptionSetTests.test("IsSubsetOf.SingleOption") {
  let s1 = Days.monday
  let s2: Days = [.monday, .tuesday, .wednesday]
  let s3: Days = [.tuesday, .wednesday]

  assert(s1.isSubset(of: s2))
  assert(s1.isSubset(of: s3) == false)
}

OptionSetTests.test("IsSubsetOf.MultipleOptions") {
  let s1: Days = [.monday, .tuesday]
  let s2: Days = [.monday, .tuesday, .wednesday]
  let s3: Days = [.thursday, .friday, .saturday]

  assert(s1.isSubset(of: s2))
  assert(s1.isSubset(of: s3) == false)
}

OptionSetTests.test("IsSubsetOf.EmptySet") {
  let s1: Days = []
  let s2: Days = [.tuesday, .wednesday]

  assert(s1.isSubset(of: s2))
}

// Test isSuperset(of:)

OptionSetTests.test("IsSupersetOf.SingleOption") {
  let s1 = Days.monday
  let s2: Days = [.monday]
  let s3: Days = [.tuesday, .wednesday]
  let s4: Days = []

  assert(s1.isSuperset(of: s2))
  assert(s1.isSuperset(of: s3) == false)
  assert(s1.isSuperset(of: s4))
}

OptionSetTests.test("IsSupersetOf.MultipleOptions") {
  let s1: Days = [.monday, .tuesday]
  let s2: Days = [.monday, .tuesday]
  let s3: Days = [.monday]
  let s4: Days = [.thursday, .friday, .saturday]

  assert(s1.isSuperset(of: s2))
  assert(s1.isSuperset(of: s3))
  assert(s1.isSuperset(of: s4) == false)
}

OptionSetTests.test("IsSupersetOf.EmptySet") {
  let s1: Days = []
  let s2: Days = [.tuesday, .wednesday]
  let s3: Days = []

  assert(s1.isSuperset(of: s2) == false)
  assert(s1.isSuperset(of: s3))
}

// Test remove()

OptionSetTests.test("Remove.SingleOption") {
  var s = Days.monday
  
  assert(s.contains(.monday))
  
  let removed = s.remove(.monday)

  assert(s.contains(.monday) == false)
  assert(s.contains(.sunday) == false)
  expectEqual(removed, .monday)
}

OptionSetTests.test("Remove.MultipleOptions") {
  var s: Days = [.monday, .tuesday, .wednesday]
  
  assert(s.contains(.tuesday))
  
  let removed = s.remove(.tuesday)
  
  assert(s.contains(.monday))
  assert(s.contains(.tuesday) == false)
  assert(s.contains(.sunday) == false)
  expectEqual(removed, .tuesday)
}

OptionSetTests.test("Remove.EmptySet") {
  var s: Days = []

  assert(s.contains(.monday) == false)
  
  let removed = s.remove(.monday)
  
  assert(s.contains(.monday) == false)
  expectEqual(removed, nil)
}

// Test subtract()

OptionSetTests.test("Subtract.SingleOption") {
  do {
    var s = Days.monday
    
    s.subtract([.monday])
    
    assert(s.contains(.monday) == false)
  }
  do {
    var s = Days.monday
    
    s.subtract([.tuesday])
    
    assert(s.contains(.monday))
  }
}

OptionSetTests.test("Subtract.MultipleOptions") {
  do {
    var s: Days = [.monday, .tuesday, .wednesday]
    
    s.subtract([.monday])
    
    assert(s.contains(.monday) == false)
    assert(s.contains(.tuesday))
  }
  do {
    var s: Days = [.monday, .tuesday, .wednesday]
    
    s.subtract([.friday])
    
    assert(s.contains(.monday))
    assert(s.contains(.friday) == false)
  }
}

OptionSetTests.test("Subtract.EmptySet") {
  var s: Days = []
  
  s.subtract([.monday])
  
  assert(s.contains(.monday) == false)
  assert(s.contains(.tuesday) == false)
}

// Test subtracting()

OptionSetTests.test("Subtracting.SingleOption") {
  do {
    let s = Days.monday
    
    let difference = s.subtracting([.monday])
    
    assert(difference.contains(.monday) == false)
  }
  do {
    let s = Days.monday
    
    let difference = s.subtracting([.tuesday])
    
    assert(difference.contains(.monday))
  }
}

OptionSetTests.test("Subtracting.MultipleOptions") {
  do {
    let s: Days = [.monday, .tuesday, .wednesday]
    
    let difference = s.subtracting([.monday])
    
    assert(difference.contains(.monday) == false)
    assert(difference.contains(.tuesday))
  }
  do {
    let s: Days = [.monday, .tuesday, .wednesday]
    
    let difference = s.subtracting([.friday])
    
    assert(difference.contains(.monday))
    assert(difference.contains(.friday) == false)
  }
}

OptionSetTests.test("Subtracting.EmptySet") {
  let s: Days = []
  
  let difference = s.subtracting([.monday])
  
  assert(difference.contains(.monday) == false)
  assert(difference.contains(.tuesday) == false)
}

// Test symmetricDifference()

OptionSetTests.test("SymmetricDifference.SingleOption") {
  do {
    let s1 = Days.monday
    let s2: Days = [.monday, .tuesday, .wednesday]
  
    let difference = s1.symmetricDifference(s2)
    assert(difference.contains(.monday) == false)
    assert(difference.contains(.tuesday))
    assert(difference.contains(.sunday) == false)
  }
  do {
    let s1 = Days.monday
    let s2: Days = [.tuesday, .wednesday, .friday]
  
    let difference = s1.symmetricDifference(s2)
    assert(difference.contains(.monday))
    assert(difference.contains(.tuesday))
    assert(difference.contains(.sunday) == false)
  }
}

OptionSetTests.test("SymmetricDifference.MultipleOptions") {
  do {
    let s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.wednesday, .thursday, .friday]
  
    let difference = s1.symmetricDifference(s2)
    assert(difference.contains(.wednesday) == false)
    assert(difference.contains(.tuesday))
    assert(difference.contains(.sunday) == false)
  }
  do {
    let s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.thursday, .friday, .saturday]
  
    let difference = s1.symmetricDifference(s2)
    assert(difference.contains(.wednesday))
    assert(difference.contains(.thursday))
    assert(difference.contains(.sunday) == false)
  }
}

OptionSetTests.test("SymmetricDifference.EmptySet") {
  let s1: Days = []
  let s2: Days = [.monday, .tuesday, .wednesday]

  let difference = s1.symmetricDifference(s2)
  assert(difference.contains(.wednesday))
  assert(difference.contains(.thursday) == false)
}

// Test union()

OptionSetTests.test("Union.SingleOption") {
  do {
    let s1 = Days.monday
    let s2: Days = [.monday, .tuesday, .wednesday]
  
    let union = s1.union(s2)
    assert(union.contains(.monday))
    assert(union.contains(.tuesday))
    assert(union.contains(.sunday) == false)
  }
  do {
    let s1 = Days.monday
    let s2: Days = [.tuesday, .wednesday, .friday]
  
    let union = s1.union(s2)
    assert(union.contains(.monday))
    assert(union.contains(.tuesday))
    assert(union.contains(.sunday) == false)
  }
}

OptionSetTests.test("Union.MultipleOptions") {
  do {
    let s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.wednesday, .thursday, .friday]
  
    let union = s1.union(s2)
    assert(union.contains(.wednesday))
    assert(union.contains(.tuesday))
    assert(union.contains(.sunday) == false)
  }
  do {
    let s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.thursday, .friday, .saturday]
  
    let union = s1.union(s2)
    assert(union.contains(.wednesday))
    assert(union.contains(.thursday))
    assert(union.contains(.sunday) == false)
  }
}

OptionSetTests.test("Union.EmptySet") {
  let s1: Days = []
  let s2: Days = [.monday, .tuesday, .wednesday]

  let union = s1.union(s2)
  assert(union.contains(.wednesday))
  assert(union.contains(.thursday) == false)
}

// Test update(with:)

OptionSetTests.test("UpdateWith.SingleOption") {
  var s = Days.monday
  
  assert(s.contains(.tuesday) == false)
  
  let member1 = s.update(with: .tuesday)

  assert(s.contains(.monday))
  assert(s.contains(.tuesday))
  assert(s.contains(.sunday) == false)
  expectEqual(member1, nil)
  
  let member2 = s.update(with: .tuesday)
  expectEqual(member2, .tuesday)
}

OptionSetTests.test("UpdateWith.MultipleOptions") {
  var s: Days = [.monday, .tuesday, .wednesday]
  
  assert(s.contains(.friday) == false)
  
  let member1 = s.update(with: .friday)
  
  assert(s.contains(.monday))
  assert(s.contains(.friday))
  assert(s.contains(.sunday) == false)
  expectEqual(member1, nil)
  
  let member2 = s.update(with: .friday)
  expectEqual(member2, .friday)
}

OptionSetTests.test("UpdateWith.EmptySet") {
  var s: Days = []

  assert(s.contains(.monday) == false)
  
  let member1 = s.update(with: .monday)
  
  assert(s.contains(.monday))
  expectEqual(member1, nil)
  
  let member2 = s.update(with: .monday)
  expectEqual(member2, .monday)
}

runAllTests()
