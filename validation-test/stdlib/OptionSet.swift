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
  
  expectEqual(32, s.rawValue)
}

OptionSetTests.test("RawValue.MultipleOptions") {
  let s: Days = [.monday, .tuesday, .wednesday, .thursday, .friday]
  
  expectEqual(62, s.rawValue)
}

OptionSetTests.test("RawValue.EmptySet") {
  var s: Days = []
  
  expectEqual(0, s.rawValue)
  
  s.insert(.monday)
  
  expectEqual(2, s.rawValue)
}

// Test isEmpty

OptionSetTests.test("IsEmpty.SingleOption") {
  let s = Days.friday
  
  expectFalse(s.isEmpty)
}

OptionSetTests.test("IsEmpty.MultipleOptions") {
  let s: Days = [.monday, .tuesday, .wednesday, .thursday, .friday]
  
  expectFalse(s.isEmpty)
}

OptionSetTests.test("IsEmpty.EmptySet") {
  var s: Days = []
  
  expectTrue(s.isEmpty)
  
  s.insert(.monday)
  
  expectFalse(s.isEmpty)
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
  
  expectFalse(s.contains(.sunday))
  expectFalse(s.contains(.monday))
  expectFalse(s.contains(.tuesday))
  expectFalse(s.contains(.wednesday))
  expectFalse(s.contains(.thursday))
  expectFalse(s.contains(.saturday))
  expectTrue(s.contains(.friday))
}

OptionSetTests.test("Contains.MultipleOptions") {
  let s: Days = [.monday, .tuesday, .wednesday, .thursday, .friday]
  
  expectFalse(s.contains(.sunday))
  expectTrue(s.contains(.monday))
  expectTrue(s.contains(.tuesday))
  expectTrue(s.contains(.wednesday))
  expectTrue(s.contains(.thursday))
  expectTrue(s.contains(.friday))
  expectFalse(s.contains(.saturday))
}

OptionSetTests.test("Contains.EmptySet") {
  let s: Days = []
  
  expectFalse(s.contains(.sunday))
  expectFalse(s.contains(.monday))
  expectFalse(s.contains(.tuesday))
  expectFalse(s.contains(.wednesday))
  expectFalse(s.contains(.thursday))
  expectFalse(s.contains(.friday))
  expectFalse(s.contains(.saturday))
}

// Test formItersection()

OptionSetTests.test("FormIntersection.SingleOption") {
  do {
    var s1 = Days.monday
    let s2: Days = [.monday, .tuesday, .wednesday]
  
    s1.formIntersection(s2)
    
    expectTrue(s1.contains(.monday))
    expectFalse(s1.contains(.tuesday))
    expectFalse(s1.contains(.sunday))
  }
  do {
    var s1 = Days.monday
    let s2: Days = [.tuesday, .wednesday, .friday]
  
    s1.formIntersection(s2)
    
    expectFalse(s1.contains(.monday))
    expectFalse(s1.contains(.tuesday))
    expectFalse(s1.contains(.sunday))
  }
}

OptionSetTests.test("FormIntersection.MultipleOptions") {
  do {
    var s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.wednesday, .thursday, .friday]
  
    s1.formIntersection(s2)
    
    expectTrue(s1.contains(.wednesday))
    expectFalse(s1.contains(.tuesday))
    expectFalse(s1.contains(.sunday))
  }
  do {
    var s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.thursday, .friday, .saturday]
  
    s1.formIntersection(s2)
    
    expectFalse(s1.contains(.wednesday))
    expectFalse(s1.contains(.thursday))
    expectFalse(s1.contains(.sunday))
  }
}

OptionSetTests.test("FormIntersection.EmptySet") {
  var s1: Days = []
  let s2: Days = [.monday, .tuesday, .wednesday]

  s1.formIntersection(s2)
  
  expectFalse(s1.contains(.wednesday))
  expectFalse(s1.contains(.thursday))
}

// Test formSymmetricDifference()

OptionSetTests.test("FormSymmetricDifference.SingleOption") {
  do {
    var s1 = Days.monday
    let s2: Days = [.monday, .tuesday, .wednesday]
  
    s1.formSymmetricDifference(s2)

    expectTrue(s1.contains(.tuesday))
    expectFalse(s1.contains(.monday))
    expectFalse(s1.contains(.sunday))
  }
  do {
    var s1 = Days.monday
    let s2: Days = [.tuesday, .wednesday, .friday]
  
    s1.formSymmetricDifference(s2)
    
    expectTrue(s1.contains(.monday))
    expectTrue(s1.contains(.tuesday))
    expectFalse(s1.contains(.sunday))
  }
}

OptionSetTests.test("FormSymmetricDifference.MultipleOptions") {
  do {
    var s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.wednesday, .thursday, .friday]
  
    s1.formSymmetricDifference(s2)

    expectTrue(s1.contains(.tuesday))
    expectFalse(s1.contains(.wednesday))
    expectFalse(s1.contains(.sunday))
  }
  do {
    var s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.thursday, .friday, .saturday]
  
    s1.formSymmetricDifference(s2)
    expectTrue(s1.contains(.wednesday))
    expectTrue(s1.contains(.thursday))
    expectFalse(s1.contains(.sunday))
  }
}

OptionSetTests.test("FormSymmetricDifference.EmptySet") {
  var s1: Days = []
  let s2: Days = [.monday, .tuesday, .wednesday]

  s1.formSymmetricDifference(s2)
  expectTrue(s1.contains(.wednesday))
  expectFalse(s1.contains(.thursday))
}

// Test formUnion()

OptionSetTests.test("FormUnion.SingleOption") {
  do {
    var s1 = Days.monday
    let s2: Days = [.monday, .tuesday, .wednesday]
  
    s1.formUnion(s2)
    expectTrue(s1.contains(.monday))
    expectTrue(s1.contains(.tuesday))
    expectFalse(s1.contains(.sunday))
  }
  do {
    var s1 = Days.monday
    let s2: Days = [.tuesday, .wednesday, .friday]
  
    s1.formUnion(s2)
    expectTrue(s1.contains(.monday))
    expectTrue(s1.contains(.tuesday))
    expectFalse(s1.contains(.sunday))
  }
}

OptionSetTests.test("FormUnion.MultipleOptions") {
  do {
    var s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.wednesday, .thursday, .friday]
  
    s1.formUnion(s2)
    expectTrue(s1.contains(.wednesday))
    expectTrue(s1.contains(.tuesday))
    expectFalse(s1.contains(.sunday))
  }
  do {
    var s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.thursday, .friday, .saturday]
  
    s1.formUnion(s2)
    expectTrue(s1.contains(.wednesday))
    expectTrue(s1.contains(.thursday))
    expectFalse(s1.contains(.sunday))
  }
}

OptionSetTests.test("FormUnion.EmptySet") {
  var s1: Days = []
  let s2: Days = [.monday, .tuesday, .wednesday]

  s1.formUnion(s2)
  expectTrue(s1.contains(.wednesday))
  expectFalse(s1.contains(.thursday))
}

// Test insert()

OptionSetTests.test("Insert.SingleOption") {
  var s = Days.monday
  
  expectFalse(s.contains(.tuesday))
  
  let (inserted1, member1) = s.insert(.tuesday)

  expectTrue(s.contains(.monday))
  expectTrue(s.contains(.tuesday))
  expectFalse(s.contains(.sunday))
  expectTrue(inserted1)
  expectEqual(.tuesday, member1)
  
  let (inserted2, member2) = s.insert(.tuesday)
  expectFalse(inserted2)
  expectEqual(.tuesday, member2)
}

OptionSetTests.test("Insert.MultipleOptions") {
  var s: Days = [.monday, .tuesday, .wednesday]
  
  expectFalse(s.contains(.friday))
  
  let (inserted1, member1) = s.insert(.friday)
  
  expectTrue(s.contains(.monday))
  expectTrue(s.contains(.friday))
  expectFalse(s.contains(.sunday))
  expectTrue(inserted1)
  expectEqual(.friday, member1)
  
  let (inserted2, member2) = s.insert(.friday)
  expectFalse(inserted2)
  expectEqual(.friday, member2)
}

OptionSetTests.test("Insert.EmptySet") {
  var s: Days = []

  expectFalse(s.contains(.monday))
  
  let (inserted1, member1) = s.insert(.monday)
  
  expectTrue(s.contains(.monday))
  expectTrue(inserted1)
  expectEqual(.monday, member1)
  
  let (inserted2, member2) = s.insert(.monday)
  expectFalse(inserted2)
  expectEqual(.monday, member2)
}

// Test intersection()

OptionSetTests.test("Intersection.SingleOption") {
  do {
    let s1 = Days.monday
    let s2: Days = [.monday, .tuesday, .wednesday]
  
    let intersection = s1.intersection(s2)
    expectTrue(intersection.contains(.monday))
    expectFalse(intersection.contains(.tuesday))
    expectFalse(intersection.contains(.sunday))
  }
  do {
    let s1 = Days.monday
    let s2: Days = [.tuesday, .wednesday, .friday]
  
    let intersection = s1.intersection(s2)
    expectFalse(intersection.contains(.monday))
    expectFalse(intersection.contains(.tuesday))
    expectFalse(intersection.contains(.sunday))
  }
}

OptionSetTests.test("Intersection.MultipleOptions") {
  do {
    let s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.wednesday, .thursday, .friday]
  
    let intersection = s1.intersection(s2)
    expectTrue(intersection.contains(.wednesday))
    expectFalse(intersection.contains(.tuesday))
    expectFalse(intersection.contains(.sunday))
  }
  do {
    let s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.thursday, .friday, .saturday]
  
    let intersection = s1.intersection(s2)
    expectFalse(intersection.contains(.wednesday))
    expectFalse(intersection.contains(.thursday))
    expectFalse(intersection.contains(.sunday))
  }
}

OptionSetTests.test("Intersection.EmptySet") {
  let s1: Days = []
  let s2: Days = [.monday, .tuesday, .wednesday]

  let intersection = s1.intersection(s2)
  expectFalse(intersection.contains(.wednesday))
  expectFalse(intersection.contains(.thursday))
}

// Test isDisjointUnion(with:)

OptionSetTests.test("IsDisjointWith.SingleOption") {
  let s1 = Days.monday
  let s2: Days = [.monday, .tuesday, .wednesday]
  let s3: Days = [.tuesday, .wednesday]

  expectFalse(s1.isDisjoint(with: s2))
  expectTrue(s1.isDisjoint(with: s3))
}

OptionSetTests.test("IsDisjointWith.MultipleOptions") {
  let s1: Days = [.monday, .tuesday, .wednesday]
  let s2: Days = [.tuesday, .wednesday]
  let s3: Days = [.thursday, .friday, .saturday]

  expectFalse(s1.isDisjoint(with: s2))
  expectTrue(s1.isDisjoint(with: s3))
}

OptionSetTests.test("IsDisjointWith.EmptySet") {
  let s1: Days = []
  let s2: Days = [.tuesday, .wednesday]

  expectTrue(s1.isDisjoint(with: s2))
}

// Test isSubset(of:)

OptionSetTests.test("IsSubsetOf.SingleOption") {
  let s1 = Days.monday
  let s2: Days = [.monday, .tuesday, .wednesday]
  let s3: Days = [.tuesday, .wednesday]

  expectTrue(s1.isSubset(of: s2))
  expectFalse(s1.isSubset(of: s3))
}

OptionSetTests.test("IsSubsetOf.MultipleOptions") {
  let s1: Days = [.monday, .tuesday]
  let s2: Days = [.monday, .tuesday, .wednesday]
  let s3: Days = [.thursday, .friday, .saturday]

  expectTrue(s1.isSubset(of: s2))
  expectFalse(s1.isSubset(of: s3))
}

OptionSetTests.test("IsSubsetOf.EmptySet") {
  let s1: Days = []
  let s2: Days = [.tuesday, .wednesday]

  expectTrue(s1.isSubset(of: s2))
}

// Test isSuperset(of:)

OptionSetTests.test("IsSupersetOf.SingleOption") {
  let s1 = Days.monday
  let s2: Days = [.monday]
  let s3: Days = [.tuesday, .wednesday]
  let s4: Days = []

  expectTrue(s1.isSuperset(of: s2))
  expectFalse(s1.isSuperset(of: s3))
  expectTrue(s1.isSuperset(of: s4))
}

OptionSetTests.test("IsSupersetOf.MultipleOptions") {
  let s1: Days = [.monday, .tuesday]
  let s2: Days = [.monday, .tuesday]
  let s3: Days = [.monday]
  let s4: Days = [.thursday, .friday, .saturday]

  expectTrue(s1.isSuperset(of: s2))
  expectTrue(s1.isSuperset(of: s3))
  expectFalse(s1.isSuperset(of: s4))
}

OptionSetTests.test("IsSupersetOf.EmptySet") {
  let s1: Days = []
  let s2: Days = [.tuesday, .wednesday]
  let s3: Days = []

  expectFalse(s1.isSuperset(of: s2))
  expectTrue(s1.isSuperset(of: s3))
}

// Test remove()

OptionSetTests.test("Remove.SingleOption") {
  var s = Days.monday
  
  expectTrue(s.contains(.monday))
  
  let removed = s.remove(.monday)

  expectFalse(s.contains(.monday))
  expectFalse(s.contains(.sunday))
  expectEqual(.monday, removed)
}

OptionSetTests.test("Remove.MultipleOptions") {
  var s: Days = [.monday, .tuesday, .wednesday]
  
  expectTrue(s.contains(.tuesday))
  
  let removed = s.remove(.tuesday)
  
  expectTrue(s.contains(.monday))
  expectFalse(s.contains(.tuesday))
  expectFalse(s.contains(.sunday))
  expectEqual(.tuesday, removed)
}

OptionSetTests.test("Remove.EmptySet") {
  var s: Days = []

  expectFalse(s.contains(.monday))
  
  let removed = s.remove(.monday)
  
  expectFalse(s.contains(.monday))
  expectNil(removed)
}

// Test subtract()

OptionSetTests.test("Subtract.SingleOption") {
  do {
    var s = Days.monday
    
    s.subtract([.monday])
    
    expectFalse(s.contains(.monday))
  }
  do {
    var s = Days.monday
    
    s.subtract([.tuesday])
    
    expectTrue(s.contains(.monday))
  }
}

OptionSetTests.test("Subtract.MultipleOptions") {
  do {
    var s: Days = [.monday, .tuesday, .wednesday]
    
    s.subtract([.monday])
    
    expectFalse(s.contains(.monday))
    expectTrue(s.contains(.tuesday))
  }
  do {
    var s: Days = [.monday, .tuesday, .wednesday]
    
    s.subtract([.friday])
    
    expectTrue(s.contains(.monday))
    expectFalse(s.contains(.friday))
  }
}

OptionSetTests.test("Subtract.EmptySet") {
  var s: Days = []
  
  s.subtract([.monday])
  
  expectFalse(s.contains(.monday))
  expectFalse(s.contains(.tuesday))
}

// Test subtracting()

OptionSetTests.test("Subtracting.SingleOption") {
  do {
    let s = Days.monday
    
    let difference = s.subtracting([.monday])
    
    expectFalse(difference.contains(.monday))
  }
  do {
    let s = Days.monday
    
    let difference = s.subtracting([.tuesday])
    
    expectTrue(difference.contains(.monday))
  }
}

OptionSetTests.test("Subtracting.MultipleOptions") {
  do {
    let s: Days = [.monday, .tuesday, .wednesday]
    
    let difference = s.subtracting([.monday])
    
    expectFalse(difference.contains(.monday))
    expectTrue(difference.contains(.tuesday))
  }
  do {
    let s: Days = [.monday, .tuesday, .wednesday]
    
    let difference = s.subtracting([.friday])
    
    expectTrue(difference.contains(.monday))
    expectFalse(difference.contains(.friday))
  }
}

OptionSetTests.test("Subtracting.EmptySet") {
  let s: Days = []
  
  let difference = s.subtracting([.monday])
  
  expectFalse(difference.contains(.monday))
  expectFalse(difference.contains(.tuesday))
}

// Test symmetricDifference()

OptionSetTests.test("SymmetricDifference.SingleOption") {
  do {
    let s1 = Days.monday
    let s2: Days = [.monday, .tuesday, .wednesday]
  
    let difference = s1.symmetricDifference(s2)
    expectFalse(difference.contains(.monday))
    expectTrue(difference.contains(.tuesday))
    expectFalse(difference.contains(.sunday))
  }
  do {
    let s1 = Days.monday
    let s2: Days = [.tuesday, .wednesday, .friday]
  
    let difference = s1.symmetricDifference(s2)
    expectTrue(difference.contains(.monday))
    expectTrue(difference.contains(.tuesday))
    expectFalse(difference.contains(.sunday))
  }
}

OptionSetTests.test("SymmetricDifference.MultipleOptions") {
  do {
    let s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.wednesday, .thursday, .friday]
  
    let difference = s1.symmetricDifference(s2)
    expectFalse(difference.contains(.wednesday))
    expectTrue(difference.contains(.tuesday))
    expectFalse(difference.contains(.sunday))
  }
  do {
    let s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.thursday, .friday, .saturday]
  
    let difference = s1.symmetricDifference(s2)
    expectTrue(difference.contains(.wednesday))
    expectTrue(difference.contains(.thursday))
    expectFalse(difference.contains(.sunday))
  }
}

OptionSetTests.test("SymmetricDifference.EmptySet") {
  let s1: Days = []
  let s2: Days = [.monday, .tuesday, .wednesday]

  let difference = s1.symmetricDifference(s2)
  expectTrue(difference.contains(.wednesday))
  expectFalse(difference.contains(.thursday))
}

// Test union()

OptionSetTests.test("Union.SingleOption") {
  do {
    let s1 = Days.monday
    let s2: Days = [.monday, .tuesday, .wednesday]
  
    let union = s1.union(s2)
    expectTrue(union.contains(.monday))
    expectTrue(union.contains(.tuesday))
    expectFalse(union.contains(.sunday))
  }
  do {
    let s1 = Days.monday
    let s2: Days = [.tuesday, .wednesday, .friday]
  
    let union = s1.union(s2)
    expectTrue(union.contains(.monday))
    expectTrue(union.contains(.tuesday))
    expectFalse(union.contains(.sunday))
  }
}

OptionSetTests.test("Union.MultipleOptions") {
  do {
    let s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.wednesday, .thursday, .friday]
  
    let union = s1.union(s2)
    expectTrue(union.contains(.wednesday))
    expectTrue(union.contains(.tuesday))
    expectFalse(union.contains(.sunday))
  }
  do {
    let s1: Days = [.monday, .tuesday, .wednesday]
    let s2: Days = [.thursday, .friday, .saturday]
  
    let union = s1.union(s2)
    expectTrue(union.contains(.wednesday))
    expectTrue(union.contains(.thursday))
    expectFalse(union.contains(.sunday))
  }
}

OptionSetTests.test("Union.EmptySet") {
  let s1: Days = []
  let s2: Days = [.monday, .tuesday, .wednesday]

  let union = s1.union(s2)
  expectTrue(union.contains(.wednesday))
  expectFalse(union.contains(.thursday))
}

// Test update(with:)

OptionSetTests.test("UpdateWith.SingleOption") {
  var s = Days.monday
  
  expectFalse(s.contains(.tuesday))
  
  let member1 = s.update(with: .tuesday)

  expectTrue(s.contains(.monday))
  expectTrue(s.contains(.tuesday))
  expectFalse(s.contains(.sunday))
  expectNil(member1)
  
  let member2 = s.update(with: .tuesday)
  expectEqual(.tuesday, member2)
}

OptionSetTests.test("UpdateWith.MultipleOptions") {
  var s: Days = [.monday, .tuesday, .wednesday]
  
  expectFalse(s.contains(.friday))
  
  let member1 = s.update(with: .friday)
  
  expectTrue(s.contains(.monday))
  expectTrue(s.contains(.friday))
  expectFalse(s.contains(.sunday))
  expectNil(member1)
  
  let member2 = s.update(with: .friday)
  expectEqual(.friday, member2)
}

OptionSetTests.test("UpdateWith.EmptySet") {
  var s: Days = []

  expectFalse(s.contains(.monday))
  
  let member1 = s.update(with: .monday)
  
  expectTrue(s.contains(.monday))
  expectNil(member1)
  
  let member2 = s.update(with: .monday)
  expectEqual(.monday, member2)
}

runAllTests()
