// RUN: %target-resilience-test
// REQUIRES: executable_test
// REQUIRES: no_asan

import StdlibUnittest
import class_change_size


var ClassChangeSizeTest = TestSuite("ClassChangeSize")

func increment(_ c: inout ChangeSize) {
  c.version += 1
}

// Change field offsets and size of a class from a different
// resilience domain

ClassChangeSizeTest.test("ChangeFieldOffsetsOfFixedLayout") {
  let t = ChangeFieldOffsetsOfFixedLayout(major: 7, minor: 5, patch: 3)

  do {
    expectEqual(t.getVersion(), "7.5.3")
  }

  do {
    t.minor.version = 1
    t.patch.version = 2
    expectEqual(t.getVersion(), "7.1.2")
  }

  do {
    increment(&t.patch)
    expectEqual(t.getVersion(), "7.1.3")
  }
}

// Superclass and subclass are in a different resilience domain

ClassChangeSizeTest.test("ChangeSizeOfSuperclass") {
  let t = ChangeSizeOfSuperclass()

  do {
    expectEqual(t.getVersion(), "7.0.0 (Big Bang)")
  }

  do {
    t.major.version = 6
    t.minor.version = 0
    t.patch.version = 5
    t.codename = "Big Deal"
    expectEqual(t.getVersion(), "6.0.5 (Big Deal)")
  }

  do {
    increment(&t.patch)
    t.codename = "Six Pack"
    expectEqual(t.getVersion(), "6.0.6 (Six Pack)")
  }
}

// Change field offsets and size of a class from the current
// resilience domain

class ChangeFieldOffsetsOfMyFixedLayout {
  init(major: Int32, minor: Int32, patch: Int32) {
    self.major = ChangeSize(version: major)
    self.minor = ChangeSize(version: minor)
    self.patch = ChangeSize(version: patch)
  }

  var major: ChangeSize
  var minor: ChangeSize
  var patch: ChangeSize

  func getVersion() -> String {
    return "\(major.version).\(minor.version).\(patch.version)"
  }
}

ClassChangeSizeTest.test("ChangeFieldOffsetsOfMyFixedLayout") {
  let t = ChangeFieldOffsetsOfMyFixedLayout(major: 9, minor: 2, patch: 1)

  do {
    expectEqual(t.getVersion(), "9.2.1")
  }

  do {
    t.major.version = 7
    t.minor.version = 6
    t.patch.version = 1
    expectEqual(t.getVersion(), "7.6.1")
  }

  do {
    increment(&t.patch)
    expectEqual(t.getVersion(), "7.6.2")
  }
}

// Subclass is in our resilience domain, superclass is in a
// different resilience domain

class MyChangeSizeOfSuperclass : ChangeFieldOffsetsOfFixedLayout {
  init() {
    self.codename = "Road Warrior"

    super.init(major: 7, minor: 0, patch: 1)
  }

  var codename: String

  override func getVersion() -> String {
    return "\(super.getVersion()) (\(codename))";
  }
}

ClassChangeSizeTest.test("MyChangeSizeOfSuperclass") {
  let t = MyChangeSizeOfSuperclass()

  do {
    expectEqual(t.getVersion(), "7.0.1 (Road Warrior)")
  }

  do {
    t.minor.version = 5
    t.patch.version = 2
    t.codename = "Marconi"
    expectEqual(t.getVersion(), "7.5.2 (Marconi)")
  }

  do {
    increment(&t.patch)
    t.codename = "Unity"
    expectEqual(t.getVersion(), "7.5.3 (Unity)")
  }
}

// Subclass and superclass are both in our resilience domain

class ChangeSizeOfMySuperclass : ChangeFieldOffsetsOfFixedLayout {
  init() {
    self.codename = "Big Bang"

    super.init(major: 7, minor: 0, patch: 0)
  }

  var codename: String

  override func getVersion() -> String {
    return "\(super.getVersion()) (\(codename))";
  }
}

ClassChangeSizeTest.test("ChangeSizeOfMySuperclass") {
  let t = ChangeSizeOfMySuperclass()

  do {
    expectEqual(t.getVersion(), "7.0.0 (Big Bang)")
  }

  do {
    t.major.version = 6
    t.minor.version = 0
    t.patch.version = 5
    t.codename = "Big Deal"
    expectEqual(t.getVersion(), "6.0.5 (Big Deal)")
  }

  do {
    increment(&t.patch)
    t.codename = "Six Pack"
    expectEqual(t.getVersion(), "6.0.6 (Six Pack)")
  }
}

runAllTests()
