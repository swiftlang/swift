// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

class Person : NSObject {
  @objc(firstNameString) var firstName: String
  @objc var lastName: String

  init(firstName: String, lastName: String) {
    self.firstName = firstName
    self.lastName = lastName
  }

  override var description: String {
    return "\(lastName), \(firstName)"
  }
}

class Band : NSObject {
  @objc var members: [Person] = []
}

class RecordLabel : NSObject {
  @objc var bands: [String : Band] = [:]
}

let band = Band()
band.members = [Person(firstName: "John", lastName: "Lennon"),
                Person(firstName: "Paul", lastName: "McCartney"),
                Person(firstName: "George", lastName: "Harrison"),
                Person(firstName: "Ringo", lastName: "Star")]

// CHECK: ===Members===
// CHECK-NEXT: (
// CHECK-NEXT:    Lennon, John
// CHECK-NEXT:    McCartney, Paul
// CHECK-NEXT:    Harrison, George
// CHECK-NEXT:    Star, Ringo
// CHECK-NEXT: )
print("===Members===")
print(band.value(forKeyPath: #keyPath(Band.members))!)

// CHECK: ===First Names===
// CHECK-NEXT: (
// CHECK-NEXT:    John,
// CHECK-NEXT:    Paul,
// CHECK-NEXT:    George,
// CHECK-NEXT:    Ringo
// CHECK-NEXT:)
print("===First Names===")
print(band.value(forKeyPath: #keyPath(Band.members.firstName))!)

let recordLabel = RecordLabel()
recordLabel.bands["Beatles"] = band

// CHECK: ===Last Names===
// CHECK-NEXT: (
// CHECK-NEXT:     Lennon,
// CHECK-NEXT:     McCartney,
// CHECK-NEXT:     Harrison,
// CHECK-NEXT:     Star
// CHECK-NEXT: )
print("===Last Names===")
print(recordLabel.value(forKeyPath: #keyPath(RecordLabel.bands.Beatles.members.lastName))!)

// CHECK: DONE
print("DONE")
