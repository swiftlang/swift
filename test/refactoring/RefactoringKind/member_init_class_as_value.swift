class Person {
  var firstName: String!
  var lastName: String!
  var age: Int!
  var planet = "Earth", solarSystem = "Milky Way"
  var avgHeight = 175
}

let _ = Person()

// RUN: %refactor -source-filename %s -pos=9:10 | %FileCheck %s -check-prefix=CHECK-NONE
// CHECK-NONE: Action begins
// CHECK-NONE-NEXT: Action ends
