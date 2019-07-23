class Person {
  var firstName: String!
  var lastName: String!
  var age: Int!
  var planet = "Earth", solarSystem = "Milky Way"
  var avgHeight = 175
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -memberwise-init -source-filename %s -pos=1:8 > %t.result/class_members.swift
// RUN: diff -u %S/Outputs/class_members/class_members.swift.expected %t.result/class_members.swift
