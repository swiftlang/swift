class Person {
  var firstName: String!
  var lastName: String!
  var age: Int!
  var planet = "Earth", solarSystem = "Milky Way"
  var avgHeight = 175
  let line = #line, file = #file, handle = #dsohandle
  lazy var idea: Idea = { fatalError() }()
}

struct Place {
  let person: Person
  let street: String
  let apartment: Optional<String>
  let city: String
  let state: String
  let postalCode: Int
  let plusFour: Int?
}

protocol Thing {
  var idea: Idea { get }
}

enum Idea {
  var subject: String { fatalError() }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -memberwise-init -source-filename %s -pos=1:8 > %t.result/generate_memberwise.swift
// RUN: diff -u %S/Outputs/generate_memberwise/class_members.swift.expected %t.result/generate_memberwise.swift

// RUN: %refactor -memberwise-init -source-filename %s -pos=11:8 > %t.result/struct_members.swift
// RUN: diff -u %S/Outputs/generate_memberwise/struct_members.swift.expected %t.result/struct_members.swift

// RUN: not %refactor -memberwise-init -source-filename %s -pos=21:10 > %t.result/protocol_members.swift
// RUN: not %refactor -memberwise-init -source-filename %s -pos=25:6 > %t.result/enum_members.swift

