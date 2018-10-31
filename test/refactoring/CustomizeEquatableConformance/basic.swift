class Animal {
  var age: Int
  var kind: AnimalKind
  var gender: Gender
  init(age: Int, kind: AnimalKind, gender: Gender) {
    self.age = age
    self.kind = kind
    self.gender = gender
  }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor customize-equatable-conformance -source-filename %s -pos=1:8 > %t.result/basic.swift
// RUN: diff -u %S/Outputs/basic/basic.swift.expected %t.result/basic.swift
