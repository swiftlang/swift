// RUN: %target-run-simple-swift | %FileCheck %s

// REQUIRES: executable_test

struct Color {
  var red = 1.0
  var green = 2.0
}

// Implicit member expressions through a static dynamic member subscript.
@dynamicMemberLookup
struct Prop: CustomStringConvertible {
  var name: String

  static subscript(dynamicMember member: KeyPath<Color, Double>) -> Self {
    Prop(name: "dynamic \(Color()[keyPath: member])")
  }

  static var green: Prop { Prop(name: "static green") }

  var description: String { name }
}

let dynamicProp: Prop = .red
// CHECK: dynamic 1.0
print(dynamicProp)

// A real static member is preferred over a dynamic member.
let staticProp: Prop = .green
// CHECK: static green
print(staticProp)

// As a function argument.
func describe(_ prop: Prop) -> String { prop.name }

// CHECK: dynamic 1.0
print(describe(.red))
