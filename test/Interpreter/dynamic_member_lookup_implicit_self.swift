// RUN: %target-run-simple-swift | %FileCheck %s

// REQUIRES: executable_test

struct Color {
  var red = 1.0
  var green = 2.0
}

// Dynamic members through implicit 'self'.
@dynamicMemberLookup
struct Lens {
  var color: Color

  subscript<T>(dynamicMember keyPath: WritableKeyPath<Color, T>) -> T {
    get { color[keyPath: keyPath] }
    set { color[keyPath: keyPath] = newValue }
  }

  mutating func bump() {
    red += 10
  }

  func sum() -> Double {
    red + green
  }
}

var lens = Lens(color: Color())
lens.bump()
// CHECK: 13.0
print(lens.sum())
// CHECK: 11.0
print(lens.color.red)
