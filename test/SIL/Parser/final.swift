// RUN: %target-swift-frontend %s -emit-silgen | %FileCheck %s

// CHECK: final class Rect
// CHECK: @_hasInitialValue @_hasStorage final var orgx: Double
final class Rect {
  var orgx = 0.0
}

protocol P { }
// CHECK: struct Rect2 : P {
// CHECK: @_hasStorage @_hasInitialValue var orgx: Double
struct Rect2 : P {
  var orgx = 0.0
}
