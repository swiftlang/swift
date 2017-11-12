// RUN: %target-swift-frontend %s -emit-silgen | %FileCheck %s

// CHECK: final class Rect
// CHECK: @sil_stored final var orgx: Double
final class Rect {
  var orgx = 0.0
}

protocol P { }
// CHECK: struct Rect2 : P {
// CHECK: @sil_stored var orgx: Double
struct Rect2 : P {
  var orgx = 0.0
}
