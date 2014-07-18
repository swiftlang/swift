// RUN: %swift %s -emit-silgen | FileCheck %s

// CHECK: final class Rect
// CHECK: @sil_stored final var orgx: Double
final class Rect {
   var orgx = 0.0
}
