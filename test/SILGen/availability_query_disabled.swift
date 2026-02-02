// RUN: %target-swift-emit-silgen -disable-availability-checking %s -verify
// RUN: %target-swift-emit-silgen -disable-availability-checking %s | %FileCheck %s

// CHECK-LABEL: // available()
func available() {
  // CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
  // CHECK: cond_br [[TRUE]]
  if #available(macOS 10.15, *) {}
}

// CHECK-LABEL: // unavailable()
func unavailable() {
  // CHECK: [[FALSE:%.*]] = integer_literal $Builtin.Int1, 0
  // CHECK: cond_br [[FALSE]]
  if #unavailable(macOS 10.15) {}
}
