// RUN: %target-swift-emit-silgen -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi -disable-availability-checking %s -verify
// RUN: %target-swift-emit-silgen -target-variant %target-cpu-apple-macosx13 -target %target-cpu-apple-ios16-macabi -disable-availability-checking %s -verify

// RUN: %target-swift-emit-silgen -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi -disable-availability-checking %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -target-variant %target-cpu-apple-macosx13 -target %target-cpu-apple-ios16-macabi -disable-availability-checking %s | %FileCheck %s

// REQUIRES: OS=macosx || OS=maccatalyst

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
