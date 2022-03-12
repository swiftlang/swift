// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s
// RUN: %target-swift-frontend -emit-silgen -parse-as-library %s | %FileCheck %s

@main
struct EntryPoint {
  static func main() {
  }
}

// CHECK-NOT: @main struct EntryPoint {
// CHECK: struct EntryPoint {
