// RUN: %target-swift-frontend -enable-implicit-dynamic -target x86_64-apple-macosx10.9 -Onone -emit-ir %s | %FileCheck --check-prefix=MAYBE-AVAILABLE %s
// RUN: %target-swift-frontend -enable-implicit-dynamic -target %target-cpu-apple-macosx10.15 -Onone -emit-ir %s | %FileCheck --check-prefix=ALWAYS-AVAILABLE %s
// REQUIRES: OS=macosx

protocol P {}
extension Int: P {}

@available(macOS 10.15, *)
func foo() -> some P {
  return 1738
}

@_silgen_name("external")
func generic<T: P>(x: T, y: T)

@available(macOS 10.15, *)
public func main() {
  generic(x: foo(), y: foo())
}

// MAYBE-AVAILABLE: declare{{.*}} extern_weak {{.*}} @swift_getOpaqueTypeConformance
// ALWAYS-AVAILABLE-NOT: declare{{.*}} extern_weak {{.*}} @swift_getOpaqueTypeConformance
