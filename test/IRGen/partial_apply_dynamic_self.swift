// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// rdar://174464752: The forwarder for 'g<Self>' takes no 'self' parameter, so
// it must load the dynamic Self metadata that 'm' stored in the closure
// context.

class C {
  func m() -> Self { f(g) }
}

func g<T>() -> T { fatalError() }
func f<T>(_: () -> T) -> T { fatalError() }

// CHECK-LABEL: define internal swiftcc void @"$s26partial_apply_dynamic_self1gxylFTA"(ptr {{.*}}%0, ptr swiftself %1)
// The forwarder loads a pointer from its context.
// CHECK:         [[SELF:%.*]] = load ptr, ptr
// CHECK:         call swiftcc void @"$s26partial_apply_dynamic_self1gxylF"(ptr {{.*}}%0, ptr [[SELF]])
