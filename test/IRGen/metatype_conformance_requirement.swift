// RUN: %target-swift-frontend -module-name Test -parse-as-library -emit-ir %s -o - | %FileCheck %s

protocol P {
  func method()
}

protocol Q: P {
}

func f<T>(_: T.Type) where T.Type: Q {
  T.method()
}

// CHECK-LABEL: define {{.*}}f

// Obtain the inherited T.Type: P witness table from the directly available
// T.Type: Q witness table.

// CHECK: [[P_SLOT:%.*]] = getelementptr inbounds ptr, ptr %T.Type.Q, i32 1
// CHECK: [[P_WTABLE:%.*]] = load ptr, ptr [[P_SLOT]]

// Load P.method from the inherited witness table.
// CHECK: [[METHOD_SLOT:%.*]] = getelementptr inbounds ptr, ptr [[P_WTABLE]], i32 1
// CHECK: [[METHOD:%.*]] = load ptr, ptr [[METHOD_SLOT]]

// Dispatch using the T.Type: P witness table.
// CHECK: call swiftcc void [[METHOD]]({{.*}}ptr [[P_WTABLE]])
