// RUN: %target-swift-frontend -module-name Test -parse-as-library -emit-ir %s -o - | %FileCheck %s
// RUN: %target-swift-frontend -module-name Test -parse-as-library -O -emit-ir %s -o /dev/null

protocol P {
  func method()
}

protocol Q: P {
}

func f<T>(_: T.Type) where T.Type: Q {
  T.method()
}

func pack<each T>(_ types: repeat (each T).Type)
    where repeat (each T).Type: P {
  for type in repeat each types {
    type.method()
  }
}

func forward<each T>(_ types: repeat (each T).Type)
    where repeat (each T).Type: P {
  pack(repeat each types)
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

// CHECK-LABEL: define {{.*}} @"$s4Test4packyyxmxQpRvzAA1PxmRQlF"(
// CHECK-SAME: [[INT:i32|i64]] [[SHAPE:%[^,]+]],
// CHECK-SAME: ptr [[METADATA_PACK:%[^,]+]],
// CHECK-SAME: ptr [[WTABLE_PACK:%[^)]+]])

// Extract the metatype conformance for the opened element.
// CHECK: [[PACK_BITS:%.*]] = ptrtoint ptr [[WTABLE_PACK]] to [[INT]]
// CHECK: [[PACK_MASK:%.*]] = and [[INT]] [[PACK_BITS]], -2
// CHECK: [[WTABLES:%.*]] = inttoptr [[INT]] [[PACK_MASK]] to ptr
// CHECK: [[WTABLE_SLOT:%.*]] = getelementptr inbounds ptr, ptr [[WTABLES]], [[INT]]
// CHECK: [[ELEMENT_WTABLE:%.*]] = load ptr, ptr [[WTABLE_SLOT]]

// Dispatch through the opened element's witness table.
// CHECK: [[ELEMENT_METHOD_SLOT:%.*]] = getelementptr inbounds ptr, ptr [[ELEMENT_WTABLE]], i32 1
// CHECK: [[ELEMENT_METHOD:%.*]] = load ptr, ptr [[ELEMENT_METHOD_SLOT]]
// CHECK: call swiftcc void [[ELEMENT_METHOD]]({{.*}}ptr [[ELEMENT_WTABLE]])

// CHECK-LABEL: define {{.*}} @"$s4Test7forwardyyxmxQpRvzAA1PxmRQlF"(
// CHECK-SAME: [[INT]] [[FORWARD_SHAPE:%[^,]+]],
// CHECK-SAME: ptr [[FORWARD_METADATA_PACK:%[^,]+]],
// CHECK-SAME: ptr [[FORWARD_WTABLE_PACK:%[^)]+]])

// Forward the witness-table pack supplied by the caller.
// CHECK: call swiftcc void @"$s4Test4packyyxmxQpRvzAA1PxmRQlF"({{.*}}ptr [[FORWARD_WTABLE_PACK]])
