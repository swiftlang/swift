// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/MixedExistential.swift -parse-as-library -emit-module -emit-library -module-name MixedExistential -o %t/libTypesToReflect
// RUN: %target-swift-reflection-dump %t/libTypesToReflect %platform-module-dir/%target-library-name(swiftCore) -dump-type-lowering < %s | %FileCheck %s --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK

// REQUIRES: objc_interop

// A class-constrained protocol composition that mixes an @objc protocol
// (contributing no witness table) with a Swift protocol (contributing one)
// must lower as a class_existential record — one object pointer plus one
// witness-table pointer — rather than being rejected as
// `@objc existential with witness tables`.

// An @objc protocol composed with a Swift protocol that is itself
// class-constrained (`: AnyObject`).
16MixedExistential30MixedWithClassConstrainedProtoVD
// CHECK-NOT: Invalid lowering
// CHECK-64: (struct MixedExistential.MixedWithClassConstrainedProto)
// CHECK-64-NEXT: (struct size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI:2048|4096|2147483647]] bitwise_takable=1
// CHECK-64-NEXT:   (field name=existential offset=0
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1)))))
// CHECK-32: (struct MixedExistential.MixedWithClassConstrainedProto)
// CHECK-32-NEXT: (struct size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:   (field name=existential offset=0
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1)))))

// An @objc protocol composed with a Swift protocol that is NOT independently
// class-constrained.
16MixedExistential27MixedWithUnconstrainedProtoVD
// CHECK-NOT: Invalid lowering
// CHECK-64: (struct MixedExistential.MixedWithUnconstrainedProto)
// CHECK-64-NEXT: (struct size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:   (field name=existential offset=0
// CHECK-64-NEXT:     (class_existential size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=object offset=0
// CHECK-64-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1)))))
// CHECK-32: (struct MixedExistential.MixedWithUnconstrainedProto)
// CHECK-32-NEXT: (struct size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:   (field name=existential offset=0
// CHECK-32-NEXT:     (class_existential size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=object offset=0
// CHECK-32-NEXT:         (reference kind=strong refcounting=unknown))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1)))))

// The existential metatype of the mixed composition lowers as an
// existential_metatype record — a metadata pointer plus one witness-table
// pointer.
16MixedExistential24MixedExistentialMetatypeVD
// CHECK-NOT: Invalid lowering
// CHECK-64: (struct MixedExistential.MixedExistentialMetatype)
// CHECK-64-NEXT: (struct size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:   (field name=metatype offset=0
// CHECK-64-NEXT:     (existential_metatype size=16 alignment=8 stride=16 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1
// CHECK-64-NEXT:       (field name=metadata offset=0
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=[[PTR_XI]] bitwise_takable=1))
// CHECK-64-NEXT:       (field name=wtable offset=8
// CHECK-64-NEXT:         (builtin size=8 alignment=8 stride=8 num_extra_inhabitants=1 bitwise_takable=1)))))
// CHECK-32: (struct MixedExistential.MixedExistentialMetatype)
// CHECK-32-NEXT: (struct size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:   (field name=metatype offset=0
// CHECK-32-NEXT:     (existential_metatype size=8 alignment=4 stride=8 num_extra_inhabitants=4096 bitwise_takable=1
// CHECK-32-NEXT:       (field name=metadata offset=0
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=4096 bitwise_takable=1))
// CHECK-32-NEXT:       (field name=wtable offset=4
// CHECK-32-NEXT:         (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=1 bitwise_takable=1)))))
// CHECK-NOT: Invalid lowering
