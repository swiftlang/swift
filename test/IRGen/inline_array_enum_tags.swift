// RUN: %target-swift-frontend -disable-availability-checking -O -emit-ir %s | %FileCheck %s

// UNSUPPORTED: PTRSIZE=32

public struct Foo {
    let x: UInt64
    let y: InlineArray<2, UInt64>
}

public struct Bar {
    let x: UInt64
    let y: [UInt64]
}

// CHECK: define {{.*}} i32 @"$s22inline_array_enum_tags3BazOwug"(ptr noalias nocapture readonly %value, ptr nocapture readnone %Baz)
// CHECK:   [[TAG_ADDR:%.*]] = getelementptr inbounds i8, ptr %value, i64 24
// CHECK:   [[TAG_VAL:%.*]] = load i1, ptr [[TAG_ADDR]], align 8
// CHECK:   [[TAG_EXT:%.*]] = zext i1 [[TAG_VAL]] to i32
// CHECK:   ret i32 [[TAG_EXT]]
// CHECK: }
public enum Baz {
    case foo(Foo)
    case bar(Bar)
}

public struct Padded {
    let x: UInt64
    let y: InlineArray<2, (UInt16, UInt8)>
}


// CHECK: define {{.*}} i32 @"$s22inline_array_enum_tags17WithPaddedPayloadOwug"(ptr noalias nocapture readonly %value, ptr nocapture readnone %WithPaddedPayload)
// CHECK: entry:
// CHECK:   [[ADDR:%.*]] = getelementptr inbounds i8, ptr %value, i64 8
// CHECK:   [[VAL:%.*]] = load i64, ptr [[ADDR]], align 8
// CHECK:   [[MASKED:%.*]] = and i64 [[VAL]], 2147483648
// CHECK:   [[TAG:%.*]] = icmp ne i64 [[MASKED]], 0
// CHECK:   [[EXTENDED:%.*]] = zext i1 [[TAG]] to i32
// CHECK:   ret i32 [[EXTENDED]]
// CHECK: }
public enum WithPaddedPayload {
    case a(Padded)
    case b(Padded)
}
