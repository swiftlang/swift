// RUN: %target-swift-frontend -disable-availability-checking -O -emit-ir %s | %FileCheck %s

public struct Foo {
    let x: UInt64
    let y: InlineArray<2, UInt64>
}

public struct Bar {
    let x: UInt64
    let y: [UInt64]
}

// CHECK: define {{.*}} i32 @"$s22inline_array_enum_tags3BazOwug"(ptr noalias{{( nocapture)?}} readonly{{( captures\(none\))?}} %value, ptr{{( nocapture)?}} readnone{{( captures\(none\))?}} %Baz)
// CHECK:   [[TAG_ADDR:%.*]] = getelementptr inbounds{{.*}} i8, ptr %value, {{i64|i32}} 24
// CHECK:   [[TAG_VAL:%.*]] = load i8, ptr [[TAG_ADDR]], align 8
// CHECK:   [[TAG_EXT:%.*]] = zext i8 [[TAG_VAL]] to i32
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


// CHECK: define {{.*}} i32 @"$s22inline_array_enum_tags17WithPaddedPayloadOwug"(ptr noalias{{( nocapture)?}} readonly{{( captures\(none\))?}} %value, ptr{{( nocapture)?}} readnone{{( captures\(none\))?}} %WithPaddedPayload)
// CHECK: entry:
// CHECK:   [[ADDR:%.*]] = getelementptr inbounds{{.*}} i8, ptr %value, {{i64|i32}} 8
// CHECK:   [[VAL:%.*]] = load {{i64|i32}}, ptr [[ADDR]], align 8
// CHECK:   [[TAG:%.*]] = lshr i32 {{%.*}}, 31
// CHECK:   ret i32 [[TAG]]
// CHECK: }
public enum WithPaddedPayload {
    case a(Padded)
    case b(Padded)
}
