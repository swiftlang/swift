// RUN: %target-swift-frontend -O -emit-ir %s | %FileCheck %s

// REQUIRES: PTRSIZE=64

// CHECK: define {{.*}}swiftcc void @"$s13rdar1499856334DateV1a1b1c1d1e1fACSiSg_A5JtcfC"(ptr noalias{{( nocapture)?}} writeonly sret(%T13rdar1499856334DateV){{( captures\(none\))?}}{{.*}} %0, i64 %1, i8 %2, i64 %3, i8 %4, i64 %5, i8 %6, i64 %7, i8 %8, i64 %9, i8 %10, i64 %11, i8 %12)
// CHECK: entry:
// CHECK:   store i64 %1
// CHECK:   getelementptr inbounds{{.*}} i8, ptr %0, i64 8
// CHECK:   store i8 %2
// CHECK:   getelementptr inbounds{{.*}} i8, ptr %0, i64 16
// CHECK:   store i64 %3
// CHECK:   getelementptr inbounds{{.*}} i8, ptr %0, i64 24
// CHECK:   store i8 %4
// CHECK:   getelementptr inbounds{{.*}} i8, ptr %0, i64 32
// CHECK:   store i64 %5
// CHECK:   getelementptr inbounds{{.*}} i8, ptr %0, i64 40
// CHECK:   store i8 %6
// CHECK:   getelementptr inbounds{{.*}} i8, ptr %0, i64 48
// CHECK:   store i64 %7
// CHECK:   getelementptr inbounds{{.*}} i8, ptr %0, i64 56
// CHECK:   store i8 %8
// CHECK:   getelementptr inbounds{{.*}} i8, ptr %0, i64 64
// CHECK:   store i64 %9
// CHECK:   getelementptr inbounds{{.*}} i8, ptr %0, i64 72
// CHECK:   store i8 %10
// CHECK:   getelementptr inbounds{{.*}} i8, ptr %0, i64 80
// CHECK:   store i64 %11
// CHECK:   getelementptr inbounds{{.*}} i8, ptr %0, i64 88
// CHECK:   store i8 %12
// CHECK:   ret void
// CHECK: }

public struct Date {
    internal var a: Int?
    internal var b: Int?
    internal var c: Int?
    internal var d: Int?
    internal var e: Int?
    internal var f: Int?

    public init(a: Int?, b: Int?, c: Int?, d: Int?, e: Int?, f: Int?) {
        self.a = a
        self.b = b
        self.c = c
        self.d = d
        self.e = e
        self.f = f
    }
}
