// RUN: %swift-frontend -O -emit-ir -module-name foo %s -o - | %FileCheck %s

// CHECK-LABEL: define internal void @"$s3foo3FooVwxx"(ptr noalias %object, ptr readonly captures(none) %"Foo<T>")
// CHECK-NEXT: entry:
// CHECK-NEXT:   tail call swiftcc void @"$s3foo3FooVfD"(ptr %"Foo<T>", ptr noalias swiftself %object)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

public struct Foo<T> : ~Copyable {
    var t: T

    public init(t: T) {
        self.t = t
    }
    deinit {
        print("Deinit")
    }
}
