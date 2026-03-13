// RUN: %target-swift-frontend -emit-irgen %s | %FileCheck %s

// REQUIRES: PTRSIZE=64

public struct SpareBits {
    var o: UInt64 = 0
    var x: UInt8 = 0
    var y: UInt64 = 0
    var x_2: UInt8 = 0
    var y_2: UInt64 = 0
    var x_3: UInt8 = 0
    var y_3: UInt64 = 0
    var x_4: UInt8 = 0
    var y_4: UInt64 = 0
    var x_5: UInt8 = 0
    var y_5: UInt64 = 0
    var x_6: UInt8 = 0
    var y_6: UInt64 = 0
}

public class MyClass {}

public enum Multipayload {
    case a
    case b(MyClass)
    case c(SpareBits)
    case e
    case f
    case g
}

@inline(never)
func dump<T>(_ t : T) {}
func dumpInt(_ i: Int) {}

// CHECK-LABEL: define{{.*}} @"$s10enum_large6testIt1eyAA12MultipayloadO_tF"{{.*}}{
// CHECK:         switch i8 {{%[^,]+}}, label {{%[^,]+}} [
// CHECK:           i8 0, label {{%[^,]+}}
// CHECK:           i8 1, label {{%[^,]+}}
// CHECK:           i8 2, label %[[REGISTER_41:[^,]+]]
// CHECK:       [[REGISTER_41]]:
// CHECK:         br i1 {{%[^,]+}}, label {{%[^,]+}}, label %[[REGISTER_67:[^,]+]]
// CHECK:       [[REGISTER_67]]:
// CHECK:         br i1 {{%[^,]+}}, label {{%[^,]+}}, label %[[REGISTER_93:[^,]+]]
// CHECK:       [[REGISTER_93]]:
// CHECK:         br i1 {{%[^,]+}}, label {{%[^,]+}}, label %[[REGISTER_119:[^,]+]]
// CHECK:       [[REGISTER_119]]:
// CHECK:         br i1 {{%[^,]+}}, label %[[REGISTER_149:[^,]+]], label {{%[^,]+}}
// CHECK:       [[REGISTER_149]]
// CHECK:         call swiftcc void @"$s10enum_large7dumpIntyySiF"(i64 8675309)
public func testIt(e : Multipayload) {
    switch e {
        case .a, .e, .f, .g:
          dumpInt(8675309)
        case .b(let c):
          dump(c)
        case .c(let s):
          dump(s)
    }
}

