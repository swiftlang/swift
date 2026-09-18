// RUN: %target-swift-frontend -disable-availability-checking -O -emit-ir %s | %FileCheck %s

// CHECK-LABEL: define {{.*}}swiftcc {{i64|i32}} @"$s{{.*}}13subscriptReadyS{{.*}}InlineArray{{.*}}"(ptr noalias readonly align {{8|4}} captures(none) dereferenceable({{[0-9]+}}) %0, {{i64|i32}} %1)
// CHECK-NOT: alloca
// CHECK-NOT: @llvm.memcpy
// CHECK: ret
public func subscriptRead(_ a: [512 of Int], _ i: Int) -> Int {
  a[i]
}

// CHECK-LABEL: define {{.*}}swiftcc {{i64|i32}} @"$s{{.*}}7spanSumyS{{.*}}InlineArray{{.*}}"(ptr noalias readonly align {{8|4}} captures(none) dereferenceable({{[0-9]+}}) %0)
// CHECK-NOT: alloca
// CHECK-NOT: @llvm.memcpy
// CHECK: ret
public func spanSum(_ value: borrowing [512 of Int]) -> Int {
  var sum = 0
  let span = value.span
  for i in span.indices {
    sum &+= span[i]
  }
  return sum
}

public struct Pixel {
    let r: UInt8
    let g: UInt8
    let b: UInt8
    let a: UInt8
    static var transparentZero: Self {
        .init(r: 0, g: 0, b: 0, a: 0)
    }
}

public struct PixelCache: ~Copyable {
    var pixels: InlineArray<64, Pixel> = .init(repeating: .transparentZero)

    subscript(index: UInt8) -> Pixel {
        pixels[Int(index)]
    }

}

// TODO: memcpy should be eliminated here
// CHECK-LABEL: define {{.*}}swiftcc i32 @"$s18inline_array_tests4test5cache5indexAA5PixelVAA0G5CacheV_s5UInt8VtF"(ptr noalias readonly align 1 captures(none) dereferenceable(256) %0, i8 %1) {{.*}} {
// CHECK: memcpy
// CHECK-LABEL: ret
public func test(cache: borrowing PixelCache, index: UInt8) -> Pixel {
    return cache[index]
}

public struct Intra {
    var table: [8 of (consuming MutableSpan<UInt8>, Int, Int) -> Void]

// CHECK-LABEL: define {{.*}}swiftcc void @"$s18inline_array_tests5IntraV07predictD03dst6offset0F6Stride4modeys11MutableSpanVys5UInt8VGn_S3itF"(ptr %0, {{i64|i32}} %1, {{i64|i32}} %2, {{i64|i32}} %3, {{i64|i32}} %4, ptr noalias readonly swiftself align {{8|4}} captures(none) dereferenceable({{[0-9]+}}) %5) {{.*}} {
// CHECK-NOT: memcpy
// CHECK-LABEL: ret

    public func predictIntra(dst: consuming MutableSpan<UInt8>, offset: Int, dstStride: Int, mode: Int) {
        table[mode&7](dst, offset, dstStride)
    }
}
