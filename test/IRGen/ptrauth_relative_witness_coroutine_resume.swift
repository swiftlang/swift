// RUN: %target-swift-frontend -enable-relative-protocol-witness-tables -enable-experimental-feature CoroutineAccessors -module-name A -primary-file %s -emit-ir -O | %FileCheck %s

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: CPU=arm64e

// Make sure relative witnesses use the right discriminators for coroutine
// accessors.
protocol P {
    associatedtype Index
    associatedtype Element
    subscript(position: Index) -> Element { read }
}

struct S: P {
    typealias Index = Int
    typealias Element = Int
    var storage: [Int]

    subscript(position: Int) -> Int {
        read {
            yield storage[position]
        }
    }
}

// The witness thunk signs the resume pointer with a yield-type discriminator.
// CHECK-LABEL: define {{.*}} @"$s1A1SVAA1PA2aDPy7ElementQz5IndexQzciyTW"(
// CHECK:         [[BLEND_THUNK:%[0-9]+]] = tail call i64 @llvm.ptrauth.blend(i64 %{{[0-9]+}}, i64 [[DISC:[0-9]+]])
// CHECK-NEXT:    {{%[0-9]+}} = tail call i64 @llvm.ptrauth.sign(i64 ptrtoint (ptr @"$s1A1SVAA1PA2aDPy7ElementQz5IndexQzciyTW.resume.0" to i64), i32 0, i64 [[BLEND_THUNK]])

// The caller authenticates the resume pointer with the same discriminator.
// CHECK-LABEL: define {{.*}} @"$s1A7iterateyyxAA1PRzSi5IndexRtzlF"(
// CHECK:         {{%[0-9]+}} = call i64 @llvm.ptrauth.blend(i64 %{{[0-9]+}}, i64 [[DISC]])
// CHECK-NEXT:    call swiftcc void %{{[0-9]+}}(ptr noalias %{{[a-z0-9-]+}}, ptr swiftcoro {{.*}}) {{.*}}[ "ptrauth"(i32 0, i64 %{{[0-9]+}}) ]

@_optimize(none)
func iterate<C: P>(_ c: C) where C.Index == Int {
    var i = 0
    let _ = c[i]
}
