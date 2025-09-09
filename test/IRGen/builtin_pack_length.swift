// RUN: %target-swift-frontend -module-name builtins -enable-builtin-module -Xllvm -sil-disable-pass=target-constant-folding -disable-access-control -primary-file %s -emit-ir -o - -disable-objc-attr-requires-foundation-module -target %target-swift-5.9-abi-triple -O | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime

// REQUIRES: CPU=x86_64 || CPU=arm64 || CPU=arm64e

import Builtin

// CHECK: define {{.*}} @"$s8builtins9packCountyBwxxQpRvzlF"(ptr {{.*}} %0, i64 {{.*}} [[PACK_COUNT:%.*]], ptr {{.*}} %"each T")
// CHECK: ret i64 [[PACK_COUNT]]
func packCount<each T>(_: repeat each T) -> Builtin.Word {
  Builtin.packLength((repeat each T).self)
}

// CHECK: define {{.*}} @"$s8builtins12packCountUseBwyF"()
// CHECK-NEXT: entry:
// CHECK-NEXT: ret i64 4
func packCountUse() -> Builtin.Word {
  packCount(123, 321, 456, 654)
}

// CHECK: define {{.*}} @"$s8builtins18weirdPackCountUse0yBwxxQpRvzlF"(ptr {{.*}} %0, i64 [[PACK_COUNT:%.*]], ptr {{.*}} %"each T")
// CHECK: [[ADD:%.*]] = add i64 [[PACK_COUNT]], 2
// CHECK-NEXT: ret i64 [[ADD]]
func weirdPackCountUse0<each T>(_ x: repeat each T) -> Builtin.Word {
  Builtin.packLength((String, repeat each T, Int).self)
}

// CHECK: define {{.*}} @"$s8builtins18weirdPackCountUse1yBwxxQpRvzlF"(ptr {{.*}} %0, i64 %1, ptr {{.*}} %"each T")
// CHECK: ret i64 3
func weirdPackCountUse1<each T>(_ x: repeat each T) -> Builtin.Word {
  Builtin.packLength((String, (repeat each T), Int).self)
}

struct Pack<each T> {
// CHECK: define {{.*}} @"$s8builtins4PackV5countBwvgZ"(i64 returned [[PACK_COUNT:%.*]], ptr{{( nocapture)?}} readnone{{( captures\(none\))?}} %"each T")
// CHECK-NEXT: entry:
// CHECK-NEXT: ret i64 [[PACK_COUNT]]
  static var count: Builtin.Word {
    Builtin.packLength((repeat each T).self)
  }
}
