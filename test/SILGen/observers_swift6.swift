// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -swift-version 6 -Xllvm -sil-full-demangle -parse-as-library -disable-objc-attr-requires-foundation-module -enable-objc-interop %s | %FileCheck %s

class Kitty {
  var age: Int = 5 {
    didSet {
    }
  }

  init(val: Int) {
    defer { age = val }
  }

  // Access in a defer matches the context in Swift 6, so this defer should be
  // direct-to-storage.
  // CHECK-LABEL: sil private [ossa] @$s16observers_swift65KittyC3valACSi_tcfc6$deferL_yyF
  // CHECK:         [[REF:%.+]] = ref_element_addr {{.*}} : $Kitty, #Kitty.age
  // CHECK:         [[ACCESS:%.+]] = begin_access {{.*}} [[REF]] : $*Int
  // CHECK:         assign {{.*}} to [[ACCESS]] : $*Int
}
