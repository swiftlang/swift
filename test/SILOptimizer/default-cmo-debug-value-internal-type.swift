// RUN: %empty-directory(%t)

// RUN: %target-build-swift %s -module-name=Lib -parse-as-library \
// RUN:   -emit-module -emit-module-path %t/Lib.swiftmodule \
// RUN:   -Xfrontend -enable-default-cmo -O -wmo -c -o %t/lib.o

// RUN: %target-build-swift %s -module-name=Lib -parse-as-library \
// RUN:   -Xfrontend -enable-default-cmo -O -wmo -emit-sil -o %t/Lib.sil
// RUN: %FileCheck --check-prefix=LOCAL %s < %t/Lib.sil

// RUN: %target-sil-opt -sil-print-types %t/Lib.swiftmodule -o - | %FileCheck %s

// Test that debug info doesn't keep cross module optimization
// from happening.

struct Internal {
  var a: Int
  var b: Int
}

public func publicEntry(_ x: Int) -> Int {
  // The variable `s`, using an internal type, is optimized out.
  // The remaining debug_value shouldn't keep CMO from happening.
  var s = Internal(a: x, b: 27)
  s.a += 1
  return s.b
}

// The local version has a debug_value.
// LOCAL-LABEL: sil @$s3Lib11publicEntryyS2iF :
// LOCAL:         debug_value {{.*}}type $Internal
// LOCAL:       } // end sil function '$s3Lib11publicEntryyS2iF'

// The function exists in the swiftmodule, and has no debug_value.
// CHECK-LABEL: sil [serialized] [canonical] [ossa] @$s3Lib11publicEntryyS2iF :
// CHECK-NOT:     debug_value
// CHECK:       } // end sil function '$s3Lib11publicEntryyS2iF'
