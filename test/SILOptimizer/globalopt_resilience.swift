// RUN: %target-swift-frontend  -O -module-name=test -enable-resilience -emit-sil -primary-file %s | %FileCheck %s

// Check if GlobalOpt generates the optimal getter for a static property with a resilient type.
// The (resilient) getter should just return the literal (and not lazily initialize a global variable).

// CHECK-LABEL: sil @$s4test15ResilientStructV9staticValACvgZ :
// CHECK:     bb0(%0 : $*ResilientStruct{{.*}}):
// CHECK:       [[L:%[0-9]+]] = integer_literal {{.*}}, 27
// CHECK:       [[I:%[0-9]+]] = struct $Int ([[L]] : {{.*}})
// CHECK:       [[S:%[0-9]+]] = struct $ResilientStruct ([[I]] : $Int)
// CHECK:       store [[S]] to %0
// CHECK:       return

public struct ResilientStruct {
  var rawValue: Int

  public static let staticVal = ResilientStruct(rawValue: 27)
}

