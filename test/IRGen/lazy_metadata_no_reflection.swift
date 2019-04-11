// RUN: %target-swift-frontend -parse-as-library -module-name=test -O %s -emit-ir -disable-reflection-metadata | %FileCheck %s

struct S { }

extension S: Equatable {
  static func ==(lhs: S, rhs: S) -> Bool {
    return false
  }
}

// CHECK-LABEL: @"$s4test1SVMn" = hidden constant
// CHECK-LABEL: @"$s4test1SVSQAAMc" = hidden constant
