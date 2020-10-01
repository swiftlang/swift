// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

extension Optional: Comparable where Wrapped: Comparable {
  public static func v(in r: ClosedRange<Self>) {}
  public static func < (lhs: Self, rhs: Self) -> Bool { false }
}

// CHECK: sSNyxSgAASL23retroactive_conformancexSLHD1__HCg_GD
