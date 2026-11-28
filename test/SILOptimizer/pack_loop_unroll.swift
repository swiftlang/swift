// RUN: %target-swift-frontend -module-name test -O -emit-sil -disable-availability-checking %s | %FileCheck %s

// Issue #84819. The loop over the key paths is unrolled and the key paths are
// projected directly, so the comparison is straight-line code on the fields of
// A: each field is compared in turn, with an early exit to the result if the
// fields differ.

enum ComparisonResult {
  case orderedAscending
  case orderedDescending
  case orderedSame
}

infix operator <=>

func <=> <C: Comparable>(_ a: C, _ b: C) -> ComparisonResult {
  if a < b {
    return .orderedAscending
  } else if a > b {
    return .orderedDescending
  } else {
    return .orderedSame
  }
}

struct ComponentwiseComparator<Root, each T: Comparable> {
  let keyPaths: (repeat KeyPath<Root, each T>)

  @inline(__always)
  init(_ keyPaths: repeat KeyPath<Root, each T>) {
    self.keyPaths = (repeat each keyPaths)
  }

  @inline(__always)
  func compare(_ left: Root, _ right: Root) -> ComparisonResult {
    for path in repeat each keyPaths {
      switch left[keyPath: path] <=> right[keyPath: path] {
      case .orderedSame:
        break
      case let x:
        return x
      }
    }

    return .orderedSame
  }
}

@frozen
public struct A: Comparable {
  var a: Int16
  var b: Int16
  var c: Int16

  // CHECK-LABEL: sil shared @$s4test1AV1loiySbAC_ACtFZTf4nnd_n : $@convention(thin) (A, A) -> Bool {
  // CHECK:       bb0([[LEFT:%[0-9]+]] : $A, [[RIGHT:%[0-9]+]] : $A):
  // CHECK:         [[L_A:%[0-9]+]] = struct_extract [[LEFT]], #A.a
  // CHECK-NEXT:    [[L_A_V:%[0-9]+]] = struct_extract [[L_A]], #Int16._value
  // CHECK-NEXT:    [[R_A:%[0-9]+]] = struct_extract [[RIGHT]], #A.a
  // CHECK-NEXT:    [[R_A_V:%[0-9]+]] = struct_extract [[R_A]], #Int16._value
  // CHECK-NEXT:    [[LT_A:%[0-9]+]] = builtin "cmp_slt_Int16"([[L_A_V]], [[R_A_V]])
  // CHECK-NEXT:    cond_br [[LT_A]], [[LESS_A:bb[0-9]+]], [[NOT_LESS_A:bb[0-9]+]]
  // CHECK:       [[NOT_LESS_A]]:
  // CHECK-NEXT:    [[GT_A:%[0-9]+]] = builtin "cmp_slt_Int16"([[R_A_V]], [[L_A_V]])
  // CHECK-NEXT:    cond_br [[GT_A]], [[GREATER_A:bb[0-9]+]], [[START_B:bb[0-9]+]]
  // CHECK:       [[LESS_A]]:
  // CHECK-NEXT:    [[ASC_A:%[0-9]+]] = enum $ComparisonResult, #ComparisonResult.orderedAscending!enumelt
  // CHECK-NEXT:    br [[MERGE:bb[0-9]+]]([[ASC_A]])
  // CHECK:       [[GREATER_A]]:
  // CHECK-NEXT:    [[DESC_A:%[0-9]+]] = enum $ComparisonResult, #ComparisonResult.orderedDescending!enumelt
  // CHECK-NEXT:    br [[MERGE]]([[DESC_A]])
  // CHECK:       [[START_B]]:
  // CHECK:         [[L_B:%[0-9]+]] = struct_extract [[LEFT]], #A.b
  // CHECK-NEXT:    [[L_B_V:%[0-9]+]] = struct_extract [[L_B]], #Int16._value
  // CHECK-NEXT:    [[R_B:%[0-9]+]] = struct_extract [[RIGHT]], #A.b
  // CHECK-NEXT:    [[R_B_V:%[0-9]+]] = struct_extract [[R_B]], #Int16._value
  // CHECK-NEXT:    [[LT_B:%[0-9]+]] = builtin "cmp_slt_Int16"([[L_B_V]], [[R_B_V]])
  // CHECK-NEXT:    cond_br [[LT_B]], [[LESS_B:bb[0-9]+]], [[NOT_LESS_B:bb[0-9]+]]
  // CHECK:       [[NOT_LESS_B]]:
  // CHECK-NEXT:    [[GT_B:%[0-9]+]] = builtin "cmp_slt_Int16"([[R_B_V]], [[L_B_V]])
  // CHECK-NEXT:    cond_br [[GT_B]], [[GREATER_B:bb[0-9]+]], [[START_C:bb[0-9]+]]
  // CHECK:       [[LESS_B]]:
  // CHECK-NEXT:    [[ASC_B:%[0-9]+]] = enum $ComparisonResult, #ComparisonResult.orderedAscending!enumelt
  // CHECK-NEXT:    br [[MERGE]]([[ASC_B]])
  // CHECK:       [[GREATER_B]]:
  // CHECK-NEXT:    [[DESC_B:%[0-9]+]] = enum $ComparisonResult, #ComparisonResult.orderedDescending!enumelt
  // CHECK-NEXT:    br [[MERGE]]([[DESC_B]])
  // CHECK:       [[START_C]]:
  // CHECK:         [[L_C:%[0-9]+]] = struct_extract [[LEFT]], #A.c
  // CHECK-NEXT:    [[L_C_V:%[0-9]+]] = struct_extract [[L_C]], #Int16._value
  // CHECK-NEXT:    [[R_C:%[0-9]+]] = struct_extract [[RIGHT]], #A.c
  // CHECK-NEXT:    [[R_C_V:%[0-9]+]] = struct_extract [[R_C]], #Int16._value
  // CHECK-NEXT:    [[LT_C:%[0-9]+]] = builtin "cmp_slt_Int16"([[L_C_V]], [[R_C_V]])
  // CHECK-NEXT:    cond_br [[LT_C]], [[LESS_C:bb[0-9]+]], [[NOT_LESS_C:bb[0-9]+]]
  // CHECK:       [[NOT_LESS_C]]:
  // CHECK-NEXT:    [[GT_C:%[0-9]+]] = builtin "cmp_slt_Int16"([[R_C_V]], [[L_C_V]])
  // CHECK-NEXT:    cond_br [[GT_C]], [[GREATER_C:bb[0-9]+]], [[SAME:bb[0-9]+]]
  // CHECK:       [[LESS_C]]:
  // CHECK-NEXT:    [[ASC_C:%[0-9]+]] = enum $ComparisonResult, #ComparisonResult.orderedAscending!enumelt
  // CHECK-NEXT:    br [[MERGE]]([[ASC_C]])
  // CHECK:       [[GREATER_C]]:
  // CHECK-NEXT:    [[DESC_C:%[0-9]+]] = enum $ComparisonResult, #ComparisonResult.orderedDescending!enumelt
  // CHECK-NEXT:    br [[MERGE]]([[DESC_C]])
  // CHECK:       [[SAME]]:
  // CHECK-NEXT:    [[SAME_V:%[0-9]+]] = enum $ComparisonResult, #ComparisonResult.orderedSame!enumelt
  // CHECK-NEXT:    br [[MERGE]]([[SAME_V]])
  // CHECK:       [[MERGE]]([[RESULT:%[0-9]+]] : $ComparisonResult):
  // CHECK:         [[ZERO:%[0-9]+]] = integer_literal ${{.*}}, 0
  // CHECK-NEXT:    [[ONE:%[0-9]+]] = integer_literal ${{.*}}, 1
  // CHECK-NEXT:    [[TWO:%[0-9]+]] = integer_literal ${{.*}}, 2
  // CHECK-NEXT:    [[SELECTED:%[0-9]+]] = select_enum [[RESULT]], case #ComparisonResult.orderedAscending!enumelt: [[ZERO]], case #ComparisonResult.orderedDescending!enumelt: [[ONE]], case #ComparisonResult.orderedSame!enumelt: [[TWO]] : ${{.*}}
  // CHECK-NEXT:    [[IS_ASC:%[0-9]+]] = builtin "cmp_eq_{{.*}}"([[SELECTED]], [[ZERO]])
  // CHECK-NEXT:    [[BOOL:%[0-9]+]] = struct $Bool ([[IS_ASC]])
  // CHECK-NEXT:    return [[BOOL]]
  // CHECK-LABEL: } // end sil function '$s4test1AV1loiySbAC_ACtFZTf4nnd_n'
  public static func <(_ left: Self, _ right: Self) -> Bool {
    ComponentwiseComparator(\.a, \.b, \.c).compare(left, right) == .orderedAscending
  }
}
