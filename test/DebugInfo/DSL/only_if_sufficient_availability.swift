// RUN: %target-swift-frontend %s -Xllvm -sil-print-debuginfo -emit-sil -g -Onone | %FileCheck %s

@resultBuilder
enum Builder {
  @available(SwiftStdlib 5.8, *)
  static func buildDebuggable(
    component: Int,
    debugInfoProvider: DSLDebugInfoProvider
  ) -> Int {
    component
  }
  
  static func buildBlock(_ components: Int...) -> [Int] {
    components
  }
}

func build(@Builder make: () -> [Int]) -> [Int] { make() }

var noDebug = build {
  // CHECK-NOT: @$s31only_if_sufficient_availabilitySaySiGyXEfU_Siff_
  1
}

if #available(SwiftStdlib 5.8, *) {
  var withDebug = build {
    // CHECK-LABEL: @$s31only_if_sufficient_availabilitySaySiGyXEfU0_Siff_
    // CHECK: debug_value %0 : $*Any, let, name "dslDebugInfo", argno 1, implicit, expr op_deref, loc
    2
  }
}

// TODO: Add buildLimitedAvailability builder
