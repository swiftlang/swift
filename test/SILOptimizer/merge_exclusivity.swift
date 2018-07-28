// RUN: %target-swift-frontend -O -enforce-exclusivity=checked -emit-sil  -primary-file %s | %FileCheck %s --check-prefix=TESTSIL
// REQUIRES: optimized_stdlib,asserts

public var check: UInt64 = 0

@inline(never)
func sum(_ x: UInt64, _ y: UInt64) -> UInt64 {
  return x &+ y
}

// TESTSIL-LABEL: sil [noinline] @$S17merge_exclusivity10MergeTest1yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$S17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL: [[B2:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: br bb2
// TESTSIL: bb1
// TESTSIL: end_access [[B2]]
// TESTSIL-NEXT: tuple
// TESTSIL-NEXT: return
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$S17merge_exclusivity10MergeTest1yySiF'
@inline(never)
public func MergeTest1(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      check = sum(check, UInt64(e))
      if (e == 0) {
        check = sum(check, UInt64(1))
      }
      else {
        check = sum(check, UInt64(2))
      }
    }
  }
}

// TESTSIL-LABEL: sil [noinline] @$S17merge_exclusivity10MergeTest2yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$S17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL: [[B2:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: br bb2
// TESTSIL: bb1
// TESTSIL: end_access [[B2]]
// TESTSIL-NEXT: tuple
// TESTSIL-NEXT: return
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$S17merge_exclusivity10MergeTest2yySiF'
@inline(never)
public func MergeTest2(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      if (e == 0) {
        check = sum(check, UInt64(1))
      }
      else {
        check = sum(check, UInt64(2))
      }
    }
  }
}

// TESTSIL-LABEL: sil [noinline] @$S17merge_exclusivity10MergeTest3yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$S17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL: [[B2:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: br bb2
// TESTSIL: bb1
// TESTSIL: end_access [[B2]]
// TESTSIL-NEXT: tuple
// TESTSIL-NEXT: return
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$S17merge_exclusivity10MergeTest3yySiF'
@inline(never)
public func MergeTest3(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      check = sum(check, UInt64(e))
    }
  }
}

// TESTSIL-LABEL: sil [noinline] @$S17merge_exclusivity10MergeTest4yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$S17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL: [[B2:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: br bb2
// TESTSIL: bb1
// TESTSIL: end_access [[B2]]
// TESTSIL-NEXT: tuple
// TESTSIL-NEXT: return
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$S17merge_exclusivity10MergeTest4yySiF'
@inline(never)
public func MergeTest4(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      if (e == 0) {
        check = sum(check, UInt64(1))
      }
    }
  }
}

// TESTSIL-LABEL: sil [noinline] @$S17merge_exclusivity10MergeTest5yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$S17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL: [[B2:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: br bb2
// TESTSIL: bb1
// TESTSIL: end_access [[B2]]
// TESTSIL-NEXT: tuple
// TESTSIL-NEXT: return
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$S17merge_exclusivity10MergeTest5yySiF'
@inline(never)
public func MergeTest5(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      if (e == 0) {
        check = sum(check, UInt64(1))
      }
      check = sum(check, UInt64(e))
    }
  }
}

// TESTSIL-LABEL: sil [noinline] @$S17merge_exclusivity10MergeTest6yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$S17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL: [[B2:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: br bb2
// TESTSIL: bb1
// TESTSIL: end_access [[B2]]
// TESTSIL-NEXT: tuple
// TESTSIL-NEXT: return
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$S17merge_exclusivity10MergeTest6yySiF'
@inline(never)
public func MergeTest6(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      if (e == 0) {
        check = sum(check, UInt64(1))
      }
      else {
        check = sum(check, UInt64(2))
      }
      check = sum(check, UInt64(e))
    }
  }
}

// TESTSIL-LABEL: sil [noinline] @$S17merge_exclusivity10MergeTest7yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$S17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL: [[B2:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: br bb2
// TESTSIL: bb1
// TESTSIL: end_access [[B2]]
// TESTSIL-NEXT: tuple
// TESTSIL-NEXT: return
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$S17merge_exclusivity10MergeTest7yySiF'
@inline(never)
public func MergeTest7(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      check = sum(check, UInt64(e))
      for _ in range {
        check = sum(check, UInt64(e))
      }
      check = sum(check, UInt64(e))
    }
  }
}

@inline(never)
public func foo() {
}

// TESTSIL-LABEL: sil [noinline] @$S17merge_exclusivity10MergeTest8yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$S17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL: [[B2:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: br bb2
// TESTSIL: bb1
// TESTSIL: end_access [[B2]]
// TESTSIL-NEXT: tuple
// TESTSIL-NEXT: return
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$S17merge_exclusivity10MergeTest8yySiF'
@inline(never)
public func MergeTest8(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      check = sum(check, UInt64(e))
      for _ in range {
        foo()
      }
      check = sum(check, UInt64(e))
    }
  }
}

// TESTSIL-LABEL: sil [noinline] @$S17merge_exclusivity10MergeTest9yySiF : $@convention(thin)
// TESTSIL: bb0
// TESTSIL: [[GLOBALVAR:%.*]] = global_addr @$S17merge_exclusivity5checks6UInt64Vvp
// TESTSIL: [[B1:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL: end_access [[B1]]
// TESTSIL: [[B2:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: br bb2
// TESTSIL: bb1
// TESTSIL: end_access [[B2]]
// TESTSIL-NEXT: [[B3:%.*]] = begin_access [modify] [dynamic] [no_nested_conflict] [[GLOBALVAR]]
// TESTSIL-NEXT: br bb8
// TESTSIL-NOT: begin_access
// TESTSIL: bb7
// TESTSIL: end_access [[B3]]
// TESTSIL-NEXT: tuple
// TESTSIL-NEXT: return
// TESTSIL-NOT: begin_access
// TESTSIL-LABEL: } // end sil function '$S17merge_exclusivity10MergeTest9yySiF'
@inline(never)
public func MergeTest9(_ N: Int) {
  let range = 0..<10000
  check = 0
  for _ in 1...N {
    for e in range {
      check = sum(check, UInt64(e))
      for _ in range {
        foo()
      }
      check = sum(check, UInt64(e))
    }
  }
  for _ in 1...N {
    for e in range {
      check = sum(check, UInt64(e))
      for _ in range {
        foo()
      }
      check = sum(check, UInt64(e))
    }
  }
}
