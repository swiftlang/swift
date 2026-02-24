// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -suppress-warnings -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_foreach_borrowing -enable-experimental-feature BorrowingForLoop -disable-availability-checking %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir -enable-experimental-feature BorrowingForLoop -disable-availability-checking %s

// REQUIRES: swift_feature_BorrowingForLoop

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_foreach_borrowing.forEachBasic
func forEachBasic(seq: borrowing Span<Int>) -> Int {
  var sum: Int = 0

  // CHECK: [[@LINE+1]]:16 -> [[@LINE+3]]:4 : 1
  for x in seq {
    sum += x
  }

  // CHECK: [[@LINE+3]]:33 -> [[@LINE+4]]:4 : (2 - 3)
  for x in seq {
    sum += x
    if (x % 2 == 0) { continue }
  }

  // CHECK: [[@LINE+3]]:26 -> [[@LINE+4]]:4 : (4 - 5)
  for x in seq {
    sum += x
    if (x == 3) { break }
  }

  return sum
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_foreach_borrowing.nestedForEach
func nestedForEach(seq1: borrowing Span<Int>, seq2: borrowing Span<Int>) -> Int {
  var sum: Int = 0

  // CHECK: [[@LINE+1]]:17 -> [[@LINE+6]]:4 : 1
  for x in seq1 {
    // CHECK: [[@LINE+1]]:19 -> [[@LINE+3]]:6 : 2
    for y in seq2 {
      sum += x + y
    }
  }

  return sum
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_foreach_borrowing.forEachWithBreak
func forEachWithBreak(seq: borrowing Span<Int>) -> Int {
  var sum: Int = 0

  // CHECK: [[@LINE+3]]:26 -> [[@LINE+4]]:4 : (1 - 2)
  for x in seq {
    sum += x
    if (x == 3) { break }
  }

  return sum
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_foreach_borrowing.forEachWithWhere
func forEachWithWhere(seq: borrowing Span<Int>) -> Int {
  var sum: Int = 0

  // CHECK: [[@LINE+1]]:28 -> [[@LINE+3]]:4 : 1
  for x in seq where x > 2 {
    sum += x
  }

  // CHECK: [[@LINE+3]]:26 -> [[@LINE+4]]:4 : (3 - 4)
  for x in seq where x < 10 {
    sum += x
    if (x == 4) { break }
  }

  return sum
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_foreach_borrowing.forEachWithReturn
func forEachWithReturn(seq: borrowing Span<Int>) -> Int {
  var sum: Int = 0

  // CHECK: [[@LINE+1]]:16 -> [[@LINE+6]]:4 : 1
  for x in seq {
    if (x == 3) { // CHECK: [[@LINE]]:17 -> [[@LINE+2]]:6 : 2
      return sum
    } // CHECK: [[@LINE]]:6 -> [[@LINE+2]]:4 : (1 - 2)
    sum += x
  } // CHECK: [[@LINE]]:4 -> [[@LINE+2]]:13 : (0 - 2)

  return sum
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_foreach_borrowing.forEachWithContinue
func forEachWithContinue(seq: borrowing Span<Int>) -> Int {
  var sum: Int = 0

  // CHECK: [[@LINE+1]]:16 -> [[@LINE+6]]:4 : 1
  for x in seq {
    if (x % 2 == 0) { // CHECK: [[@LINE]]:21 -> [[@LINE+2]]:6 : 2
      continue
    } // CHECK: [[@LINE]]:6 -> [[@LINE+2]]:4 : (1 - 2)
    sum += x
  }

  return sum
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_foreach_borrowing.forEachMultipleConditions
func forEachMultipleConditions(seq: borrowing Span<Int>) -> Int {
  var sum: Int = 0

  // CHECK: [[@LINE+1]]:16 -> [[@LINE+10]]:4 : 1
  for x in seq {
    if (x % 2 == 0) { // CHECK: [[@LINE]]:21 -> [[@LINE+2]]:6 : 2
      continue
    } // CHECK: [[@LINE]]:6 -> [[@LINE+6]]:4 : (1 - 2)
    sum += x
    if (x > 7) { // CHECK: [[@LINE]]:16 -> [[@LINE+2]]:6 : 3
      break
    } // CHECK: [[@LINE]]:6 -> [[@LINE+2]]:4 : ((1 - 2) - 3)
    sum += 1
  }

  return sum
}
