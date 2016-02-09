// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_while %s | FileCheck %s

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_while.foo
func foo() -> Int32 {
  var x : Int32 = 0
  // CHECK: [[@LINE+1]]:9 -> [[@LINE+1]]:17 : (0 + 1)
  while (x < 10) {
    x += 1
  }

  // CHECK: [[@LINE+1]]:9 -> [[@LINE+1]]:18 : (0 + 2)
  while (--x > 0) {
    if (x % 2 == 0) { continue }
  }

  // CHECK: [[@LINE+1]]:9 -> [[@LINE+1]]:18 : ((0 + 4) - 5)
  while (x < 100) {
    if (x++ == 10) { break }
  }

  // CHECK: [[@LINE+1]]:9 -> [[@LINE+1]]:18 : ((0 + 6) - 9)
  while (x < 100) {
    x += 1
    while (true) { break }
    if (x % 2 == 0) { continue }
    // CHECK: [[@LINE-1]]:33 -> [[@LINE+2]]:4 : (6 - 8)
    if (x > 30) { break }
  }

  // CHECK: [[@LINE+1]]:10 -> [[@LINE+4]]:4 : 10
  repeat {
    x -= 1
    // CHECK: [[@LINE+1]]:11 -> [[@LINE+1]]:16 : 10
  } while x > 0

  // CHECK: [[@LINE+1]]:9 -> [[@LINE+1]]:18 : ((0 + 11) - 12)
  while (x < 100) {
    if (x == 40) { // CHECK: [[@LINE]]:18 -> [[@LINE+2]]:6 : 12
      return x
    }
    x += 1
  }

  var y : Int32? = 2
  // CHECK: [[@LINE+1]]:9 -> [[@LINE+1]]:15 : ((0 + 13) - 12)
  while x > 30, let z = y {
    y = nil
  }

  // TODO: [[@LINE+1]]:9 -> [[@LINE+1]]:18 : ((0 + 14) - 12)
  while let z = y {
  }
  // CHECK: [[@LINE-1]]:4 -> [[@LINE+1]]:11 : (0 - 12)
  return x
}

// rdar://problem/24572268
// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_while.goo
// CHECK: [[@LINE+1]]:12 -> {{[0-9]+}}:2 : [[BAR:[0-9]+]]
func goo() {
  var x : Int32 = 0

  repeat { // CHECK-DAG: [[@LINE]]:10 -> [[@LINE+2]]:4 : [[RWS1:[0-9]+]]
    x += 1
  } while x < 10 // CHECK-DAG: [[@LINE]]:11 -> [[@LINE]]:17 : [[RWS1]]

  repeat { // CHECK-DAG: [[@LINE]]:10 -> [[@LINE+6]]:4 : [[RWS2:[0-9]+]]
    x += 1
    if (x % 2 == 0) { // CHECK-DAG: [[@LINE]]:21 -> [[@LINE+2]]:6 : [[CONT1:[0-9]+]]
      continue
    } // CHECK-DAG: [[@LINE]]:6 -> [[@LINE+2]]:4 : ([[RWS2]] - [[CONT1]])
    x += 1
  } while x < 20 // CHECK-DAG: [[@LINE]]:11 -> [[@LINE]]:17 : [[RWS2]]

  repeat { // CHECK-DAG: [[@LINE]]:10 -> [[@LINE+6]]:4 : [[RWS3:[0-9]+]]
    x += 1
    if (x % 2 == 0) { // CHECK-DAG: [[@LINE]]:21 -> [[@LINE+2]]:6 : [[BRK1:[0-9]+]]
      break
    } // CHECK-DAG: [[@LINE]]:6 -> [[@LINE+2]]:4 : ([[RWS3]] - [[BRK1]])
    x += 1
  } while x < 30 // CHECK-DAG: [[@LINE]]:11 -> [[@LINE]]:17 : ([[RWS3]] - [[BRK1]])

  repeat { // CHECK-DAG: [[@LINE]]:10 -> [[@LINE+10]]:4 : [[RWS4:[0-9]+]]
    x += 1
    if (x % 2 == 0) { // CHECK-DAG: [[@LINE]]:21 -> [[@LINE+2]]:6 : [[CONT2:[0-9]+]]
      continue
    } // CHECK-DAG: [[@LINE]]:6 -> [[@LINE+6]]:4 : ([[RWS4]] - [[CONT2]])
    x += 1
    if (x % 7 == 0) { // CHECK-DAG: [[@LINE]]:21 -> [[@LINE+2]]:6 : [[BRK2:[0-9]+]]
      break
    } // CHECK-DAG: [[@LINE]]:6 -> [[@LINE+2]]:4 : (([[RWS4]] - [[CONT2]]) - [[BRK2]])
    x += 1
  } while x < 40 // CHECK-DAG: [[@LINE]]:11 -> [[@LINE]]:17 : ([[RWS4]] - [[BRK2]])

  repeat { // CHECK-DAG: [[@LINE]]:10 -> [[@LINE+1]]:4 : [[RWS5:[0-9]+]]
  } while false // CHECK-DAG: [[@LINE]]:11 -> [[@LINE]]:16 : [[RWS5]]

  repeat { // CHECK-DAG: [[@LINE]]:10 -> [[@LINE+4]]:4 : [[RWS6:[0-9]+]]
    repeat { // CHECK-DAG: [[@LINE]]:12 -> [[@LINE+2]]:6 : [[RWS7:[0-9]+]]
      return
    } while false // CHECK-DAG: [[@LINE]]:13 -> [[@LINE]]:18 : [[RWS7]]
  } while false // CHECK-DAG: [[@LINE]]:11 -> [[@LINE]]:16 : [[RWS6]]
}

foo()
goo()
