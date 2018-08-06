// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s
// SR-8398
func fibonacci(_ n: Int) -> Int {
  var cache: [Int: Int] = [:]

  func recursive(_ m: Int) -> Int {
    return cache[m] ?? {
      // Make sure cache is only captured once in the closure
      // CHECK: implicit closure #1 in recursive #1
      // CHECK-LABEL: sil private [transparent] @{{.*}}9fibonacci{{.*}}9recursive{{.*}} : $@convention(thin) (Int, @guaranteed { var Dictionary<Int, Int> }) -> (Int, @error Error)
      // CHECK: closure #1 in implicit closure #1 in recursive #1
      // CHECK-LABEL: sil private @{{.*}}9fibonacci{{.*}}9recursive{{.*}} : $@convention(thin) (Int, @guaranteed { var Dictionary<Int, Int> }) -> Int
      let output = m < 2 ? m : recursive(m - 1) + recursive(m - 2)
      cache[m] = output
      return output
      }()
  }
  return recursive(n)
}

