// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5  -disable-availability-checking -enable-experimental-flow-sensitive-concurrent-captures | %FileCheck %s
// REQUIRES: concurrency

func acceptsConcurrent(_: @escaping @Sendable () -> Int) { }

func concurrentWithCaptures(i: Int) -> Int {
  var i = i

  // CHECK: sil private [ossa] @$s4test22concurrentWithCaptures1iS2i_tFSiyYbcfU_ : $@convention(thin) @Sendable (@guaranteed { var Int }) -> Int
  acceptsConcurrent {
    i + 1
  }
  i = i + 1

  // CHECK: sil private [ossa] @$s4test22concurrentWithCaptures1iS2i_tF13localFunctionL_SiyYbF : $@convention(thin) @Sendable (@guaranteed { var Int }) -> Int
  @Sendable func localFunction() -> Int {
    return i + 1
  }
  acceptsConcurrent(localFunction)
  i = i + 1

  return i
}
