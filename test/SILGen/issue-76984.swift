// RUN: %target-swift-emit-silgen %s | %FileCheck %s

func f(_ c: [any RandomAccessCollection<Int>]) {
  let _ = c.compactMap(\.last)
}

// CHECK: keypath $KeyPath<any RandomAccessCollection<Int>, Optional<Int>>
// CHECK: @$s{{.*}}SgvpSk_pSiABSkRts_XPTK : $@convention(keypath_accessor_getter) (@in_guaranteed any RandomAccessCollection<Int>) -> @out Optional<Int>
