// RUN: %swift -emit-sil %s | FileCheck %s

func fizzbuzz(i:Int) -> String {
  return i % 3 == 0
    ? "fizz"
    : i % 5 == 0
    ? "buzz"
    : "\(i)"
  // CHECK: condbranch {{%.*}}, [[OUTER_TRUE:bb[0-9]+]], [[OUTER_FALSE:bb[0-9]+]]
  // CHECK: [[OUTER_TRUE]]:
  // CHECK: br [[OUTER_CONT:bb[0-9]+]]
  // CHECK: [[OUTER_FALSE]]:
  // CHECK: [[OUTER_FALSE_TMP:%.*]] = alloc_var stack
  // CHECK: dealloc_var stack [[OUTER_FALSE_TMP]]
  // CHECK: condbranch {{%.*}}, [[INNER_TRUE:bb[0-9]+]], [[INNER_FALSE:bb[0-9]+]]
  // CHECK: [[INNER_TRUE]]:
  // CHECK: br [[INNER_CONT:bb[0-9]+]]
  // CHECK: [[INNER_FALSE]]:
  // CHECK: [[INNER_FALSE_TMP:%.*]] = alloc_var stack
  // CHECK: dealloc_var stack [[INNER_FALSE_TMP]]
  // CHECK: br [[INNER_CONT]]
  // CHECK: [[INNER_CONT]]({{.*}}):
  // CHECK: br [[OUTER_CONT]]
  // CHECK: [[OUTER_CONT]]({{.*}}):
  // CHECK: return
}
