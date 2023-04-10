// RUN: %target-swift-frontend %s -g -emit-sil  -profile-generate -profile-coverage-mapping -parse-as-library -o - | %FileCheck %s

func consume<T>(_ t: T) {}

public func f<T>(collection : [T]) {
  for element in collection {
    // CHECK: increment_profiler_counter {{.*}}:[[@LINE-1]]:29, scope [[SCOPE:[0-9]+]]
    // FIXME: Ideally, these would share the same scope, or the increment should come below the variable initialization code.
    // CHECK: unchecked_take_enum_data_addr {{.*}}:[[@LINE-3]]:3, scope
    // CHECK: copy_addr {{.*}}:[[@LINE-4]]:29, scope [[SCOPE]]
    consume(element)
  }
}
