// rdar://143334632

// RUN: %target-swift-emit-silgen -enable-library-evolution %s | %FileCheck %s

public struct Foo {
  // CHECK-LABEL: sil {{.*}} @$s{{.*}}3FooV3foo
  mutating func foo() -> Int {
    // CHECK: [[BEGIN_READ:%.*]] = begin_access [read] [unknown] %0
    // CHECK: ([[RESULT:%.*]], [[TOKEN:%.*]]) = begin_apply %{{.*}}([[BEGIN_READ]])
    // CHECK: end_apply [[TOKEN]]
    // CHECK: end_access [[BEGIN_READ]]
    // CHECK: return [[RESULT]]
    return self[]
  }

  var object : AnyObject

  subscript() -> Int {
      _read {fatalError()}
      _modify {fatalError()}
  }
}
