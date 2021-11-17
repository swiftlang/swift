// RUN: %target-swift-frontend -dump-ast %s | %FileCheck %s

struct Q {
  var x: Int
}

@dynamicMemberLookup
struct R {
  subscript(dynamicMember dynamicMember: KeyPath<Q, Int>) -> Int {
    fatalError()
  }
}

@dynamicMemberLookup
struct S {
  subscript(dynamicMember dynamicMember: KeyPath<R, Int>) -> Int {
    fatalError()
  }
}

// rdar://85236369 - We shouldn't synthesize a parsed root or path, we should
// just have the original parsed root.

// CHECK-NOT: (parsed_root
// CHECK-NOT: (parsed_path
// CHECK: (parsed_root
// CHECK-NOT: (parsed_path
_ = \S.x
