// RUN: %target-repl-run-simple-swift | FileCheck %s

// REQUIRES: swift_repl

// <rdar://problem/13694699> - Type metadata was getting re-emitted with every

struct X {}

1+1 // CHECK: Int = 2
1+1 // CHECK: Int = 2
