// RUN: %swift < %s -repl | FileCheck %s
// <rdar://problem/13694699> - Type metadata was getting re-emitted with every

struct X {}

1+1 // CHECK: Int = 2
1+1 // CHECK: Int = 2
