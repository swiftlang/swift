// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -I%t %t/caller.swift -emit-ir | %FileCheck %s

//--- c_funcs.h

__attribute__((swiftcall))
extern void with_swiftcc(void);

//--- module.modulemap

module c_funcs {
  header "c_funcs.h"
}

//--- caller.swift

import c_funcs

func test() {
  // CHECK: call swiftcc void @with_swiftcc()
  with_swiftcc()
}
// CHECK: declare {{.*}}swiftcc void @with_swiftcc()

test()
