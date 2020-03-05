// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-library -o %t/%target-library-name(Test) -emit-module -module-link-name Test -DWRONG_MODULE -wmo %s
// RUN: cp  %t/%target-library-name(Test)  %t/wrong_%target-library-name(Test)
// RUN: %target-build-swift -emit-library -o %t/%target-library-name(Test) -emit-module -module-link-name Test -DMODULE -wmo %s
// RUN: cp  %t/wrong_%target-library-name(Test)  %t/%target-library-name(Test)
// RUN: not %target-build-swift -I %t -L %t -wmo -o %t/a.out -DMAIN %s 2>&1 | %FileCheck %s


// CHECK: Undefined symbols
// CHECK-NEXT: "_$s4Test{{.*}}Tw"

#if MODULE

@inlinable
public func foo(_ x: Int) -> Int {
  return x + 1
}

#endif

#if WRONG_MODULE

@inlinable
public func foo(_ x: Int) -> Int {
  return x + 2
}

#endif

#if MAIN

import Test

public func callit() {
  print(foo(27))
}
 
#endif
