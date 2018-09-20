// REQUIRES: OS=linux-gnu
// RUN: %empty-directory(%t)

// RUN: echo 'int abc = 42;' | %clang -x c - -shared -fPIC -o %t/libabc.so
// RUN: echo 'int test() { extern int abc; return abc; }' | %clang -x c - -L%t -shared -fPIC -labc -o %t/libfoo.so

// RUN: %swift_driver -I %S/Inputs/custom-modules -L%t %s | %FileCheck %s
// CHECK: {{okay}}

// Now test a dependency on a library in the compiler's resource directory.
// RUN: %empty-directory(%t/rsrc/%target-sdk-name/%target-cpu)
// RUN: ln -s %t/libabc.so %t/rsrc/%target-sdk-name/%target-cpu
// RUN: ln -s %platform-module-dir/* %t/rsrc/%target-sdk-name/%target-cpu
// RUN: ln -s %platform-module-dir/../../shims %t/rsrc/
// RUN: %empty-directory(%t/other)
// RUN: ln -s %t/libfoo.so %t/other

// RUN: %swift_driver -I %S/Inputs/custom-modules -L%t/other -resource-dir %t/rsrc/ %s | %FileCheck %s

import foo

if test() == 42 {
  print("okay")
} else {
  print("problem")
}
