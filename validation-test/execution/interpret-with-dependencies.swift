// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// REQUIRES: swift_interpreter

// RUN: echo 'int abc = 42;' | %clang -x c - -dynamiclib -Xlinker -install_name -Xlinker libabc.dylib -o %t/libabc.dylib
// RUN: echo 'int test() { extern int abc; return abc; }' | %clang -x c - -L%t -dynamiclib -labc -o %t/libfoo.dylib

// RUN: %swift_driver -I %S/Inputs/custom-modules -L%t %s | %FileCheck %s
// CHECK: {{okay}}

// Now test a dependency on a library in the compiler's resource directory.
// RUN: %empty-directory(%t/rsrc/%target-sdk-name)
// RUN: ln -s %t/libabc.dylib %t/rsrc/%target-sdk-name/
// RUN: ln -s %platform-module-dir/* %t/rsrc/%target-sdk-name/
// RUN: ln -s %platform-module-dir/../shims %t/rsrc/
// RUN: %empty-directory(%t/other)
// RUN: ln -s %t/libfoo.dylib %t/other

// RUN: %swift_driver -I %S/Inputs/custom-modules -L%t/other -resource-dir %t/rsrc/ %s | %FileCheck %s

import foo

if test() == 42 {
  print("okay")
} else {
  print("problem")
}
