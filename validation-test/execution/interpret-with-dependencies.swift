// REQUIRES: OS=macosx
// RUN: rm -rf %t && mkdir %t

// RUN: echo 'int abc = 42;' | %clang -x c - -dynamiclib -Xlinker -install_name -Xlinker libabc.dylib -o %t/libabc.dylib
// RUN: echo 'int test() { extern int abc; return abc; }' | %clang -x c - -L%t -dynamiclib -labc -o %t/libfoo.dylib

// RUN: %swift_driver -I %S/Inputs/custom-modules -L%t %s | FileCheck %s
// CHECK: {{okay}}

import foo

if test() == 42 {
  print("okay")
} else {
  print("problem")
}
