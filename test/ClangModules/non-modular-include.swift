// RUN: rm -rf %t && mkdir -p %t
// RUN: not %swift -parse %s -I %S/Inputs/non-modular -F %S/Inputs/non-modular 2>&1 | FileCheck %s

// CHECK: error: {{.+}}/non-modular/Foo.framework/Headers/Foo.h:1: include of non-modular header inside framework module 'Foo'
// CHECK: error: could not build Objective-C module 'Foo'
// CHECK-NOT: error

import Foo

let _ = Foo.x
