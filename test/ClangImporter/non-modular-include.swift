// RUN: rm -rf %t && mkdir -p %t
// RUN: not %target-swift-frontend -typecheck %s -I %S/Inputs/non-modular -F %S/Inputs/non-modular 2>&1 | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-%target-runtime %s

// CHECK: {{.+}}/non-modular/Foo.framework/Headers/Foo.h:1:9: error: include of non-modular header inside framework module 'Foo'
// CHECK-objc: error: could not build Objective-C module 'Foo'
// CHECK-native: error: could not build C module 'Foo'
// CHECK-NOT: error

import Foo

_ = Foo.x
