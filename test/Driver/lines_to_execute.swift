// RUN: %swift_driver_plain -e 'print("Hello, world!")' 2>&1 | %FileCheck -check-prefix CHECK-1 %s
// CHECK-1: Hello, world!

// RUN: not %swift_driver_plain -e 'foo' 2>&1 | %FileCheck -check-prefix CHECK-2 %s
// CHECK-2: -e:1:1: error: use of unresolved identifier 'foo'

// RUN: %swift_driver_plain -e 'print("Hello, world!")' -e 'print("Hi, land!")' 2>&1 | %FileCheck -check-prefix CHECK-3 %s
// CHECK-3: Hello, world!
// CHECK-3: Hi, land!

// RUN: not %swift_driver_plain -e 'print("Hello, world!")' -e 'foo' 2>&1 | %FileCheck -check-prefix CHECK-4 %s
// CHECK-4: -e:2:1: error: use of unresolved identifier 'foo'
// CHECK-4-NOT: Hello, world!

// RUN: %swift_driver_plain -MFoundation -e 'NSLog("Hello, world!")' 2>&1 | %FileCheck -check-prefix CHECK-5 %s
// CHECK-5: swift[{{.*}}] Hello, world!