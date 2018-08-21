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

// RUN: %swift_driver_plain -MSwiftShims -e '_stdlib_write(1, "Hello, world!", 13)' 2>&1 | %FileCheck -check-prefix CHECK-5 %s
// CHECK-5: Hello, world!

// RUN: not %swift_driver_plain -MFnord -e 'fnord' 2>&1 | %FileCheck -check-prefix CHECK-6 %s
// CHECK-6: -M:1:8: error: no such module 'Fnord'
// CHECK-6-NOT: -e:1:1: error: use of unresolved identifier 'fnord'

// RUN: not %swift_driver_plain -MSwiftShims -e 'fnord' 2>&1 | %FileCheck -check-prefix CHECK-7 %s
// CHECK-7: -e:1:1: error: use of unresolved identifier 'fnord'

// RUN: not %swift_driver_plain -MSwiftShims 2>&1 | %FileCheck -check-prefix CHECK-8 %s
// CHECK-8: error: cannot use -M arguments to import modules without -e arguments to use them
