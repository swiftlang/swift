// REQUIRES: shell
// Check that 'swift' and 'swift repl' invoke the REPL.

// RUN: rm -rf %t.dir
// RUN: mkdir -p %t.dir/usr/bin
// RUN: mkdir -p %t.dir/usr/lib
// RUN: %hardlink-or-copy(from: %swift_frontend_plain, to: %t.dir/usr/bin/swift)

// RUN: %host-library-env %t.dir/usr/bin/swift -### 2>&1 | %FileCheck -check-prefix=CHECK-SWIFT-INVOKES-REPL %s
// RUN: %host-library-env %t.dir/usr/bin/swift repl -### 2>&1 | %FileCheck -check-prefix=CHECK-SWIFT-INVOKES-REPL %s

// CHECK-SWIFT-INVOKES-REPL: {{.*}}/swift{{.*}} -repl


// RUN: %empty-directory(%t.dir)
// RUN: %empty-directory(%t.dir/subpath)
// RUN: echo "print(\"exec: \" + #file)" > %t.dir/stdin
// RUN: echo "print(\"exec: \" + #file)" > %t.dir/t.swift
// RUN: echo "print(\"exec: \" + #file)" > %t.dir/subpath/build
// RUN: cd %t.dir && %swift_driver_plain -### - < %t.dir/stdin 2>&1 | %FileCheck -check-prefix=CHECK-SWIFT-INVOKES-INTERPRETER %s
// RUN: cd %t.dir && %swift_driver_plain -### t.swift 2>&1 | %FileCheck -check-prefix=CHECK-SWIFT-INVOKES-INTERPRETER %s
// RUN: cd %t.dir && %swift_driver_plain -### subpath/build 2>&1 | %FileCheck -check-prefix=CHECK-SWIFT-INVOKES-INTERPRETER %s

// swift-frontend cannot be copied to another location with bootstrapping because
// it will not find the libswiftCore library with its relative RPATH.
// UNSUPPORTED: swift_in_compiler

// CHECK-SWIFT-INVOKES-INTERPRETER: {{.*}}/swift-frontend -frontend -interpret


// Check that 'swift foo' invokes 'swift-foo'.
//
// RUN: %empty-directory(%t.dir)
// RUN: echo "#!/bin/sh" > %t.dir/swift-foo
// RUN: echo "echo \"exec: \$0\"" >> %t.dir/swift-foo
// RUN: chmod +x %t.dir/swift-foo
// RUN: env PATH=%t.dir SWIFT_USE_OLD_DRIVER=1 %swift_driver_plain foo | %FileCheck -check-prefix=CHECK-SWIFT-SUBCOMMAND %s
// CHECK-SWIFT-SUBCOMMAND: exec: {{.*}}/swift-foo
