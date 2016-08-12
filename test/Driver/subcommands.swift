// Check that each of 'swift', 'swift repl', 'swift run' invoke the REPL.
//
// REQUIRES: swift_interpreter
//
// RUN: %swift_driver_plain -### 2>&1 | %FileCheck -check-prefix=CHECK-SWIFT-INVOKES-REPL %s
// RUN: %swift_driver_plain run -### 2>&1 | %FileCheck -check-prefix=CHECK-SWIFT-INVOKES-REPL %s
// RUN: %swift_driver_plain repl -### 2>&1 | %FileCheck -check-prefix=CHECK-SWIFT-INVOKES-REPL %s
//
// CHECK-SWIFT-INVOKES-REPL: {{.*}}/swift -frontend -repl


// Check that 'swift -', 'swift t.swift', and 'swift /path/to/file' invoke the interpreter
// (for shebang line use). We have to run these since we can't get the driver to
// dump what it is doing and test the argv[1] processing.
//
// RUN: rm -rf %t.dir
// RUN: mkdir -p %t.dir/subpath
// RUN: echo "print(\"exec: \" + #file)" > %t.dir/stdin
// RUN: echo "print(\"exec: \" + #file)" > %t.dir/t.swift
// RUN: echo "print(\"exec: \" + #file)" > %t.dir/subpath/build
// RUN: cd %t.dir && %swift_driver_plain - < %t.dir/stdin -### 2>&1 | %FileCheck -check-prefix=CHECK-SWIFT-STDIN-INVOKES-INTERPRETER %s
// CHECK-SWIFT-STDIN-INVOKES-INTERPRETER: exec: <stdin>
// RUN: cd %t.dir && %swift_driver_plain t.swift -### 2>&1 | %FileCheck -check-prefix=CHECK-SWIFT-SUFFIX-INVOKES-INTERPRETER %s
// CHECK-SWIFT-SUFFIX-INVOKES-INTERPRETER: exec: t.swift
// RUN: cd %t.dir && %swift_driver_plain subpath/build -### 2>&1 | %FileCheck -check-prefix=CHECK-SWIFT-PATH-INVOKES-INTERPRETER %s
// CHECK-SWIFT-PATH-INVOKES-INTERPRETER: exec: subpath/build


// Check that 'swift foo' invokes 'swift-foo'.
//
// RUN: rm -rf %t.dir
// RUN: mkdir -p %t.dir
// RUN: echo "#!/bin/sh" > %t.dir/swift-foo
// RUN: echo "echo \"exec: \$0\"" >> %t.dir/swift-foo
// RUN: chmod +x %t.dir/swift-foo
// RUN: env PATH=%t.dir %swift_driver_plain foo | %FileCheck -check-prefix=CHECK-SWIFT-SUBCOMMAND %s
// CHECK-SWIFT-SUBCOMMAND: exec: {{.*}}/swift-foo
