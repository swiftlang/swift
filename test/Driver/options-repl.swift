// RUN: not %swift -repl %s 2>&1 | FileCheck -check-prefix=REPL_NO_FILES %s
// RUN: not %swift_driver -repl %s 2>&1 | FileCheck -check-prefix=REPL_NO_FILES %s
// RUN: not %swift_driver -lldb-repl %s 2>&1 | FileCheck -check-prefix=REPL_NO_FILES %s
// RUN: not %swift_driver -integrated-repl %s 2>&1 | FileCheck -check-prefix=REPL_NO_FILES %s

// REPL_NO_FILES: REPL mode requires no input files


// RUN: %swift_driver -integrated-repl -### | FileCheck -check-prefix=INTEGRATED %s

// INTEGRATED: swift -frontend -repl
// INTEGRATED: -module-name REPL


// RUN: %swift_driver -lldb-repl -### | FileCheck -check-prefix=LLDB %s
// RUN: %swift_driver -lldb-repl -DA,B,C -DD -sdk / -I "this folder" -module-name Test -### | FileCheck -check-prefix=LLDB-OPTS %s

// LLDB: lldb "--repl=
// LLDB-DAG: -target {{[^ ]+}}
// LLDB-DAG: -module-name REPL
// LLDB: "

// LLDB-OPTS: lldb "--repl=
// LLDB-OPTS-DAG: -target {{[^ ]+}}
// LLDB-OPTS-DAG: -module-name Test
// LLDB-OPTS-DAG: -D A,B,C -D D
// LLDB-OPTS-DAG: -sdk /
// LLDB-OPTS-DAG: -I \"this folder\"
// LLDB-OPTS: "


// Test LLDB detection, first in a clean environment, then in one that looks
// like the Xcode installation environment. We use hard links to make sure
// the Swift driver really thinks it's been moved.

// RUN: %swift_driver -repl -experimental-prefer-lldb -### | FileCheck -check-prefix=INTEGRATED %s
// RUN: %swift_driver -experimental-prefer-lldb -### | FileCheck -check-prefix=INTEGRATED %s

// RUN: rm -rf %t
// RUN: mkdir -p %t/usr/bin/
// RUN: touch %t/usr/bin/lldb
// RUN: ln %swift_driver_plain %t/usr/bin/swift
// RUN: %t/usr/bin/swift -repl -experimental-prefer-lldb -### | FileCheck -check-prefix=LLDB %s
// RUN: %t/usr/bin/swift -experimental-prefer-lldb -### | FileCheck -check-prefix=LLDB %s

// RUN: mkdir -p %t/Toolchains/Test.xctoolchain/usr/bin/
// RUN: mv %t/usr/bin/swift %t/Toolchains/Test.xctoolchain/usr/bin/swift
// RUN: %t/Toolchains/Test.xctoolchain/usr/bin/swift -repl -experimental-prefer-lldb -### | FileCheck -check-prefix=LLDB %s

// Clean up the test executable because hard links are expensive.
// RUN: rm -rf %t/Toolchains/Test.xctoolchain/usr/bin/swift
