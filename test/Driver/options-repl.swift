// RUN: not %swift -repl %s 2>&1 | %FileCheck -check-prefix=REPL_NO_FILES %s
// RUN: not %swift_driver -sdk "" -repl %s 2>&1 | %FileCheck -check-prefix=REPL_NO_FILES %s
// RUN: not %swift_driver -sdk "" -lldb-repl %s 2>&1 | %FileCheck -check-prefix=REPL_NO_FILES %s
// RUN: not %swift_driver -sdk "" -deprecated-integrated-repl %s 2>&1 | %FileCheck -check-prefix=REPL_NO_FILES %s

// REPL_NO_FILES: REPL mode requires no input files

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/usr/bin
// RUN: %hardlink-or-copy(from: %swift_driver_plain, to: %t/usr/bin/swift)

// RUN: %t/usr/bin/swift -sdk "" -deprecated-integrated-repl -### | %FileCheck -check-prefix=INTEGRATED %s

// INTEGRATED: swift{{c?(\.exe)?"?}} -frontend -repl
// INTEGRATED: -module-name REPL


// RUN: %swift_driver -sdk "" -lldb-repl -### | %FileCheck -check-prefix=LLDB %s
// RUN: %swift_driver -sdk "" -lldb-repl -D A -DB -D C -DD -L /path/to/libraries -L /path/to/more/libraries -F /path/to/frameworks -lsomelib -framework SomeFramework -sdk / -I "this folder" -module-name Test -target %target-triple -### | %FileCheck -check-prefix=LLDB-OPTS %s

// LLDB: lldb{{(\.exe)?"?}} {{"?}}--repl=
// LLDB-NOT: -module-name
// LLDB-NOT: -target

// LLDB-OPTS: lldb{{(\.exe)?"?}} "--repl=
// LLDB-OPTS-DAG: -target {{[^ ]+}}
// LLDB-OPTS-DAG: -D A -D B -D C -D D
// LLDB-OPTS-DAG: -sdk /
// LLDB-OPTS-DAG: -L /path/to/libraries
// LLDB-OPTS-DAG: -L /path/to/more/libraries
// LLDB-OPTS-DAG: -F /path/to/frameworks
// LLDB-OPTS-DAG: -lsomelib
// LLDB-OPTS-DAG: -framework SomeFramework
// LLDB-OPTS-DAG: -I \"this folder\"
// LLDB-OPTS: "


// Test LLDB detection, first in a clean environment, then in one that looks
// like the Xcode installation environment. We use hard links to make sure
// the Swift driver really thinks it's been moved.

// RUN: %t/usr/bin/swift -sdk "" -repl -### | %FileCheck -check-prefix=INTEGRATED %s
// RUN: %t/usr/bin/swift -sdk "" -### | %FileCheck -check-prefix=INTEGRATED %s

// RUN: touch %t/usr/bin/lldb
// RUN: chmod +x %t/usr/bin/lldb
// RUN: %t/usr/bin/swift -sdk "" -repl -### | %FileCheck -check-prefix=LLDB %s
// RUN: %t/usr/bin/swift -sdk "" -### | %FileCheck -check-prefix=LLDB %s
