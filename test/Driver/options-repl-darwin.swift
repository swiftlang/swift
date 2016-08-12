// XFAIL: freebsd, linux

// Test LLDB detection, first in a clean environment, then in one that looks
// like the Xcode installation environment. We use hard links to make sure
// the Swift driver really thinks it's been moved.

// RUN: %swift_driver -repl -### | %FileCheck -check-prefix=INTEGRATED %s
// RUN: %swift_driver -### | %FileCheck -check-prefix=INTEGRATED %s

// RUN: rm -rf %t
// RUN: mkdir -p %t/usr/bin/
// RUN: touch %t/usr/bin/lldb
// RUN: chmod +x %t/usr/bin/lldb
// RUN: ln %swift_driver_plain %t/usr/bin/swift
// RUN: %t/usr/bin/swift -repl -### | %FileCheck -check-prefix=LLDB %s
// RUN: %t/usr/bin/swift -### | %FileCheck -check-prefix=LLDB %s

// RUN: mkdir -p %t/Toolchains/Test.xctoolchain/usr/bin/
// RUN: mv %t/usr/bin/swift %t/Toolchains/Test.xctoolchain/usr/bin/swift
// RUN: %t/Toolchains/Test.xctoolchain/usr/bin/swift -repl -### | %FileCheck -check-prefix=LLDB %s

// Clean up the test executable because hard links are expensive.
// RUN: rm -rf %t/Toolchains/Test.xctoolchain/usr/bin/swift


// INTEGRATED: swift -frontend -repl
// INTEGRATED: -module-name REPL

// LLDB: lldb{{"?}} "--repl=
// LLDB-NOT: -module-name
// LLDB: -target {{[^ "]+}}
// LLDB-NOT: -module-name
// LLDB: "
