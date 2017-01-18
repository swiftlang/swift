// REQUIRES: OS=macosx

// Test LLDB detection, first in a clean environment, then in one that looks
// like the Xcode installation environment. We use hard links to make sure
// the Swift driver really thinks it's been moved.

// RUN: rm -rf %t
// RUN: mkdir -p %t/usr/bin/
// RUN: %hardlink-or-copy(from: %swift_driver_plain, to: %t/usr/bin/swift)

// RUN: %t/usr/bin/swift -repl -### | %FileCheck -check-prefix=INTEGRATED %s
// RUN: %t/usr/bin/swift -### | %FileCheck -check-prefix=INTEGRATED %s

// RUN: touch %t/usr/bin/lldb
// RUN: chmod +x %t/usr/bin/lldb
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
// LLDB-NOT: -target
// LLDB: "
