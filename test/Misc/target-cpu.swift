// RUN: not %swift -parse -target arm64-apple-ios7 -Xcc -### %s 2>&1 | FileCheck -check-prefix=TARGETCPU1 %s
// TARGETCPU1: "-target-cpu" "cyclone"

// RUN: not %swift -parse -target armv7s-apple-ios7 -Xcc -### %s 2>&1 | FileCheck -check-prefix=TARGETCPU2 %s
// TARGETCPU2: "-target-cpu" "swift"

// RUN: not %swift -parse -target armv7-apple-ios7 -Xcc -### %s 2>&1 | FileCheck -check-prefix=TARGETCPU3 %s
// TARGETCPU3: "-target-cpu" "cortex-a8"

// RUN: not %swift -parse -target i386-apple-ios7 -Xcc -### %s 2>&1 | FileCheck -check-prefix=SIMULATOR_CPU %s
// SIMULATOR_CPU: "-target-cpu" "yonah"

// RUN: not %swift -parse -target x86_64-apple-ios7 -Xcc -### %s 2>&1 | FileCheck -check-prefix=SIMULATOR64_CPU %s
// SIMULATOR64_CPU: "-target-cpu" "core2"
