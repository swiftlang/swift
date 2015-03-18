// RUN: not %swift -parse -target arm64-apple-ios7 -Xcc -### %s 2>&1 | FileCheck -check-prefix=TARGETCPU1 %s
// TARGETCPU1: "-target-cpu" "cyclone"

// RUN: not %swift -parse -target arm64-apple-tvos9 -Xcc -### %s 2>&1 | FileCheck -check-prefix=APPLETVTARGETCPU1 %s
// APPLETVTARGETCPU1: "-target-cpu" "cyclone"

// RUN: not %swift -parse -target armv7s-apple-tvos9 -Xcc -### %s 2>&1 | FileCheck -check-prefix=APPLETVTARGETCPU2 %s
// APPLETVTARGETCPU2: "-target-cpu" "swift"

// RUN: not %swift -parse -target armv7-apple-tvos9 -Xcc -### %s 2>&1 | FileCheck -check-prefix=APPLETVTARGETCPU3 %s
// APPLETVTARGETCPU3: "-target-cpu" "cortex-a8"

// RUN: not %swift -parse -target armv7-apple-watchos2 -Xcc -### %s 2>&1 | FileCheck -check-prefix=WATCHTARGETCPU1 %s
// WATCHTARGETCPU1: "-target-cpu" "cortex-a8"

// RUN: not %swift -parse -target arm64-apple-watchos2 -Xcc -### %s 2>&1 | FileCheck -check-prefix=WATCHTARGETCPU2 %s
// WATCHTARGETCPU2: "-target-cpu" "cyclone"

// RUN: not %swift -parse -target armv7s-apple-ios7 -Xcc -### %s 2>&1 | FileCheck -check-prefix=TARGETCPU2 %s
// TARGETCPU2: "-target-cpu" "swift"

// RUN: not %swift -parse -target armv7-apple-ios7 -Xcc -### %s 2>&1 | FileCheck -check-prefix=TARGETCPU3 %s
// TARGETCPU3: "-target-cpu" "cortex-a8"

// RUN: not %swift -parse -target i386-apple-ios7 -Xcc -### %s 2>&1 | FileCheck -check-prefix=SIMULATOR_CPU %s
// SIMULATOR_CPU: "-target-cpu" "yonah"

// RUN: not %swift -parse -target i386-apple-watchos2 -Xcc -### %s 2>&1 | FileCheck -check-prefix=WATCHSIMULATOR_CPU %s
// WATCHSIMULATOR_CPU: "-target-cpu" "yonah"

// RUN: not %swift -parse -target x86_64-apple-ios7 -Xcc -### %s 2>&1 | FileCheck -check-prefix=SIMULATOR64_CPU %s
// SIMULATOR64_CPU: "-target-cpu" "core2"

// RUN: not %swift -parse -target x86_64-apple-tvos9 -Xcc -### %s 2>&1 | FileCheck -check-prefix=APPLETVSIMULATOR64_CPU %s
// APPLETVSIMULATOR64_CPU: "-target-cpu" "core2"

// RUN: not %swift -parse -target x86_64-apple-watchos2 -Xcc -### %s 2>&1 | FileCheck -check-prefix=WATCHSIMULATOR64_CPU %s
// WATCHSIMULATOR64_CPU: "-target-cpu" "core2"

