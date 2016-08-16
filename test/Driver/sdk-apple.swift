// XFAIL: freebsd, linux

// Test SDK detection for immediate mode.
// RUN: rm -rf %t && mkdir -p %t/usr/bin/

// RUN: cp %S/Inputs/xcrun-bad.sh %t/usr/bin/xcrun
// RUN: env PATH=%t/usr/bin %swift_driver_plain -deprecated-integrated-repl -### | %FileCheck -check-prefix=NOSDK %s
// RUN: env PATH=%t/usr/bin %swift_driver_plain -### %s | %FileCheck -check-prefix=NOSDK %s

// NOSDK-NOT: -sdk

// RUN: cp %S/Inputs/xcrun.sh %t/usr/bin/xcrun
// RUN: env PATH=%t/usr/bin %swift_driver_plain -deprecated-integrated-repl -### | %FileCheck -check-prefix=XCRUN-SDK %s
// RUN: env PATH=%t/usr/bin %swift_driver_plain -### %s | %FileCheck -check-prefix=XCRUN-SDK %s

// XCRUN-SDK: -sdk /path/to/sdk

// RUN: cp %S/Inputs/xcrun-empty.sh %t/usr/bin/xcrun
// RUN: env PATH=%t/usr/bin %swift_driver_plain -deprecated-integrated-repl -### | %FileCheck -check-prefix=ROOT-SDK %s
// RUN: env PATH=%t/usr/bin %swift_driver_plain -### %s | %FileCheck -check-prefix=ROOT-SDK %s

// ROOT-SDK: -sdk /{{ |$}}

// RUN: rm -rf %t && mkdir %t
// RUN: mkdir %t/MacOSX10.8.sdk && not %swift_driver -sdk %t/MacOSX10.8.sdk -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/MacOSX10.9.sdk && not %swift_driver -sdk %t/MacOSX10.9.sdk -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/MacOSX10.9.Internal.sdk && not %swift_driver -sdk %t/MacOSX10.9.Internal.sdk -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/MacOSX10.10.sdk && not %swift_driver -sdk %t/MacOSX10.10.sdk -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/MacOSX10.10.Internal.sdk && not %swift_driver -sdk %t/MacOSX10.10.Internal.sdk -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/MacOSX10.11.sdk && not %swift_driver -sdk %t/MacOSX10.11.sdk -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/MacOSX10.11.Internal.sdk && not %swift_driver -sdk %t/MacOSX10.11.Internal.sdk -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/MacOSX10.12.sdk && %swift_driver -sdk %t/MacOSX10.12.sdk -### 2>&1 | %FileCheck -check-prefix=SDK-OKAY %s
// RUN: mkdir %t/OSX12.sdk && %swift_driver -sdk %t/OSX12.sdk -### 2>&1 | %FileCheck -check-prefix=SDK-OKAY %s

// RUN: not %swift_driver -sdk %t/MacOSX10.9.sdk/ -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: not %swift_driver -sdk %t/MacOSX10.9.Internal.sdk/ -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: not %swift_driver -sdk %t/MacOSX10.10.sdk/ -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: not %swift_driver -sdk %t/MacOSX10.10.Internal.sdk/ -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: not %swift_driver -sdk %t/MacOSX10.11.sdk/ -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: not %swift_driver -sdk %t/MacOSX10.11.Internal.sdk/ -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: %swift_driver -sdk %t/MacOSX10.12.sdk/ -### 2>&1 | %FileCheck -check-prefix=SDK-OKAY %s
// RUN: %swift_driver -sdk %t/MacOSX10.12.Internal.sdk/ -### 2>&1 | %FileCheck -check-prefix=SDK-OKAY %s

// RUN: mkdir %t/iPhoneOS7.0.sdk && not %swift_driver -sdk %t/iPhoneOS7.0.sdk -target x86_64-apple-ios7 -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/iPhoneOS7.0.Internal.sdk && not %swift_driver -sdk %t/iPhoneOS7.0.Internal.sdk -target x86_64-apple-ios7 -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/iPhoneOS8.0.sdk && not %swift_driver -sdk %t/iPhoneOS8.0.sdk -target x86_64-apple-ios7 -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/iPhoneOS8.0.Internal.sdk && not %swift_driver -sdk %t/iPhoneOS8.0.Internal.sdk -target x86_64-apple-ios7 -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/iPhoneOS9.0.sdk && not %swift_driver -sdk %t/iPhoneOS9.0.sdk -target x86_64-apple-ios7 -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/iPhoneOS10.0.sdk && %swift_driver -sdk %t/iPhoneOS10.0.sdk -target x86_64-apple-ios7 -### 2>&1 | %FileCheck -check-prefix=SDK-OKAY %s

// RUN: mkdir %t/tvOS8.0.sdk && not %swift_driver -sdk %t/tvOS8.0.sdk -target x86_64-apple-tvos9 -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/tvOS8.0.Internal.sdk && not %swift_driver -sdk %t/tvOS8.0.Internal.sdk -target x86_64-apple-tvos9 -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/tvOS9.0.sdk && not %swift_driver -sdk %t/tvOS9.0.sdk -target x86_64-apple-tvos9 -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/tvOS10.0.sdk && %swift_driver -sdk %t/tvOS10.0.sdk -target x86_64-apple-tvos9 -### 2>&1 | %FileCheck -check-prefix=SDK-OKAY %s

// RUN: mkdir %t/watchOS1.0.sdk && not %swift_driver -sdk %t/watchOS1.0.sdk -target x86_64-apple-watchos2 -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/watchOS1.0.Internal.sdk && not %swift_driver -sdk %t/watchOS1.0.Internal.sdk -target x86_64-apple-watchos2 -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/watchOS2.0.sdk && not %swift_driver -sdk %t/watchOS2.0.sdk -target x86_64-apple-watchos2 -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/watchOS3.0.sdk && %swift_driver -sdk %t/watchOS3.0.sdk -target x86_64-apple-watchos2 -### 2>&1 | %FileCheck -check-prefix=SDK-OKAY %s

// RUN: mkdir %t/iPhoneSimulator7.0.sdk && not %swift_driver -sdk %t/iPhoneSimulator7.0.sdk -target x86_64-apple-ios7 -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/iPhoneSimulator8.0.sdk && not %swift_driver -sdk %t/iPhoneSimulator8.0.sdk -target x86_64-apple-ios7 -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/AppleTVSimulator8.0.sdk && not %swift_driver -sdk %t/AppleTVSimulator8.0.sdk -target x86_64-apple-tvos9 -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s
// RUN: mkdir %t/WatchSimulator1.0.sdk && not %swift_driver -sdk %t/WatchSimulator1.0.sdk -target i386-apple-watchos2 -### 2>&1 | %FileCheck -check-prefix=SDK-TOO-OLD %s

// RUN: mkdir %t/iPhoneOS.sdk && %swift_driver -sdk %t/iPhoneOS.sdk -target x86_64-apple-ios7 -### 2>&1 | %FileCheck -check-prefix=SDK-OKAY %s
// RUN: mkdir %t/tvOS.sdk && %swift_driver -sdk %t/tvOS.sdk -target x86_64-apple-tvos9 -### 2>&1 | %FileCheck -check-prefix=SDK-OKAY %s
// RUN: mkdir %t/watchOS.sdk && %swift_driver -sdk %t/watchOS.sdk -target x86_64-apple-watchos2 -### 2>&1 | %FileCheck -check-prefix=SDK-OKAY %s

// RUN: mkdir %t/custom-sdk && %swift_driver -sdk %t/custom-sdk -### 2>&1 | %FileCheck -check-prefix=SDK-OKAY %s

// SDK-TOO-OLD: error: Swift does not support the SDK '{{.+}}.sdk'{{$}}
// SDK-OKAY: -sdk {{.*}}/{{[^/ ]+}}sdk
