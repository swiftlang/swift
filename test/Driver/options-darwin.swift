// REQUIRES: objc_interop
// RUN: %swiftc_driver -target x86_64-apple-darwin19.0 -### %s 2>&1 | %FileCheck -check-prefix=DEFAULT %s
// DEFAULT: -target x86_64-apple-darwin{{.*}} -stack-check

// RUN: %swiftc_driver -target x86_64-apple-darwin18.5 -### %s 2>&1 | %FileCheck -check-prefix=DEFAULTOFF %s
// DEFAULTOFF-NOT: -stack-check

// RUN: %swiftc_driver -no-stack-check -### %s 2>&1 | %FileCheck -check-prefix=DISABLED %s
// DISABLED-NOT: -stack-check

// RUN: %swiftc_driver -target x86_64-apple-macosx10.15 -### %s 2>&1 | %FileCheck -check-prefix=MACOSX1015 %s
// MACOSX1015: -target x86_64-apple-macosx10.15{{.*}} -stack-check

// RUN: %swiftc_driver -target x86_64-apple-macosx10.14 -### %s 2>&1 | %FileCheck -check-prefix=MACOSX1014 %s
// MACOSX1014-NOT: -stack-check

// RUN: %swiftc_driver -target x86_64-apple-ios13 -### %s 2>&1 | %FileCheck -check-prefix=IOS13 %s
// IOS13: -stack-check

// RUN: %swiftc_driver -target x86_64-apple-ios12 -### %s 2>&1 | %FileCheck -check-prefix=IOS12 %s
// IOS12-NOT: -stack-check

// RUN: %swiftc_driver -target x86_64-apple-watchos6 -### %s 2>&1 | %FileCheck -check-prefix=WATCHOS6 %s
// WATCHOS6: -stack-check

// RUN: %swiftc_driver -target x86_64-apple-watchos5 -### %s 2>&1 | %FileCheck -check-prefix=WATCHOS5 %s
// WATCHOS5-NOT: -stack-check

// RUN: %swiftc_driver -target x86_64-apple-tvos13 -### %s 2>&1 | %FileCheck -check-prefix=TVOS %s
// TVOS-NOT: -stack-check
