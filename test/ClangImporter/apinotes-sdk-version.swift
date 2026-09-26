// RUN: %empty-directory(%t)

// Old SDK versions get both API notes, even when a new deployment version is used.
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64-apple-macosx26.1 -sdk %S/Inputs/MacOSX13.0.sdk 2>&1 | %FileCheck -check-prefixes=COMMON,DISPATCH,OS %s
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64-apple-ios26.0 -sdk %S/Inputs/iPhoneOS13.0.sdk 2>&1 | %FileCheck -check-prefixes=COMMON,DISPATCH,OS %s
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64-apple-tvos26.0 -sdk %S/Inputs/AppleTVOS15.0.sdk 2>&1 | %FileCheck -check-prefixes=COMMON,DISPATCH,OS %s

// Check SDK versions new enough to drop Dispatch, but old enough to keep os. An
// old deployment version doesn't pick up Dispatch, and a new deployment version
// doesn't drop os. watchOS 10.0 and visionOS 1.0 are exactly the Dispatch
// cutoff, so they drop it.
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64-apple-macosx13.0 -sdk %S/Inputs/MacOSX15.1.sdk 2>&1 | %FileCheck -check-prefixes=COMMON,SKIP_DISPATCH,OS %s
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64_32-apple-watchos26.0 -sdk %S/Inputs/WatchOS10.0.sdk 2>&1 | %FileCheck -check-prefixes=COMMON,SKIP_DISPATCH,OS %s
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64-apple-xros26.0 -sdk %S/Inputs/XROS1.0.sdk 2>&1 | %FileCheck -check-prefixes=COMMON,SKIP_DISPATCH,OS %s

// New SDK versions skip both API notes, even when an old deployment version is
// used. macOS 15.2 is exactly the os cutoff, so it skips that too.
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64-apple-macosx13.0 -sdk %S/Inputs/MacOSX15.2.sdk 2>&1 | %FileCheck -check-prefixes=COMMON,SKIP_DISPATCH,SKIP_OS %s
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64-apple-xros1.1 -sdk %S/Inputs/XROS26.0.sdk 2>&1 | %FileCheck -check-prefixes=COMMON,SKIP_DISPATCH,SKIP_OS %s

// Mac Catalyst doesn't use the iOS versions, the SDK is still macOS.
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64-apple-ios18.4-macabi -sdk %S/Inputs/MacOSX13.0.sdk 2>&1 | %FileCheck -check-prefixes=COMMON,DISPATCH,OS %s
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64-apple-ios16.1-macabi -sdk %S/Inputs/MacOSX15.2.sdk 2>&1 | %FileCheck -check-prefixes=COMMON,SKIP_DISPATCH,SKIP_OS %s

// Totally new SDKs skip the API notes.
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64-apple-ios13.0 -sdk %S/Inputs/FakeOS1.0.sdk 2>&1 | %FileCheck -check-prefixes=COMMON,SKIP_DISPATCH,SKIP_OS %s

// Particularly old SDK versions have no SDKSettings, they get both API notes.
// RUN: mkdir -p %t/MacOSX10.14.sdk
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target arm64-apple-macosx26.1 -sdk %t/MacOSX10.14.sdk 2>&1 | %FileCheck -check-prefixes=COMMON,DISPATCH,OS %s

// COMMON: clang importer driver args:
// COMMON-SAME: '-iapinotes-modules' '{{.*}}.sdk{{/|\\}}usr{{/|\\}}lib{{/|\\}}swift{{/|\\}}apinotes'
// COMMON-SAME: '-iapinotes-modules' '{{.*}}lib{{/|\\}}swift{{/|\\}}apinotes'
// DISPATCH-SAME: '-iapinotes-modules' '{{.*}}lib{{/|\\}}swift{{/|\\}}apinotes-dispatch'
// SKIP_DISPATCH-NOT: apinotes-dispatch
// OS-SAME: '-iapinotes-modules' '{{.*}}lib{{/|\\}}swift{{/|\\}}apinotes-os'
// SKIP_OS-NOT: apinotes-os
