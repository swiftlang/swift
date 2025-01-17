// RUN: %empty-directory(%t/ForeignModule.swiftmodule)
// RUN: touch %t/ForeignModule.swiftmodule/garbage-garbage-garbage.swiftmodule

// Test format: We try to import ForeignModule with architectures besides
// garbage-garbage-garbage and check the target triple listed in the error
// message to make sure it was normalized correctly. This works in lieu of a
// mechanism to query the compiler for normalized triples.
//
// The extra flags in the RUN lines serve the following purposes:
//
// * "-parse-stdlib" ensures we don't reject any of these for not having an
//   appropriate standard library built.
// * "-Xcc -arch -Xcc i386" makes sure the clang importer doesn't reject triples
//   clang considers invalid.

import ForeignModule

// CHECK: error: could not find module 'ForeignModule'
// CHECK-SAME: '[[NORM]]'

// Run lines for individual test cases follow.

//
// OSES
//

// OS version numbers should be stripped.

// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target x86_64-apple-macosx10.42 2>&1 | %FileCheck -DNORM=x86_64-apple-macos %s

// macos, macosx, and darwin should all normalize to macos.

// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target x86_64-apple-macos10.42 2>&1 | %FileCheck -DNORM=x86_64-apple-macos %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target x86_64-apple-macosx10.42 2>&1 | %FileCheck -DNORM=x86_64-apple-macos %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target x86_64-apple-darwin46.0 2>&1 | %FileCheck -DNORM=x86_64-apple-macos %s

// ios, tvos, watchos should be accepted.

// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target arm64-apple-ios40.0 2>&1 | %FileCheck -DNORM=arm64-apple-ios %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target arm64-apple-tvos40 2>&1 | %FileCheck -DNORM=arm64-apple-tvos %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target arm64-apple-watchos9.1.1 2>&1 | %FileCheck -DNORM=arm64-apple-watchos %s

// Other OSes should be passed through without version stripping. We can't test
// a totally garbage case because we'll get diag::error_unsupported_target_os.

// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target x86_64-apple-linux40.04 2>&1 | %FileCheck -DNORM=x86_64-apple-linux40.04 %s

//
// VENDORS
//

// If the OS looks like an Apple OS, vendor should be normalized to apple.

// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target x86_64-unknown-macos10.42 2>&1 | %FileCheck -DNORM=x86_64-apple-macos %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target arm64--ios40.0 2>&1 | %FileCheck -DNORM=arm64-apple-ios %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target arm64-ibm-tvos40 2>&1 | %FileCheck -DNORM=arm64-apple-tvos %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target arm64-snapple-watchos9.1.1 2>&1 | %FileCheck -DNORM=arm64-apple-watchos %s

//
// ARCHITECTURES
//

// arm64 and aarch64 are normalized to arm64.

// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target arm64-apple-ios40.0 2>&1 | %FileCheck -DNORM=arm64-apple-ios %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target aarch64-apple-ios40.0 2>&1 | %FileCheck -DNORM=arm64-apple-ios %s

// armv7s, armv7k, armv7, arm64_32, arm64e should be accepted.

// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target armv7s-apple-ios40.0 2>&1 | %FileCheck -DNORM=armv7s-apple-ios %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target armv7k-apple-ios40.0 2>&1 | %FileCheck -DNORM=armv7k-apple-ios %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target armv7-apple-ios40.0 2>&1 | %FileCheck -DNORM=armv7-apple-ios %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target arm64_32-apple-ios40.0 2>&1 | %FileCheck -DNORM=arm64_32-apple-ios %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target arm64e-apple-ios40.0 2>&1 | %FileCheck -DNORM=arm64e-apple-ios %s

// x86_64h should be accepted.

// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target x86_64h-apple-macos10.11 2>&1 | %FileCheck -DNORM=x86_64h-apple-macos %s

// x64_64 and amd64 are normalized to x86_64.

// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target x86_64-apple-macos10.11 2>&1 | %FileCheck -DNORM=x86_64-apple-macos %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target amd64-apple-macos10.11 2>&1 | %FileCheck -DNORM=x86_64-apple-macos %s

// i[3-9]86 are normalized to i386.

// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target i386-apple-macos10.11 2>&1 | %FileCheck -DNORM=i386-apple-macos %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target i486-apple-macos10.11 2>&1 | %FileCheck -DNORM=i386-apple-macos %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target i586-apple-macos10.11 2>&1 | %FileCheck -DNORM=i386-apple-macos %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target i686-apple-macos10.11 2>&1 | %FileCheck -DNORM=i386-apple-macos %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target i786-apple-macos10.11 2>&1 | %FileCheck -DNORM=i386-apple-macos %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target i886-apple-macos10.11 2>&1 | %FileCheck -DNORM=i386-apple-macos %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target i986-apple-macos10.11 2>&1 | %FileCheck -DNORM=i386-apple-macos %s

// Other arches should be passed through. We can't test a totally garbage case
// because we'll get diag::error_unsupported_target_arch.

// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target powerpc64-apple-macos10.11 2>&1 | %FileCheck -DNORM=powerpc64-apple-macos %s

//
// ENVIRONMENTS
//

// simulator should be permitted on the non-Mac operating systems.

// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target x86_64-apple-ios40.0-simulator 2>&1 | %FileCheck -DNORM=x86_64-apple-ios-simulator %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target x86_64-apple-tvos40-simulator 2>&1 | %FileCheck -DNORM=x86_64-apple-tvos-simulator %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target x86_64-apple-watchos9.1.1-simulator 2>&1 | %FileCheck -DNORM=x86_64-apple-watchos-simulator %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target i386-apple-ios40.0-simulator 2>&1 | %FileCheck -DNORM=i386-apple-ios-simulator %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target i386-apple-tvos40-simulator 2>&1 | %FileCheck -DNORM=i386-apple-tvos-simulator %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target i386-apple-watchos9.1.1-simulator 2>&1 | %FileCheck -DNORM=i386-apple-watchos-simulator %s

// Other environments should be passed through.

// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target i386-apple-ios40.0-in_spaaaaaaace 2>&1 | %FileCheck -DNORM=i386-apple-ios-in_spaaaaaaace %s

//
// DARWIN ONLY
//

// Non-isDarwinOS() OSes should have no normalization applied.

// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target x86_64-unknown-linux40.04 2>&1 | %FileCheck -DNORM=x86_64-unknown-linux40.04 %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target aarch64--linux-android 2>&1 | %FileCheck -DNORM=aarch64--linux-android %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target amd64-ibm-linux40.04 2>&1 | %FileCheck -DNORM=amd64-ibm-linux40.04 %s
// RUN: not %target-swift-frontend %s -typecheck -I %t -parse-stdlib -Xcc -arch -Xcc i386 -target i986-snapple-haiku 2>&1 | %FileCheck -DNORM=i986-snapple-haiku %s
