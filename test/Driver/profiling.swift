// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target x86_64-apple-macosx10.9 %s | FileCheck -check-prefix=CHECK -check-prefix=OSX %s

// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target x86_64-apple-ios7.1 %s | FileCheck -check-prefix=CHECK -check-prefix=IOS %s

// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target x86_64-unknown-linux-gnu %s | FileCheck -check-prefix=CHECK -check-prefix=LINUX %s

// CHECK: swift
// CHECK: -profile-generate

// OSX: bin/ld{{"? }}
// OSX: lib/swift/clang/{{[^ ]*}}/lib/darwin/libclang_rt.profile_osx.a

// IOS: bin/ld{{"? }}
// IOS: lib/swift/clang/{{[^ ]*}}/lib/darwin/libclang_rt.profile_ios.a

// LINUX: clang++{{"? }}
// LINUX: lib/swift/clang/{{[^ ]*}}/lib/linux/libclang_rt.profile-x86_64.a
