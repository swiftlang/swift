// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target x86_64-apple-macosx10.9 %s | %FileCheck -check-prefix=CHECK -check-prefix=OSX %s

// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target x86_64-apple-ios7.1 -resource-dir %S/Inputs/fake-resource-dir-old/lib/swift/ %s | %FileCheck -check-prefix=CHECK -check-prefix=IOS %s
// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target arm64-apple-ios7.1 -resource-dir %S/Inputs/fake-resource-dir-old/lib/swift/ %s | %FileCheck -check-prefix=CHECK -check-prefix=IOS %s

// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target x86_64-apple-ios7.1 -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ %s | %FileCheck -check-prefix=CHECK -check-prefix=IOSSIM %s
// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target arm64-apple-ios7.1 -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ %s | %FileCheck -check-prefix=CHECK -check-prefix=IOS %s

// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target x86_64-apple-tvos9.0 -resource-dir %S/Inputs/fake-resource-dir-old/lib/swift/ %s | %FileCheck -check-prefix=CHECK -check-prefix=tvOS %s
// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target arm64-apple-tvos9.0 -resource-dir %S/Inputs/fake-resource-dir-old/lib/swift/ %s | %FileCheck -check-prefix=CHECK -check-prefix=tvOS %s

// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target x86_64-apple-tvos9.0 -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ %s | %FileCheck -check-prefix=CHECK -check-prefix=tvOS_SIM %s
// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target arm64-apple-tvos9.0 -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ %s | %FileCheck -check-prefix=CHECK -check-prefix=tvOS %s

// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target i386-apple-watchos2.0 -resource-dir %S/Inputs/fake-resource-dir-old/lib/swift/ %s | %FileCheck -check-prefix=CHECK -check-prefix=watchOS %s
// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target armv7k-apple-watchos2.0 -resource-dir %S/Inputs/fake-resource-dir-old/lib/swift/ %s | %FileCheck -check-prefix=CHECK -check-prefix=watchOS %s
// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target i386-apple-watchos2.0 -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ %s | %FileCheck -check-prefix=CHECK -check-prefix=watchOS_SIM %s
// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target armv7k-apple-watchos2.0 -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ %s | %FileCheck -check-prefix=CHECK -check-prefix=watchOS %s

// RUN: %swiftc_driver -driver-print-jobs -profile-generate -target x86_64-unknown-linux-gnu %s | %FileCheck -check-prefix=CHECK -check-prefix=LINUX %s

// CHECK: swift
// CHECK: -profile-generate

// OSX: bin/ld{{"? }}
// OSX: lib/swift/clang/lib/darwin/libclang_rt.profile_osx.a

// IOS: bin/ld{{"? }}
// IOS: lib/swift/clang/lib/darwin/libclang_rt.profile_ios.a

// IOSSIM: bin/ld{{"? }}
// IOSSIM: lib/swift/clang/lib/darwin/libclang_rt.profile_iossim.a

// tvOS: bin/ld{{"? }}
// tvOS: lib/swift/clang/lib/darwin/libclang_rt.profile_tvos.a

// tvOS_SIM: bin/ld{{"? }}
// tvOS_SIM: lib/swift/clang/lib/darwin/libclang_rt.profile_tvossim.a

// watchOS: bin/ld{{"? }}
// watchOS: lib/swift/clang/lib/darwin/libclang_rt.profile_watchos.a

// watchOS_SIM: bin/ld{{"? }}
// watchOS_SIM: lib/swift/clang/lib/darwin/libclang_rt.profile_watchossim.a

// LINUX: clang++{{"? }}
// LINUX: lib/swift/clang/lib/linux/libclang_rt.profile-x86_64.a
// LINUX: -u__llvm_profile_runtime
