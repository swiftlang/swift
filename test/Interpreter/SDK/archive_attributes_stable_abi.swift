// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/archive_attributes.swift -module-name=test -DENCODE -o %t/encode
// RUN: %target-run %t/encode %t/test.arc
// RUN: plutil -p %t/test.arc | %FileCheck -check-prefix=CHECK-ARCHIVE %S/archive_attributes.swift

// RUN: %target-build-swift %S/archive_attributes.swift -module-name=test -o %t/decode -target %target-stable-abi-triple -link-objc-runtime
// RUN: %target-run %t/decode %t/test.arc NEW --stdlib-unittest-in-process

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: swift_stable_abi
// REQUIRES: CPU=i386 || CPU=x86_64
