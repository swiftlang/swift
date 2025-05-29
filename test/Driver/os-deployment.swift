// RUN: not %swiftc_driver -sdk '""' -target x86_64-apple-macosx10.8 %s 2>&1 | %FileCheck --check-prefix=CHECK-OSX %s
// RUN: not %swiftc_driver -sdk '""' -target x86_64-apple-ios6.0 %s 2>&1 | %FileCheck --check-prefix=CHECK-IOS %s
// RUN: not %swiftc_driver -sdk '""' -target x86_64-apple-tvos8.0 %s 2>&1 | %FileCheck --check-prefix=CHECK-tvOS %s
// RUN: not %swiftc_driver -sdk '""' -target x86_64-apple-watchos1.0 %s 2>&1 | %FileCheck --check-prefix=CHECK-watchOS %s

// RUN: not %swiftc_driver -sdk '""' -target i386-apple-ios11.0 %s -### 2>&1 | %FileCheck --check-prefix=CHECK-IOS-11 %s
// RUN: not %swiftc_driver -sdk '""' -target armv7-apple-ios11.0 %s -### 2>&1 | %FileCheck --check-prefix=CHECK-IOS-11 %s
// RUN: not %swiftc_driver -sdk '""' -target i386-apple-ios12.0 %s -### 2>&1 | %FileCheck --check-prefix=CHECK-IOS-12 %s
// RUN: not %swiftc_driver -sdk '""' -target armv7-apple-ios12.0 %s -### 2>&1 | %FileCheck --check-prefix=CHECK-IOS-12 %s
// RUN: %swiftc_driver -sdk '""' -target i386-apple-ios10.3 %s -### >/dev/null
// RUN: %swiftc_driver -sdk '""' -target armv7-apple-ios10.3 %s -### >/dev/null
// RUN: %swiftc_driver -sdk '""' -target x86_64-apple-ios11.0 %s -### >/dev/null
// RUN: %swiftc_driver -sdk '""' -target arm64-apple-ios11.0 %s -### >/dev/null


// CHECK-OSX: Swift requires a minimum deployment target of OS X 10.9
// CHECK-IOS: Swift requires a minimum deployment target of iOS 7
// CHECK-tvOS: Swift requires a minimum deployment target of tvOS 9.0
// CHECK-watchOS: Swift requires a minimum deployment target of watchOS 2.0

// CHECK-IOS-11: iOS 11 does not support 32-bit programs
// CHECK-IOS-12: iOS 12 does not support 32-bit programs
