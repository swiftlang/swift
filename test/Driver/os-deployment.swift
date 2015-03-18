// RUN: not %swiftc_driver -target x86_64-apple-macosx10.8 %s 2>&1 | FileCheck --check-prefix=CHECK-OSX %s
// RUN: not %swiftc_driver -target x86_64-apple-ios6.0 %s 2>&1 | FileCheck --check-prefix=CHECK-IOS %s
// RUN: not %swiftc_driver -target x86_64-apple-tvos8.0 %s 2>&1 | FileCheck --check-prefix=CHECK-tvOS %s
// RUN: not %swiftc_driver -target x86_64-apple-watchos1.0 %s 2>&1 | FileCheck --check-prefix=CHECK-watchOS %s


// CHECK-OSX: Swift requires a minimum deployment target of OS X 10.9
// CHECK-IOS: Swift requires a minimum deployment target of iOS 7
// CHECK-tvOS: Swift requires a minimum deployment target of tvOS 9.0
// CHECK-watchOS: Swift requires a minimum deployment target of watchOS 2.0
