// RUN: not %swift -target x86_64-apple-macosx10.8 %s 2>&1 | FileCheck --check-prefix=CHECK-OSX %s
// RUN: not %swift -target x86_64-apple-ios6.0 %s 2>&1 | FileCheck --check-prefix=CHECK-IOS %s


// CHECK-OSX: Swift requires a minimum deployment target of OSX 10.9
// CHECK-IOS: Swift requires a minimum deployment target of iOS 7
