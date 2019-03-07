// RUN: %empty-directory(%t)

// RUN: echo 'void testNSLog();' > %t/NSLogIsWorking.h
// RUN: echo '@import Foundation; void testNSLog() { NSLog(@"working"); }' | %target-clang -fobjc-arc -fmodules -x objective-c - -c -o %t/NSLogIsWorking.o

// RUN: %target-build-swift %s %t/NSLogIsWorking.o -import-objc-header %t/NSLogIsWorking.h -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main 2>%t/output.txt
// RUN: %FileCheck %s < %t/output.txt

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

// CHECK: working
testNSLog()

// CHECK: 1 is the loneliest number that you'll ever do
NSLog(
  "%@ is the loneliest number that you'll ever %@", 
  NSNumber(value: 1), "do"
)
