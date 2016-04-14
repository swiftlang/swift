// RUN: %target-run-simple-swift 2>&1 | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

// CHECK: 1 is the loneliest number that you'll ever do
NSLog(
  "%@ is the loneliest number that you'll ever %@", 
  NSNumber(value: 1), "do"
)
