// RUN: %target-run-simple-swift 2>&1 | FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK: 1 is the loneliest number that you'll ever do
NSLog(
  "%@ is the loneliest number that you'll ever %@", 
  NSNumber(integer: 1), "do"
)
