// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

// CHECK: 1 is the loneliest number that you'll ever do
NSLog(
  "%@ is the loneliest number that you'll ever %@", 
  NSNumber(withInteger: 1), "do"
)
