// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -nsstring-is-string -i -module-cache-path=%t/clang-module-cache %s | FileCheck %s

import Foundation

// Add a method to String to make sure a value is really a String and not
// implicitly converted from an NSString, since method lookup doesn't see through
// implicit conversions.
extension String {
  func reallyAString() -> String { return this }
}

func printString(x:String) {
  println(x)
}

func printDescription(o:NSObject) {
  println(o.description().reallyAString())
}

class Pootie : NSObject {
  func description() -> String {
    return "cole me down on the panny sty"
  }

  func sinePittyOnRunnyKine(x:String) -> String {
    return "\(x). sa-da-tay"
  }
}

var a:NSNumber = 2001
printDescription(a) // CHECK: 2001

var p = Pootie()
printDescription(p) // CHECK: cole me down on the panny sty

var s1:String = "wa-da-ta"
// We don't say 'var s2:NSString = "..."' in order to keep this test independent of the
// ABI of NSString.convertFromStringLiteral.
var s2s:String = "kappa-chow"
var s2:NSString = s2s
printDescription(s2) // CHECK: kappa-chow
printDescription(p.performSelector("sinePittyOnRunnyKine:", withObject:s2)) // CHECK: kappa-chow. sa-da-tay

var s3:String = s2.stringByAppendingPathComponent(s1).reallyAString()
printDescription(s3) // CHECK: kappa-chow/wa-da-ta
