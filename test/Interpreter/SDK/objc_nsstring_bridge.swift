// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

// Add a method to String to make sure a value is really a String and not
// implicitly converted from an NSString, since method lookup doesn't see through
// implicit conversions.
extension String {
  func reallyAString() -> String { return self }
}

func printString(x: String) {
  println(x)
}

func printDescription(o: AnyObject) {
  // FIXME: This is picking up the class method.
#if os(OSX)
  println(o.description!.reallyAString())
#else
  println(o.description()!.reallyAString())
#endif
}

class Pootie : NSObject {
  override var description : String {
    return "cole me down on the panny sty"
  }

  func sinePittyOnRunnyKine(x: String) -> String {
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
printDescription(p.sinePittyOnRunnyKine(s2)) // CHECK: kappa-chow. sa-da-tay

var s3:String = s2.stringByAppendingPathComponent(s1).reallyAString()
printDescription(s3) // CHECK: kappa-chow/wa-da-ta

// Unicode conversion
var s4 : String = NSString(string: "\uf8ff\ufffd")
printDescription(s4) // CHECK: �

// NSCFConstantString conversion
var s5 : String = NSRangeException
printDescription(s5) // CHECK: NSRangeException

// Check conversions to AnyObject
var s6: NSString = "foo"
var ao: AnyObject = s6.copy()
var s7 = (ao as NSString)!
var s8 = ao as String
