// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify %s

// REQUIRES: objc_interop

import blocks
import Foundation

var someNSString : NSString
func useString(s: String) {}

dispatch_async(dispatch_get_current_queue()) { }
someNSString.enumerateLinesUsingBlock {(s:String?) in }
someNSString.enumerateLinesUsingBlock {s in }
someNSString.enumerateLinesUsingBlock({ useString($0) })

dispatch_async(dispatch_get_current_queue(), /*not a block=*/()) // expected-error{{cannot convert value of type '()' to expected argument type 'dispatch_block_t' (aka '() -> ()')}}

func testNoEscape(@noescape f: @convention(block) () -> Void, nsStr: NSString,
                  @noescape fStr: (String!) -> Void) {
  dispatch_async(dispatch_get_current_queue(), f) // expected-error{{invalid conversion from non-escaping function of type '@noescape @convention(block) () -> Void' to potentially escaping function type 'dispatch_block_t' (aka '() -> ()')}}
  dispatch_sync(dispatch_get_current_queue(), f) // okay: dispatch_sync is noescape

  // rdar://problem/19818617
  nsStr.enumerateLinesUsingBlock(fStr) // okay due to @noescape

  _ = nsStr.enumerateLinesUsingBlock as Int // expected-error{{'(@noescape (String!) -> Void) -> Void' is not convertible}}
}
