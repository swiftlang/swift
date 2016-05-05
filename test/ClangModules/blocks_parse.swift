// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify %s

// REQUIRES: objc_interop

import blocks
import Foundation

var someNSString : NSString
func useString(_ s: String) {}

dispatch_async(dispatch_get_current_queue()) { }
someNSString.enumerateLines {(s:String?) in }
someNSString.enumerateLines {s in }
someNSString.enumerateLines({ useString($0) })

dispatch_async(dispatch_get_current_queue(), /*not a block=*/()) // expected-error{{cannot convert value of type '()' to expected argument type 'dispatch_block_t' (aka '@convention(block) () -> ()')}}

func testNoEscape(f: @noescape @convention(block) () -> Void, nsStr: NSString,
                  fStr: @noescape (String!) -> Void) {
  dispatch_async(dispatch_get_current_queue(), f) // expected-error{{invalid conversion from non-escaping function of type '@noescape @convention(block) () -> Void' to potentially escaping function type 'dispatch_block_t' (aka '@convention(block) () -> ()')}}
  dispatch_sync(dispatch_get_current_queue(), f) // okay: dispatch_sync is noescape

  // rdar://problem/19818617
  nsStr.enumerateLines(fStr) // okay due to @noescape

  _ = nsStr.enumerateLines as Int // expected-error{{cannot convert value of type '(@noescape (String) -> Void) -> Void' to type 'Int' in coercion}}
}

func checkTypeImpl<T>(_ a: inout T, _: T.Type) {}
do {
  var blockOpt = blockWithoutNullability()
  checkTypeImpl(&blockOpt, Optional<dispatch_block_t>.self)
  var block: dispatch_block_t = blockWithoutNullability()
}
do {
  var block = blockWithNonnull()
  checkTypeImpl(&block, dispatch_block_t.self)
}
do {
  var blockOpt = blockWithNullUnspecified()
  checkTypeImpl(&blockOpt, Optional<dispatch_block_t>.self)
  var block: dispatch_block_t = blockWithNullUnspecified()
}
do {
  var block = blockWithNullable()
  checkTypeImpl(&block, Optional<dispatch_block_t>.self)
}
