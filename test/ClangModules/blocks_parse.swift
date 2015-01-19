// RUN: %target-swift-frontend %clang-importer-sdk -parse -verify %s

import blocks
import Foundation

var someNSString : NSString
func useString(s: String) {}

dispatch_async(dispatch_get_current_queue()) { }
someNSString.enumerateLinesUsingBlock {(s:String?) in }
someNSString.enumerateLinesUsingBlock {s in }
someNSString.enumerateLinesUsingBlock({ useString($0) })

dispatch_async(dispatch_get_current_queue(), /*not a block=*/()) // expected-error{{cannot invoke 'dispatch_async' with an argument list of type '(dispatch_queue_t, ())'}} expected-note {{expected an argument list of type '(dispatch_queue_t, dispatch_block_t!)'}}

