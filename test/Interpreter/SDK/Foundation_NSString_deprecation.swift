// RUN: %target-swift-frontend -parse -verify %s
// REQUIRES: objc_interop

import Foundation

let hello = "Hello, world!"

let hello2 = hello
  .stringByAddingPercentEscapesUsingEncoding(NSUTF8StringEncoding) // expected-warning{{'stringByAddingPercentEscapesUsingEncoding' is deprecated}}

let hello3 = hello2?
  .stringByReplacingPercentEscapesUsingEncoding(NSUTF8StringEncoding) // expected-warning{{'stringByReplacingPercentEscapesUsingEncoding' is deprecated}}
