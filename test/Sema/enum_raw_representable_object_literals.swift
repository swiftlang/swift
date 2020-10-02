// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=ios

import UIKit

struct FooLiteral: _ExpressibleByColorLiteral, _ExpressibleByImageLiteral, _ExpressibleByFileReferenceLiteral {
  init(_colorLiteralRed: Float, green: Float, blue: Float, alpha: Float) {}
  init(imageLiteralResourceName: String) {}
  init(fileReferenceLiteralResourceName: String) {}
}

enum Foo: FooLiteral { // expected-error {{raw type 'FooLiteral' is not expressible by a string, integer, or floating-point literal}}
  typealias RawValue = Never
  var rawValue: Never { fatalError() }
  init(rawValue: Never) { fatalError() }
  case bar1 = #colorLiteral(red: 1, green: 0, blue: 0, alpha: 1)
  case bar2 = #imageLiteral(resourceName: "hello.png")
  case bar3 = #fileLiteral(resourceName: "what.txt")
}
