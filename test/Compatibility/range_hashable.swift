// RUN: %target-typecheck-verify-swift -swift-version 3

// REQUIRES: objc_interop

import Foundation

extension NSRange : Hashable { // expected-warning{{conformance of '_NSRange' to protocol 'Hashable' was already stated in the type's module 'Foundation'}}
  var hashValue: Int { return 0 } // expected-note{{var 'hashValue' will not be used to satisfy the conformance to 'Hashable'}}
}
