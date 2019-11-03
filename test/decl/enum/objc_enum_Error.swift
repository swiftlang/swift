// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

import Foundation

func acceptBridgeableNSError<E : _ObjectiveCBridgeableError>(_ e: E) { } // expected-note {{where 'E' = 'E3'}}

@objc enum E2 : Int, Error {
  case A = 1
}

acceptBridgeableNSError(E2.A)


@objc enum E3 : Int {
  case A = 1
}

acceptBridgeableNSError(E3.A)
// expected-error@-1{{global function 'acceptBridgeableNSError' requires that 'E3' conform to '_ObjectiveCBridgeableError'}}
