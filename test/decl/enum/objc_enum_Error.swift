// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

import Foundation

func acceptBridgeableNSError<E : _ObjectiveCBridgeableError>(_ e: E) { }

@objc enum E2 : Int, Error {
  case A = 1
}

acceptBridgeableNSError(E2.A)


@objc enum E3 : Int {
  case A = 1
}

acceptBridgeableNSError(E3.A)
// expected-error@-1{{argument type 'E3' does not conform to expected type '_ObjectiveCBridgeableError'}}
