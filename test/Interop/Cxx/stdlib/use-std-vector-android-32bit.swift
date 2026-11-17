// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default -I %S/Inputs -Xcc -fignore-exceptions

// REQUIRES: OS=linux-androideabi

// On Android armv7, std::vector's iterator does not conform to
// UnsafeCxxRandomAccessIterator, because its operator- returns CInt while its
// operator+= takes Int. 
// Make sure std::vector does not get an invalid CxxVector
// conformance, which used to crash SILGen.

import StdVector
import CxxStdlib

func requiresCxxVector<V: CxxVector>(_ v: V) {} // expected-note {{where 'V' = 'Vector'}}

func test() {
  var v = Vector()
  v.push_back(1)

  // FIXME: std::vector should conform to CxxVector on 32-bit targets
  // https://github.com/swiftlang/swift/issues/93524
  requiresCxxVector(v) // expected-error {{conform to 'CxxVector'}}
}
