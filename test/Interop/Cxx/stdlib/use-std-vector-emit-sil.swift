// RUN: %target-swift-frontend %s -emit-sil -cxx-interoperability-mode=default -I %S/Inputs -Xcc -fignore-exceptions -verify

// FIXME: The synthesized conformance of std::vector to CxxVector on android-armv7 is invalid
// https://github.com/swiftlang/swift/issues/93524
// XFAIL: OS=linux-androideabi

import StdVector
import CxxStdlib

@available(SwiftStdlib 6.4, *)
func pushToVectorDuringIter() {
  var vec = Vector([1, 2, 3])
  for el in vec { // expected-note {{conflicting access is here}}
    vec.push_back(el) // expected-error {{overlapping accesses to 'vec', but modification requires exclusive access}}
  }

  var vecNC = makeVectorOfNonCopyable()
  for el in vecNC { // expected-note {{conflicting access is here}}
    vecNC.push_back(el) // expected-error {{overlapping accesses to 'vecNC', but modification requires exclusive access}}
  }
}
