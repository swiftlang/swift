// RUN: %target-swift-frontend %s -emit-sil -cxx-interoperability-mode=default -I %S/Inputs -Xcc -fignore-exceptions -verify

import StdVector
import CxxStdlib

@available(SwiftStdlib 6.4, *)
func pushToVectorDuringIter() {
  var vec = Vector([1, 2, 3])
  for el in vec { // expected-note {{conflicting access is here}}
    vec.push_back(el) // expected-error {{overlapping accesses to 'vec', but modification requires exclusive access; consider copying to a local variable}}
  }
}
