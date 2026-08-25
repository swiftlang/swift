// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ImportUnsafeCxxMethodsAsAlwaysUnsafe

// REQUIRES: swift_feature_ImportUnsafeCxxMethodsAsAlwaysUnsafe

// The CxxSet and CxxDictionary conformances are only synthesized where the
// standard library declares the members they are derived from -- MSVC's STL,
// for one, does not spell 'insert(const value_type&)' -- so this matches the
// platforms use-std-set.swift and use-std-map.swift run on. The rest of the
// overlay is covered by always-unsafe-overlay-conformances.swift.
// REQUIRES: OS=macosx || OS=linux-gnu

import StdSet
import StdMap
import CxxStdlib

func useSet(_ s: inout SetOfCInt) {
  // 'insert' keeps the rename, so CxxUniqueSet.insert(_:) is not shadowed.
  s.insert(1)
  _ = s.contains(1)
  _ = s.remove(1)
  _ = Array(s)
}

func useMap(_ m: inout Map) {
  m[1] = 2
  _ = m[1]
  _ = m.removeValue(forKey: 1)
  _ = m.mapValues { $0 + 1 }
}
