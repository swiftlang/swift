// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ImportUnsafeCxxMethodsAsAlwaysUnsafe

// REQUIRES: swift_feature_ImportUnsafeCxxMethodsAsAlwaysUnsafe

// This fails only for 32-bit Android for some reason.
// XFAIL: OS=linux-androideabi

// The C++ standard library overlay still spells '__beginUnsafe()',
// '__insertUnsafe(_:)' and friends in its protocol requirements, so the
// synthesized conformances and the safe wrappers built on top of them must keep
// working with the feature enabled.
//
// 'std::set' and 'std::map' are covered by
// always-unsafe-overlay-conformances-set-map.swift instead, since their
// conformances are only synthesized on some platforms.

import StdVector
import StdOptional
import CxxStdlib

func useVector(_ v: inout Vector) {
  // Requires CxxRandomAccessCollection, i.e. a witness named '__beginUnsafe()'.
  for x in v {
    _ = x
  }
  _ = v.count
  _ = v[0]
  _ = v.map { $0 + 1 }

  // Requires CxxMutableRandomAccessCollection, whose synthesis looks up
  // '__beginMutatingUnsafe()' by name through ClangRecordMemberLookup.
  v[0] = 1
  v.sort()
}

func useOptional(_ o: StdOptionalInt) {
  // 'value' keeps the rename, so CxxOptional.value is not shadowed.
  _ = o.value
  _ = o.hasValue
  _ = Int32?(fromCxx: o)
}

func useString(_ s: inout std.string) {
  // 'append' keeps the rename, so the overlay's append(_:) is not ambiguated.
  s.append(s)
  _ = s + s
  _ = String(s)
}
