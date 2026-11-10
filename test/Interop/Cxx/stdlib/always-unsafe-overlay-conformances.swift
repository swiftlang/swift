// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default


// This fails only for 32-bit Android for some reason.
// XFAIL: OS=linux-androideabi

// The C++ standard library overlay's protocol requirements name methods such as
// 'begin()', which are '@unsafe(always)' and disfavored, or '__insertUnsafe(_:)'.
// The synthesized conformances and the safe wrappers built on top of them must
// keep working.
//
// 'std::set' and 'std::map' are covered by
// always-unsafe-overlay-conformances-set-map.swift instead, since their
// conformances are only synthesized on some platforms.

import StdVector
import StdOptional
import CxxStdlib

func useVector(_ v: inout Vector) {
  // Requires CxxRandomAccessCollection, i.e. a witness named 'begin()'.
  for x in v {
    _ = x
  }
  _ = v.count
  _ = v[0]
  _ = v.map { $0 + 1 }

  // Requires CxxMutableRandomAccessCollection, whose synthesis looks up
  // 'beginMutating()' by name through ClangRecordMemberLookup.
  v[0] = 1
  v.sort()
}

func useOptional(_ o: StdOptionalInt) {
  // 'value' keeps its name as a disfavored '@unsafe(always)' overload, so
  // CxxOptional.value is still what this resolves to.
  _ = o.value
  _ = o.hasValue
  _ = Int32?(fromCxx: o)
}

func useString(_ s: inout std.string) {
  // Likewise 'append': the overlay's append(_:) wins over the disfavored
  // unsafe import rather than being ambiguated by it.
  s.append(s)
  _ = s + s
  _ = String(s)
}
