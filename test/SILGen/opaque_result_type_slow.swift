// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/opaque_result_type_slow_other.swift -emit-module -emit-module-path %t/opaque_result_type_slow_other.swiftmodule -enable-library-evolution
// RUN: %target-swift-emit-silgen %s -I %t

import opaque_result_type_slow_other

struct G<T: P & Q>: P & Q {}

public func myWrap<T: P & Q>(_ t: T) -> some P & Q {
  return G<T>()
}

public func wrapWrap<T: P & Q>(_ t: T) -> some P & Q {
  return wrap(wrap(myWrap(t)))
}

public struct S: P, Q {}

// We generate a series of substitution maps where each one contains an opaque archetype,
// and an abstract conformance of this opaque archetype to a protocol. Each opaque archetype
// then has a substitution map of the same form, and so on.
//
// Transforming each substitution map then visits each opaque archetype twice: once as part
// of the replacement type, and again as part of the conformance.
//
// What saves us is that we would skip the conformance substitution in some cases, and perform
// a lookup instead. This lookup is now load bearing, because changing it into a substitution
// makes this program intractably slow.
//
// The correct fix is to probably cache substituted opaque GenericEnvironments, or
// substituted SubstitutionMaps, inside of the InFlightSubstitution.
let x = wrapWrap(wrapWrap(wrapWrap(wrapWrap(wrapWrap(wrapWrap(wrapWrap(wrapWrap(wrapWrap(S())))))))))
