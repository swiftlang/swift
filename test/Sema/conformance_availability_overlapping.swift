// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/conformance_availability_overlapping_other.swift -emit-module-path %t/conformance_availability_overlapping_other.swiftmodule
// RUN: %target-typecheck-verify-swift -I %t

// REQUIRES: OS=macosx

import conformance_availability_overlapping_other

extension HasUnavailableConformance : @retroactive P {}

extension HasConditionallyAvailableConformance : @retroactive P {}

extension HasAlwaysAvailableConformance : @retroactive P {}
// expected-warning@-1 {{conformance of 'HasAlwaysAvailableConformance' to protocol 'P' was already stated in the type's module 'conformance_availability_overlapping_other'}}

struct G<T : P> {}

// None of these should produce a warning about an unavailable conformance.
func usesConformance(_: G<HasUnavailableConformance>) {}
func usesConformance(_: G<HasConditionallyAvailableConformance>) {}
func usesConformance(_: G<HasAlwaysAvailableConformance>) {}