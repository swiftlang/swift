// RUN: %target-typecheck-verify-swift -I %S%{fs-sep}Inputs -target %target-swift-5.8-abi-triple -enable-experimental-feature ForeignReferenceTypeSubclassing

// REQUIRES: swift_feature_ForeignReferenceTypeSubclassing

// A C foreign reference type has no virtual destructor, so it cannot be
// subclassed from Swift.

import ForeignReference

final class SubOfLocalCount: LocalCount {} // expected-error {{cannot inherit from non-open class 'LocalCount' outside of its defining module}} expected-error {{class 'SubOfLocalCount' has no initializers}}

final class SubOfGlobalCount: GlobalCount {} // expected-error {{cannot inherit from non-open class 'GlobalCount' outside of its defining module}} expected-error {{class 'SubOfGlobalCount' has no initializers}}
