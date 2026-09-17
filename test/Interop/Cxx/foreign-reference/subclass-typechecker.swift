// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default -enable-experimental-feature ForeignReferenceTypeSubclassing -I %S%{fs-sep}Inputs %s -target %target-swift-5.8-abi-triple

// REQUIRES: swift_feature_ForeignReferenceTypeSubclassing

import InheritFRTSubclassing

final class SwiftSub: SubclassableShared {}

final class SwiftSubOfDerived: DerivedSubclassableShared {}

class NotFinal: SubclassableShared {} // expected-error {{class 'NotFinal' must be 'final' because it subclasses a C++ foreign reference type}}

open class Opened: SubclassableShared {} // expected-error {{class 'Opened' must be 'final' because it subclasses a C++ foreign reference type}}

final class TransitiveSub: SwiftSub {} // expected-error {{inheritance from a final class 'SwiftSub'}}

final class SubOfNonVirtual: NonVirtualShared {} // expected-error {{cannot inherit from non-open class 'NonVirtualShared' outside of its defining module}}
// TODO: emit a note explaining that the C++ type needs a virtual destructor

final class SubOfFinal: FinalShared {} // expected-error {{inheritance from a final class 'FinalShared'}}

final class SubOfPrivateDtor: PrivateDtorShared {} // expected-error {{cannot inherit from non-open class 'PrivateDtorShared' outside of its defining module}}
final class SubOfDeletedDtor: DeletedDtorShared {} // expected-error {{cannot inherit from non-open class 'DeletedDtorShared' outside of its defining module}}
final class SubOfProtectedDtor: ProtectedDtorShared {}

final class WithMembers: SubclassableShared {
  var computed: Int { 0 }
  func method() {}

  final var okComputed: Int { 0 }
  final func okMethod() {}
}
