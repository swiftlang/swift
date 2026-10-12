// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature MoveOnlyClasses

// REQUIRES: swift_feature_MoveOnlyClasses

class KlassModern: ~Copyable {}

class Konditional<T: ~Copyable> {}

func checks<T: ~Copyable, C>(
          _ b: KlassModern, // expected-error {{parameter of noncopyable type 'KlassModern' must specify ownership}} // expected-note 3{{add}}
          _ c: Konditional<T>,
          _ d: Konditional<C>) {}

// Make sure that MoveOnlyClasses don't suppress more than ~Copyable.
do {
  class NiceTry: ~Copyable, Copyable {} // expected-error {{class 'NiceTry' required to be 'Copyable' but is marked with '~Copyable'}}

  class KlassNonescapable: ~Escapable {} // expected-error {{classes cannot be '~Escapable'}}
  class KlassNonescapableButEscapable: ~Escapable, Escapable {} // expected-error {{classes cannot be '~Escapable'}}

  func requiresEscapable<T>(_: T.Type) {}
  func checkEscapable() {
    requiresEscapable(KlassNonescapable.self)
  }
}

// A subclass inherits its superclass's conformance to Copyable, including any
// conditions on it.
struct NoncopyableStruct: ~Copyable {}

class ConditionallyCopyable<T: ~Copyable>: ~Copyable {}
extension ConditionallyCopyable: Copyable where T: Copyable {}

class SubOfCopyableInstance: ConditionallyCopyable<Int> {}
class SubOfNoncopyableInstance: ConditionallyCopyable<NoncopyableStruct> {}
// expected-note@-1 {{requirement from conditional conformance of 'SubOfNoncopyableInstance' to 'Copyable'}}
class SubSubOfNoncopyableInstance: SubOfNoncopyableInstance {}
// expected-note@-1 {{requirement from conditional conformance of 'SubSubOfNoncopyableInstance' to 'Copyable'}}
class GenericSub<U: ~Copyable>: ConditionallyCopyable<U> {}
// expected-note@-1 {{requirement from conditional conformance of 'GenericSub<NoncopyableStruct>' to 'Copyable'}}

func requiresCopyable<T>(_: T.Type) {}

func checkInheritedConditionalCopyable() {
  requiresCopyable(SubOfCopyableInstance.self)
  requiresCopyable(SubOfNoncopyableInstance.self) // expected-error {{global function 'requiresCopyable' requires that 'NoncopyableStruct' conform to 'Copyable'}}
  requiresCopyable(SubSubOfNoncopyableInstance.self) // expected-error {{global function 'requiresCopyable' requires that 'NoncopyableStruct' conform to 'Copyable'}}
  requiresCopyable(GenericSub<Int>.self)
  requiresCopyable(GenericSub<NoncopyableStruct>.self) // expected-error {{global function 'requiresCopyable' requires that 'NoncopyableStruct' conform to 'Copyable'}}
}
