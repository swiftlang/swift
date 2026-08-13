// RUN: rm -rf %t
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}direct.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}direct.h

// RUN: %target-swift-frontend -typecheck -verify %t%{fs-sep}apinotes.swift \
// RUN:   -I %t%{fs-sep}Inputs \
// RUN:   -Xcc -iapinotes-modules -Xcc %t%{fs-sep}Inputs \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}apinotes.h

//--- Inputs/module.modulemap
module Direct {
    header "direct.h"
    requires cplusplus
}
module APINotes {
    header "apinotes.h"
    requires cplusplus
}

//--- Inputs/direct.h
#include "swift/bridging"

// expected-error@+1 {{'ImmortalNonEscapable' cannot be both a foreign reference type and non-escapable}}
struct SWIFT_IMMORTAL_REFERENCE SWIFT_NONESCAPABLE ImmortalNonEscapable {
  int value;
};

// A foreign reference type without accessible constructors is diagnosed too.
// expected-error@+1 {{'ImmortalNonEscapableNoCtor' cannot be both a foreign reference type and non-escapable}}
struct SWIFT_IMMORTAL_REFERENCE SWIFT_NONESCAPABLE ImmortalNonEscapableNoCtor {
  int value;

private:
  ImmortalNonEscapableNoCtor();
  ~ImmortalNonEscapableNoCtor();
};

struct SharedNonEscapable;
void retainShared(SharedNonEscapable *);
void releaseShared(SharedNonEscapable *);

// expected-error@+2 {{'SharedNonEscapable' cannot be both a foreign reference type and non-escapable}}
struct SWIFT_SHARED_REFERENCE(retainShared, releaseShared) SWIFT_NONESCAPABLE
    SharedNonEscapable {
  int value;
};

// Non-escapability inherited from a non-escapable foreign reference base.
// expected-note@+2 {{escapable record 'DerivedFromNonEscapable' cannot have non-escapable base 'ImmortalNonEscapable'}}
struct SWIFT_IMMORTAL_REFERENCE DerivedFromNonEscapable
    : ImmortalNonEscapable {};

// Non-escapability inferred from a non-escapable field.
struct SWIFT_NONESCAPABLE View {
  int *pointer;
};

void retainWithField(class FRTWithNonEscapableField *);
void releaseWithField(class FRTWithNonEscapableField *);

// expected-note@+1 {{escapable record 'FRTWithNonEscapableField' cannot have non-escapable field 'view'}}
class FRTWithNonEscapableField {
public:
  View view;
} SWIFT_SHARED_REFERENCE(retainWithField, releaseWithField);

// expected-note@+1 {{escapable record 'ComplexFRTWithNonEscapableField' cannot have non-escapable field 'view'}}
struct SWIFT_IMMORTAL_REFERENCE ComplexFRTWithNonEscapableField {
  View view;
  ~ComplexFRTWithNonEscapableField();
  ComplexFRTWithNonEscapableField(const ComplexFRTWithNonEscapableField &);
};

// A templated foreign reference type that stores a non-escapable template
// argument is rejected, whether or not it spells SWIFT_ESCAPABLE_IF. A
// non-escapable argument that is not stored is fine, since nothing that cannot
// escape ends up inside the class.
// expected-warning@+3 2 {{SWIFT_ESCAPABLE_IF has no effect on}}
// expected-note@+2 {{escapable record 'StoresArgument<View>' cannot have non-escapable field 'value'}}
template <class T> struct SWIFT_IMMORTAL_REFERENCE SWIFT_ESCAPABLE_IF(T)
    StoresArgument {
  T value;
};
using StoresNonEscapable = StoresArgument<View>;
using StoresEscapable = StoresArgument<int>;

// expected-note@+1 {{escapable record 'UnannotatedTemplate<View>' cannot have non-escapable field 'value'}}
template <class T> struct SWIFT_IMMORTAL_REFERENCE UnannotatedTemplate {
  T value;
};
using UnannotatedNonEscapable = UnannotatedTemplate<View>;

template <class T> struct SWIFT_IMMORTAL_REFERENCE DoesNotStoreArgument {
  int value;
};
using DoesNotStoreNonEscapable = DoesNotStoreArgument<View>;

// A foreign reference type with no escapability annotation is imported as usual.
struct SWIFT_IMMORTAL_REFERENCE Immortal {
  int value;
};

// Escapability annotations are redundant on a foreign reference type, whether
// it is annotated directly or inherits its reference-ness from a base.
// expected-warning@+1 {{SWIFT_ESCAPABLE has no effect on 'ImmortalEscapable'}}
struct SWIFT_IMMORTAL_REFERENCE SWIFT_ESCAPABLE ImmortalEscapable {
  int value;
};

// expected-warning@+1 {{SWIFT_ESCAPABLE has no effect on 'DerivedEscapable'}}
struct SWIFT_ESCAPABLE DerivedEscapable : Immortal {};

// expected-warning@+2 {{SWIFT_ESCAPABLE_IF has no effect on 'ConditionallyEscapable<int>'}}
template <class T> struct SWIFT_IMMORTAL_REFERENCE SWIFT_ESCAPABLE_IF(T)
    ConditionallyEscapable {
  T value;
};
using ConditionallyEscapableInt = ConditionallyEscapable<int>;

struct HoldsNonEscapableFRT {
  // expected-note@+2 {{field 'field' unavailable (cannot import)}}
  // expected-note@+1 {{pointer to non-escapable type 'ImmortalNonEscapable' cannot be imported}}
  ImmortalNonEscapable *field;
};

struct SWIFT_IMMORTAL_REFERENCE ImmortalHoldsNonEscapableFRT {
  // expected-note@+2 {{field 'field' unavailable (cannot import)}}
  // expected-note@+1 {{pointer to non-escapable type 'ImmortalNonEscapable' cannot be imported}}
  ImmortalNonEscapable *field;
};

//--- Inputs/apinotes.h
#include "swift/bridging"

// expected-error@+1 {{'AnnotatedViaAPINotes' cannot be both a foreign reference type and non-escapable}}
struct SWIFT_IMMORTAL_REFERENCE AnnotatedViaAPINotes {
  int value;
};

// An escapability annotation applied via API notes is redundant too.
// expected-warning@+1 {{SWIFT_ESCAPABLE has no effect on 'EscapableViaAPINotes'}}
struct SWIFT_IMMORTAL_REFERENCE EscapableViaAPINotes {
  int value;
};

//--- Inputs/APINotes.apinotes
Name: APINotes
Tags:
- Name: AnnotatedViaAPINotes
  SwiftEscapable: false
- Name: EscapableViaAPINotes
  SwiftEscapable: true

//--- direct.swift
import Direct

// The ill-formed types are not imported at all.
public func useImmortal(_ x: ImmortalNonEscapable) {} // expected-error {{cannot find type 'ImmortalNonEscapable' in scope}}
public func useImmortalNoCtor(_ x: ImmortalNonEscapableNoCtor) {} // expected-error {{cannot find type 'ImmortalNonEscapableNoCtor' in scope}}
public func useShared(_ x: SharedNonEscapable) {} // expected-error {{cannot find type 'SharedNonEscapable' in scope}}
public func useDerived(_ x: DerivedFromNonEscapable) {} // expected-error {{cannot find type 'DerivedFromNonEscapable' in scope}}
public func useFRTWithNEField(_ x: FRTWithNonEscapableField) {} // expected-error {{cannot find type 'FRTWithNonEscapableField' in scope}}
public func useComplexFRTWithNEField(_ x: ComplexFRTWithNonEscapableField) {} // expected-error {{cannot find type 'ComplexFRTWithNonEscapableField' in scope}}
public func useStoresNE(_ x: StoresNonEscapable) {} // expected-error {{cannot find type 'StoresNonEscapable' in scope}}
public func useUnannotatedNE(_ x: UnannotatedNonEscapable) {} // expected-error {{cannot find type 'UnannotatedNonEscapable' in scope}}

// A non-escapable template argument that is never stored is not a problem.
public func useStoresEscapable(_ x: StoresEscapable) -> Int32 { x.value }
public func useDoesNotStoreNE(_ x: DoesNotStoreNonEscapable) -> Int32 { x.value }

public func useEscapable(_ x: Immortal) -> Int32 { x.value }
public func useImmortalEscapable(_ x: ImmortalEscapable) -> Int32 { x.value }
public func useDerivedEscapable(_ x: DerivedEscapable) {}
public func useConditional(_ x: ConditionallyEscapableInt) -> Int32 { x.value }

// Records with a non-escapable foreign reference field are imported without
// that field.
public func useHolder(_ x: HoldsNonEscapableFRT) {
  _ = x.field // expected-error {{value of type 'HoldsNonEscapableFRT' has no member 'field'}}
}

public func useImmortalHolder(_ x: ImmortalHoldsNonEscapableFRT) {
  _ = x.field // expected-error {{value of type 'ImmortalHoldsNonEscapableFRT' has no member 'field'}}
}

//--- apinotes.swift
import APINotes

public func useAnnotated(_ x: AnnotatedViaAPINotes) {} // expected-error {{cannot find type 'AnnotatedViaAPINotes' in scope}}
public func useEscapableViaAPINotes(_ x: EscapableViaAPINotes) -> Int32 { x.value }
