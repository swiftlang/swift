
// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -Xcc -iapinotes-modules -Xcc %swift_src_root/stdlib/public/Cxx/std -Xcc -std=c++20 -I %t%{fs-sep}Inputs  %t%{fs-sep}test.swift -strict-memory-safety -enable-experimental-feature LifetimeDependence -cxx-interoperability-mode=default -diagnostic-style llvm -plugin-path %swift-plugin-dir -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}nonescapable.h 2>&1

// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: std_span

//--- Inputs/module.modulemap
module Test {
    header "nonescapable.h"
    requires cplusplus
}

//--- Inputs/nonescapable.h
#include "swift/bridging"
#include <span>
#include <vector>
#include <tuple>
#include <memory>

struct SWIFT_NONESCAPABLE View {
    __attribute__((swift_attr("@lifetime(immortal)")))
    View() : member(nullptr) {}
    __attribute__((swift_attr("@lifetime(copy p)")))
    View(const int *p [[clang::lifetimebound]]) : member(p) {}
    View(const View&) = default;
private:
    const int *member;
};

struct SWIFT_ESCAPABLE Owner {};

struct Unannotated {
    Unannotated();
    int *pointer;
};

struct SWIFT_UNSAFE_REFERENCE UnsafeReference {};

struct SafeEscapableAggregate {
    int a;
    float b[5];
};

struct UnknownEscapabilityAggregate {
    SafeEscapableAggregate agg;
    Unannotated unann;
};

template <typename T> struct SWIFT_ESCAPABLE_IF(T) EscapableIfT { T t; };
using SafeEscapableIf = EscapableIfT<int>;

struct ConditionalMemberBeforePointer {
    SafeEscapableIf cond;
    int *pointer;
};

struct ConditionalMemberAfterPointer {
    int *pointer;
    SafeEscapableIf cond;
};

struct SharedPtrBeforePointer {
    std::shared_ptr<int> shared;
    int *pointer;
};

struct MyContainer {
    int begin() const { return 0; }
    int end() const { return -1; }
};

using SpanOfInt = std::span<int>;
using SpanOfIntAlias = SpanOfInt;
using VecOfPtr = std::vector<int*>;
using VecOfInt = std::vector<int>;
using SafeTuple = std::tuple<int, int, int>;
using UnsafeTuple = std::tuple<int, int*, int>;

View safeFunc(View v1 [[clang::noescape]], View v2 [[clang::lifetimebound]]);
// Second non-escapable type is not annotated in any way.
void unsafeFunc(View v1 [[clang::noescape]], View v2);

// expected-warning@+1{{the returned type 'View' is annotated as non-escapable; its lifetime dependencies must be annotated}}
View returnsViewNoAnnotation(const Owner &o);

struct InferredNonEscapable {
  View v;
};
InferredNonEscapable returnsInferredNonEscapable(const Owner &o);

struct HasUnannotatedViewGetter {
  // expected-warning@+2{{the returned type 'View' is annotated as non-escapable; its lifetime dependencies must be annotated}}
  // expected-error@+1{{cannot infer lifetime dependence on a method because 'self' is BitwiseCopyable, specify '@_lifetime(borrow self)'}}
  View getView() const;
};

// expected-warning@+2{{the returned type 'ViewWithUnannotatedCtor' is annotated as non-escapable; its lifetime dependencies must be annotated}}
struct SWIFT_NONESCAPABLE ViewWithUnannotatedCtor {
    ViewWithUnannotatedCtor(const Owner &o);
private:
    const int *member;
};

View returnsViewLifetimebound(const Owner &o [[clang::lifetimebound]]);

// A lifetime dependency whose target is Escapable is dropped, so the annotation
// is not enforced: import the function as unsafe.
// expected-warning@+1{{the returned type 'Owner' is annotated as escapable; it cannot have lifetime dependencies}}
Owner returnsOwnerLifetimebound(const View &v [[clang::lifetimebound]]);

__attribute__((swift_attr("@lifetime(borrow o)")))
// expected-warning@+1{{the returned type 'View' is annotated as non-escapable; its lifetime dependencies must be annotated}}
View returnsViewHandWrittenLifetime(const Owner &o);

// expected-expansion@+5:6{{
//   expected-error@1{{cannot borrow the lifetime of 'byValue', which is passed by value on a function}}
// }}
__attribute__((swift_attr("@lifetime(borrow byValue)")))
// expected-warning@+1{{the returned type 'View' is annotated as non-escapable; its lifetime dependencies must be annotated}}
View returnsViewByValueLifetime(Owner byValue);

__attribute__((swift_attr("safe")))
// expected-warning@+1{{the returned type 'View' is annotated as non-escapable; its lifetime dependencies must be annotated}}
View returnsViewAuditedSafe(const Owner &o);

class SharedObject {
public:
  View getView() const [[clang::lifetimebound]];
private:
  int *p;
} SWIFT_SHARED_REFERENCE(retainSharedObject, releaseSharedObject);

View getViewFromSharedObject(SharedObject* p [[clang::lifetimebound]]);

inline void retainSharedObject(SharedObject *) {}
inline void releaseSharedObject(SharedObject *) {}

struct DerivedFromSharedObject : SharedObject {};

// Unsafety of a reference type is inherited by the types derived from it, just
// like for value types.
struct HasUnsafeReferenceBase : UnsafeReference {};
struct HasUnsafeReferenceBaseTransitively : HasUnsafeReferenceBase {};
struct SWIFT_SAFE WrapsUnsafeReferenceBase : UnsafeReference {};
struct DerivedFromWrapsUnsafeReferenceBase : WrapsUnsafeReferenceBase {};

class SWIFT_UNSAFE ExplicitlyUnsafeSharedObject {
} SWIFT_SHARED_REFERENCE(retainExplicitlyUnsafeSharedObject,
                         releaseExplicitlyUnsafeSharedObject);

inline void retainExplicitlyUnsafeSharedObject(ExplicitlyUnsafeSharedObject *) {}
inline void releaseExplicitlyUnsafeSharedObject(ExplicitlyUnsafeSharedObject *) {}

struct HasExplicitlyUnsafeSharedObjectBase : ExplicitlyUnsafeSharedObject {};

// The base class may be a class template specialization, as in CRTP.
template <class Derived>
struct SWIFT_UNSAFE_REFERENCE CRTPBase {};

struct HasCRTPBase : CRTPBase<HasCRTPBase> {};
struct SWIFT_SAFE WrapsCRTPBase : CRTPBase<WrapsCRTPBase> {};

template <class T> struct DerivedFromParam : T {};
using HasUnsafeReferenceParamBase = DerivedFromParam<UnsafeReference>;

template <class T> struct MiddleTemplate : CRTPBase<MiddleTemplate<T>> {};
template <class T> struct BottomTemplate : MiddleTemplate<T> {};
using HasCRTPBaseTransitively = BottomTemplate<int>;

struct OwnedData {
  SpanOfInt getView() const [[clang::lifetimebound]];
  void takeSharedObject(SharedObject *) const;
};

// A class template that throws away its type argument.
//
// If this template is instantiated with an unsafe type, it should be considered
// unsafe even if that type is never used.
template <typename> struct TTake {};

using TTakeInt = TTake<int>;
using TTakePtr = TTake<int *>;
using TTakeSafeTuple = TTake<SafeTuple>;
using TTakeUnsafeTuple = TTake<UnsafeTuple>;

struct HoldsShared {
  SharedObject* obj;

  SharedObject* getObj() const SWIFT_RETURNS_INDEPENDENT_VALUE
                               SWIFT_RETURNS_UNRETAINED;
};

template <typename F, typename S> struct SWIFT_ESCAPABLE_IF(F, S) TTake2 {};
template <typename T> struct PassThru {
  T field;
};
struct IsUnsafe { int *p; };
struct HasUnsafe : TTake2<PassThru<HasUnsafe>, IsUnsafe> {};
using AlsoUnsafe = PassThru<HasUnsafe>;

struct SWIFT_UNSAFE ExplicitlyUnsafeStruct {};
struct HasUnsafeMember {
  HasUnsafeMember();
  ExplicitlyUnsafeStruct member;
};

struct HasUnsafeBase : ExplicitlyUnsafeStruct {
  HasUnsafeBase();
};

struct SWIFT_SAFE WrapsUnsafeMember {
  WrapsUnsafeMember();
  ExplicitlyUnsafeStruct member;
};

struct SWIFT_SAFE WrapsUnsafeBase : ExplicitlyUnsafeStruct {
  WrapsUnsafeBase();
};

struct SWIFT_SAFE WrapsUnannotatedMember {
  WrapsUnannotatedMember();
  Unannotated member;
};

//--- test.swift

import Test
import CxxStdlib
#if canImport(CoreFoundation)
import CoreFoundation
#endif

func useUnsafeParam(x: Unannotated) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
 _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

@available(SwiftStdlib 5.8, *)
func useUnsafeParam2(x: UnsafeReference) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func useUnsafeParam3(x: UnknownEscapabilityAggregate) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func useConditionalMemberBeforePointer(x: ConditionalMemberBeforePointer) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func useConditionalMemberAfterPointer(x: ConditionalMemberAfterPointer) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func useSharedPtrBeforePointer(x: SharedPtrBeforePointer) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func useSafeEscapableIf(x: SafeEscapableIf) {
  _ = x
}

func useSafeParams(x: Owner, y: View, z: SafeEscapableAggregate, c: MyContainer) {
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
    let _ = c.__beginUnsafe() // expected-note{{reference to unsafe instance method '__beginUnsafe()'}}
}

#if canImport(CoreFoundation)
func useCfType(x: CFArray) {
  _ = x
}
#endif

func useString(x: std.string) {
  _ = x
}

func useVecOfPtr(x: VecOfPtr) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func useVecOfInt(x: VecOfInt) {
  _ = x
}

func useSafeTuple(x: SafeTuple) {
  _ = x
}

func useUnsafeTuple(x: UnsafeTuple) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func useCppSpan(x: SpanOfInt) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
  _ = x.size()
}

func useCppSpan2(x: SpanOfIntAlias) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func useCppSpan3() -> SpanOfInt {
  let x = OwnedData()
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  return x.getView() // expected-note {{reference to instance method 'getView()' involves unsafe type 'SpanOfInt'}}
}

func useSafeLifetimeAnnotated(v: View) {
    let _ = safeFunc(v, v)
}

func useUnsafeLifetimeAnnotated(v: View) {
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
    unsafeFunc(v, v) // expected-note{{reference to unsafe global function 'unsafeFunc'}}
}

func useInferredLifetimeDependency(o: Owner, g: HasUnannotatedViewGetter) {
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
    let _ = returnsViewNoAnnotation(o) // expected-note{{reference to unsafe global function 'returnsViewNoAnnotation'}}
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
    let _ = returnsInferredNonEscapable(o) // expected-note{{reference to unsafe global function 'returnsInferredNonEscapable'}}
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
    let _ = g.getView() // expected-note{{reference to unsafe instance method 'getView()'}}
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
    let _ = ViewWithUnannotatedCtor(o) // expected-note{{reference to unsafe initializer 'init(_:)'}}
}

func useDefaultConstructedView() {
    let _ = View()
}

func useAnnotatedLifetimeDependency(o: Owner) {
    let _ = returnsViewLifetimebound(o)
    let _ = returnsViewHandWrittenLifetime(o)
    let _ = returnsViewByValueLifetime(o)
    let _ = returnsViewAuditedSafe(o)
}

func useEscapableLifetimeDependency(v: View) {
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
    let _ = returnsOwnerLifetimebound(v) // expected-note{{reference to unsafe global function 'returnsOwnerLifetimebound'}}
}

@available(SwiftStdlib 5.8, *)
func useSharedReference(frt: SharedObject, x: OwnedData) {
  let _ = frt
  x.takeSharedObject(frt)
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  let _ = frt.getView() // expected-note{{reference to unsafe instance method 'getView()'}}
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  let _ = getViewFromSharedObject(frt) // expected-note{{reference to unsafe global function 'getViewFromSharedObject'}}
}

@available(SwiftStdlib 5.8, *)
func useSharedReference(frt: DerivedFromSharedObject, h: HoldsShared) {
  let _ = frt
  let _ = h.getObj()
}

@available(SwiftStdlib 5.8, *)
func unsafeReferenceBases(a: HasUnsafeReferenceBase,
                          b: HasUnsafeReferenceBaseTransitively,
                          c: WrapsUnsafeReferenceBase,
                          d: DerivedFromWrapsUnsafeReferenceBase,
                          e: ExplicitlyUnsafeSharedObject,
                          f: HasExplicitlyUnsafeSharedObjectBase,
                          g: HasCRTPBase,
                          h: WrapsCRTPBase,
                          i: HasUnsafeReferenceParamBase,
                          j: HasCRTPBaseTransitively) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = a // expected-note{{reference to parameter 'a' involves unsafe type}}
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = b // expected-note{{reference to parameter 'b' involves unsafe type}}
  _ = c
  _ = d
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = e // expected-note{{reference to parameter 'e' involves unsafe type}}
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = f // expected-note{{reference to parameter 'f' involves unsafe type}}
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = g // expected-note{{reference to parameter 'g' involves unsafe type}}
  _ = h
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = i // expected-note{{reference to parameter 'i' involves unsafe type}}
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = j // expected-note{{reference to parameter 'j' involves unsafe type}}
}

func useTTakeInt(x: TTakeInt) {
  _ = x
}

func useTTakePtr(x: TTakePtr) {
  _ = x
}

func useTTakeSafeTuple(x: TTakeSafeTuple) {
  _ = x
}

func useTTakeUnsafeTuple(x: TTakeUnsafeTuple) {
  _ = x
}

func useTTakeUnsafeTuple(x: HasUnsafe) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func useTTakeUnsafeTuple(x: AlsoUnsafe) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func explicitlyUnsafeTypes(a: ExplicitlyUnsafeStruct, 
                           b: HasUnsafeMember,
                           c: HasUnsafeBase,
                           d: WrapsUnsafeMember,
                           e: WrapsUnsafeBase,
                           f: WrapsUnannotatedMember) {
 // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
 _ = a // expected-note{{reference to parameter 'a' involves unsafe type}} 
 // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
 _ = b // expected-note{{reference to parameter 'b' involves unsafe type}}
 // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
 _ = c // expected-note{{reference to parameter 'c' involves unsafe type}}
 _ = d
 _ = e
 _ = f
}
