// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_ForeignReferenceTypeInheritance
// REQUIRES: std_span
// REQUIRES: executable_test

// UNSUPPORTED: back_deployment_runtime || use_os_stdlib

// RUN: %target-run-simple-swift-split-file(test.swift -I %t%{fs-sep}Inputs -target %target-swift-6.2-abi-triple \
// RUN:   -cxx-interoperability-mode=default -Xcc -std=c++20 -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature ForeignReferenceTypeInheritance)

// Signal if unsupported functions start compiling to remind the author of creating a runtime test case.
// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t%{fs-sep}Inputs \
// RUN:   -target %target-swift-6.2-abi-triple -cxx-interoperability-mode=default -Xcc -std=c++20 \
// RUN:   -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature ForeignReferenceTypeInheritance -D NOT_YET_SUPPORTED -verify \
// RUN:   -suppress-notes %t/test.swift

// This is the C++ counterpart of test/Interop/C/swiftify-import/runtime.swift.
// The C test covers the shapes a safe wrapper can take: counts, sizes, spans,
// out parameters, nullability and the bounds checks. This test deliberately
// sticks to one wrapped signature,
//
//   Impl callKind(std::span<const int> values) const
//
// and varies the C++ concepts involved instead: virtual
// dispatch, single and multiple inheritance, virtual (diamond) bases, reference
// types deriving from value types, class templates, chained calls returning
// `this`, non-const `this`, static member functions and constructors.
//
// Every function definition in the header returns its own value of the Impl
// enum, and functions that only forward to another function return whatever that
// one returned. Each test then names the implementation it expects to have run.
// Wrong data and a wrong `this` are reported as named values as well, so a
// failure says what went wrong rather than printing an unexpected number.
//
// Wrapped members come in two virtual function flavors:
//
//   - ...Indirect is implemented once in the base class and reaches the concrete
//     class only by calling the virtual kind().
//   - ...Direct is itself virtual, so each class has its own override returning
//     its own kind.
//
// At the moment only value types get wrappers for the Direct flavor, so the
// calls to a reference type's Direct members are commented out for now.

//--- Inputs/module.modulemap
module Test {
  header "header.h"
  requires cplusplus
  export std.span
}

//--- Inputs/header.h
#include <span>

#define FRT_IMMORTAL                                                           \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:immortal")))                               \
  __attribute__((swift_attr("release:immortal")))

using IntSpan = std::span<const int>;

enum class Impl {
  // Reported instead of the implementation's own value when a wrapper called
  // the right implementation with the wrong data or the wrong `this`.
  unexpectedValues,
  unexpectedSelf,
  // What the recorded field below holds before C++ writes to it.
  unwritten,

  shapeStaticKind,
  slabKind,
  prismKind,
  prismSelf,
  cubeKind,
  cubeHiddenKind,

  multiKind,
  secondMarker,
  diamondKind,

  templatedKind,
  templatedDerivedKind,
  crtpKind,
  crtpSelf,
  crtpBaseKind,
  crtpRefKind,
  crtpRefSelf,
  crtpRefBaseKind,

  valueBaseKind,
  valueDerivedKind,
  refDerivedKind,

  spanConstructed,
};

inline bool areOneTwoThree(IntSpan values) {
  return values.size() == 3 && values[0] == 1 && values[1] == 2 &&
         values[2] == 3;
}

// An abstract foreign reference type. callKindIndirect is not virtual, but it
// dispatches virtually internally.
struct FRT_IMMORTAL Shape {
  virtual Impl kind() const = 0;
  virtual ~Shape() = default;

  Impl callKindIndirect(IntSpan values [[clang::noescape]]) const {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return kind();
  }
  virtual Impl callKindDirect(IntSpan values [[clang::noescape]]) const = 0;

  // Non-const `this`: Swift reads the result back out of the recorded field.
  void recordKindIndirect(IntSpan values [[clang::noescape]]) {
    recorded = areOneTwoThree(values) ? kind() : Impl::unexpectedValues;
  }

  // Returns `this`, so that Swift can chain a second wrapped call onto the
  // result of the first.
  Shape *chainIndirect(IntSpan values [[clang::noescape]]) {
    recordKindIndirect(values);
    return this;
  }

  static Impl staticKind(IntSpan values [[clang::noescape]]) {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return Impl::shapeStaticKind;
  }

  Impl recorded = Impl::unwritten;
};

struct Slab : Shape {
  Impl kind() const override { return Impl::slabKind; }
  Impl callKindDirect(IntSpan values [[clang::noescape]]) const override {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return Impl::slabKind;
  }
  static Slab &create() {
    static Slab instance;
    return instance;
  }
};

// A derived class that declares a wrapped method of its own and has a field of its own.
struct Prism : Shape {
  Impl kind() const override { return Impl::prismKind; }
  Impl callKindDirect(IntSpan values [[clang::noescape]]) const override {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return Impl::prismKind;
  }

  // Reading a field that only exists in the derived class, so a wrapper that
  // passed the wrong `this` reports unexpectedSelf.
  Impl derivedCallKindIndirect(IntSpan values [[clang::noescape]]) const {
    if (selfMarker != Impl::prismSelf)
      return Impl::unexpectedSelf;
    return callKindIndirect(values);
  }

  static Prism &create() {
    static Prism instance;
    return instance;
  }
  Impl selfMarker = Impl::prismSelf;
};

// An intermediate class that overrides nothing, so that Cube below inherits
// wrappers across two levels of the hierarchy.
struct Passthrough : Prism {};

struct Cube final : Passthrough {
  Impl kind() const override { return Impl::cubeKind; }

  // Hides Prism::derivedCallKindIndirect rather than overriding it, since
  // neither is virtual. Swift's super.foo() syntax is what reaches the hidden one.
  Impl derivedCallKindIndirect(IntSpan values [[clang::noescape]]) const {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return Impl::cubeHiddenKind;
  }
  Impl callKindDirect(IntSpan values [[clang::noescape]]) const override {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return Impl::cubeKind;
  }
  static Cube &create() {
    static Cube instance;
    return instance;
  }
};

__attribute__((swift_name("makeShape(cube:)")))
inline Shape *makeShape(bool cube) {
  if (cube)
    return &Cube::create();
  return &Slab::create();
}

// A second base class, contributing a field and a wrapped method of its own.
// Shape is the primary base, so Swift makes Shape the Swift superclass of
// MultiDerived and clones Second's members into it.
struct Second {
  Impl secondCallKindIndirect(IntSpan values [[clang::noescape]]) const {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return marker;
  }
  Impl marker = Impl::secondMarker;
};

struct MultiDerived : Shape, Second {
  // Reaching Second's field requires `this` to be adjusted, so an override
  // called with an unadjusted `this` reports unexpectedSelf.
  Impl kind() const override {
    return marker == Impl::secondMarker ? Impl::multiKind : Impl::unexpectedSelf;
  }
  Impl callKindDirect(IntSpan values [[clang::noescape]]) const override {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return kind();
  }
  static MultiDerived &create() {
    static MultiDerived instance;
    return instance;
  }
};

// The diamond pattern: Diamond has one shared VirtualBase subobject, reached
// through a vbase offset rather than at a fixed offset from `this`.
struct FRT_IMMORTAL VirtualBase {
  virtual Impl kind() const = 0;
  virtual ~VirtualBase() = default;

  Impl callKindIndirect(IntSpan values [[clang::noescape]]) const {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return kind();
  }
};

struct Left : virtual VirtualBase {};
struct Right : virtual VirtualBase {};

struct Diamond : Left, Right {
  Impl kind() const override { return Impl::diamondKind; }
  static Diamond &create() {
    static Diamond instance;
    return instance;
  }
};

// A class template. Its own members do not get wrappers, but a wrapper it
// inherits from a non-template reference type does.
template <class T>
struct FRT_IMMORTAL Templated {
  virtual Impl kind() const { return Impl::templatedKind; }
  virtual ~Templated() = default;

  Impl callKindIndirect(std::span<const T> values [[clang::noescape]]) const {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return kind();
  }

  static Templated<T> &create() {
    static Templated<T> instance;
    return instance;
  }
};
using TemplatedInt = Templated<int>;

template <class T>
struct TemplatedDerived : Shape {
  Impl kind() const override { return Impl::templatedDerivedKind; }
  Impl callKindDirect(IntSpan values [[clang::noescape]]) const override {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return Impl::templatedDerivedKind;
  }
  static TemplatedDerived<T> &create() {
    static TemplatedDerived<T> instance;
    return instance;
  }
};
using TemplatedDerivedInt = TemplatedDerived<int>;

// The curiously recurring template pattern: CRTPBase resolves the call to the
// derived class at compile time instead of through a vtable.
template <class Derived>
struct CRTPBase {
  Impl dispatchKind() const {
    return static_cast<const Derived *>(this)->kind();
  }

  // A wrapped member of the template itself, for contrast with the one
  // CRTPConcrete declares below.
  Impl baseCallKindIndirect(IntSpan values [[clang::noescape]]) const {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return dispatchKind();
  }

  // A CRTP base has no reason to declare virtual functions , but nothing stops it from
  // doing so, and the Direct flavor is what a class template's virtual member
  // looks like.
  virtual Impl baseCallKindDirect(IntSpan values [[clang::noescape]]) const {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return Impl::crtpBaseKind;
  }
  virtual ~CRTPBase() = default;
};

struct FRT_IMMORTAL CRTPConcrete : CRTPBase<CRTPConcrete> {
  // Not virtual: the static_cast in CRTPBase is what finds this, and it reads a
  // field of its own, so a wrapper that passed the wrong `this` reports
  // unexpectedSelf rather than the wrong kind.
  Impl kind() const {
    return selfMarker == Impl::crtpSelf ? Impl::crtpKind : Impl::unexpectedSelf;
  }

  Impl callKindIndirect(IntSpan values [[clang::noescape]]) const {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return dispatchKind();
  }
  Impl baseCallKindDirect(IntSpan values [[clang::noescape]]) const override {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return kind();
  }

  static CRTPConcrete &create() {
    static CRTPConcrete instance;
    return instance;
  }
  Impl selfMarker = Impl::crtpSelf;
};

// The same pattern with the CRTP base a reference type as well. Note that
// CRTPRefConcrete inherits its reference semantics rather than repeating the
// annotation: a class that annotates itself is imported as an independent class
// with no Swift superclass, which is what puts CRTPConcrete above and RefDerived
// below in the cloned-wrapper cases.
template <class Derived>
struct FRT_IMMORTAL CRTPRefBase {
  Impl dispatchKind() const {
    return static_cast<const Derived *>(this)->kind();
  }

  Impl baseCallKindIndirect(IntSpan values [[clang::noescape]]) const {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return dispatchKind();
  }
  virtual Impl baseCallKindDirect(IntSpan values [[clang::noescape]]) const {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return Impl::crtpRefBaseKind;
  }
  virtual ~CRTPRefBase() = default;
};

struct CRTPRefConcrete : CRTPRefBase<CRTPRefConcrete> {
  Impl kind() const {
    return selfMarker == Impl::crtpRefSelf ? Impl::crtpRefKind
                                           : Impl::unexpectedSelf;
  }
  Impl baseCallKindDirect(IntSpan values [[clang::noescape]]) const override {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return kind();
  }

  static CRTPRefConcrete &create() {
    static CRTPRefConcrete instance;
    return instance;
  }
  Impl selfMarker = Impl::crtpRefSelf;
};

using CRTPRefBaseOfConcrete = CRTPRefBase<CRTPRefConcrete>;

inline CRTPRefBaseOfConcrete *makeCRTPRefBase() {
  return &CRTPRefConcrete::create();
}

// A value type is polymorphic in C++ but not in Swift. Unlike a reference type
// it does get a wrapper for its virtual methods.
struct ValueBase {
  virtual Impl kind() const { return Impl::valueBaseKind; }
  virtual ~ValueBase() = default;

  Impl callKindIndirect(IntSpan values [[clang::noescape]]) const {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return kind();
  }
  virtual Impl callKindDirect(IntSpan values [[clang::noescape]]) const {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return Impl::valueBaseKind;
  }
};

struct ValueDerived : ValueBase {
  Impl kind() const override { return Impl::valueDerivedKind; }
  Impl callKindDirect(IntSpan values [[clang::noescape]]) const override {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return Impl::valueDerivedKind;
  }
};

// Only the derived class is annotated as a reference type, so the wrapper in the
// value type base is cloned into a Swift class rather than inherited.
struct FRT_IMMORTAL RefDerived : ValueBase {
  Impl kind() const override { return Impl::refDerivedKind; }
  Impl callKindDirect(IntSpan values [[clang::noescape]]) const override {
    if (!areOneTwoThree(values))
      return Impl::unexpectedValues;
    return Impl::refDerivedKind;
  }
  static RefDerived &create() {
    static RefDerived instance;
    return instance;
  }
};

struct SpanConstructed {
  SpanConstructed(IntSpan values [[clang::noescape]])
      : kind(areOneTwoThree(values) ? Impl::spanConstructed
                                    : Impl::unexpectedValues) {}
  Impl kind;
};

//--- test.swift
import StdlibUnittest
import Test

var Suite = TestSuite("C++ safe interop wrappers")

let values: [CInt] = [1, 2, 3]

// MARK: - Reference type hierarchies

#if NOT_YET_SUPPORTED
// If any of these stop being an error, upgrade it to a proper runtime test case.

// FIXME: calling a wrapper through 'super' is rejected: only a wrapper for the
// virtual dispatch thunk currently exists, so there is no static dispatch
// version of the safe wrapper to remap the call to.
extension Cube {
  func callKindIndirectViaSuper(_ values: Span<CInt>) -> Impl {
    // expected-error@+1 {{calling safe interop wrapper 'callKindIndirect' in foreign reference type 'Shape' using 'super' is not supported}}
    super.callKindIndirect(values)
  }
}

extension Cube {
  func callKindDirectViaSuper(_ values: Span<CInt>) -> Impl {
    // expected-error@+1 {{calling safe interop wrapper 'callKindDirect' in foreign reference type 'Shape' using 'super' is not supported}}
    super.callKindDirect(values)
  }
}

extension Cube {
  func derivedCallKindIndirectViaSuper(_ values: Span<CInt>) -> Impl {
    // expected-error@+1 {{calling safe interop wrapper 'derivedCallKindIndirect' in foreign reference type 'Prism' using 'super' is not supported}}
    super.derivedCallKindIndirect(values)
  }
}

Suite.test("hidden wrapper is ambiguous") {
  // FIXME: Cube's own wrapper and the one it inherits from Prism are both
  // visible, so the call cannot be resolved.
  // expected-error@+2 {{ambiguous use of 'derivedCallKindIndirect'}}
  expectEqual(.cubeHiddenKind,
              Cube.create().derivedCallKindIndirect(values.span))
}

Suite.test("cloned wrapper: base that is not the primary base") {
  // Second is not the primary base, so Shape becomes the Swift superclass and Second's members are
  // cloned into MultiDerived, but safe wrappers are not cloned.
  // expected-error@+2 {{cannot convert value of type 'Span<CInt>' (aka 'Span<Int32>') to expected argument type 'IntSpan'}}
  expectEqual(.secondMarker,
              MultiDerived.create().secondCallKindIndirect(values.span))
}

Suite.test("cloned wrapper: virtual base") {
  // A virtual base is never imported as the superclass, so Diamond has no superclass at all and clones everything.
  // expected-error@+1 {{cannot convert value of type 'Span<CInt>' (aka 'Span<Int32>') to expected argument type 'IntSpan'}}
  expectEqual(.diamondKind, Diamond.create().callKindIndirect(values.span))
}

Suite.test("cloned wrapper: value type base of a reference type") {
  // A Swift class cannot inherit from a struct.
  // expected-error@+1 {{cannot convert value of type 'Span<CInt>' (aka 'Span<Int32>') to expected argument type 'IntSpan'}}
  expectEqual(.refDerivedKind, RefDerived.create().callKindIndirect(values.span))
}

Suite.test("cloned wrapper: value type derived class") {
  // Swift structs have no inheritance at all, so a value type derived class clones every base member.
  // FIXME: emit safe wrappers for cloned functions.
  // expected-error@+1 {{cannot convert value of type 'Span<CInt>' (aka 'Span<Int32>') to expected argument type 'IntSpan'}}
  expectEqual(.valueDerivedKind, ValueDerived().callKindIndirect(values.span))
}

Suite.test("cloned wrapper: CRTP base") {
  // The CRTP base is a value type too, so even though the wrapper is inferred for CRTPBase<CRTPConcrete>,
  // CRTPConcrete doesn't inherit the safe wrapper.
  // expected-error@+2 {{cannot convert value of type 'Span<CInt>' (aka 'Span<Int32>') to expected argument type 'IntSpan'}}
  expectEqual(.crtpKind,
              CRTPConcrete.create().baseCallKindIndirect(values.span))
}
#endif

Suite.test("virtual dispatch through a wrapper on the base class") {
  expectEqual(.slabKind, Slab.create().callKindIndirect(values.span))
  // Cube inherits the wrapper across two levels of the hierarchy.
  expectEqual(.cubeKind, Cube.create().callKindIndirect(values.span))
  // Reached through a base class reference, so Swift cannot see the dynamic type.
  expectEqual(.cubeKind, makeShape(cube: true)!.callKindIndirect(values.span))
}

Suite.test("wrapper declared on a derived class") {
  expectEqual(.prismKind, Prism.create().derivedCallKindIndirect(values.span))
}

Suite.test("multiple inheritance") {
  // MultiDerived::kind() reads a field of its second base class, so it reports
  // unexpectedSelf if the wrapper passed a `this` that was not adjusted back to
  // the most derived object.
  expectEqual(.multiKind, MultiDerived.create().callKindIndirect(values.span))
}

Suite.test("member of a class template") {
  expectEqual(.templatedKind, TemplatedInt.create().callKindIndirect(values.span))
}

Suite.test("class template deriving from a reference type") {
  expectEqual(.templatedDerivedKind,
              TemplatedDerivedInt.create().callKindIndirect(values.span))
}

Suite.test("curiously recurring template pattern") {
  // The wrapper is declared on the concrete class, and its body reaches the
  // concrete class again through CRTPBase's static_cast.
  expectEqual(.crtpKind, CRTPConcrete.create().callKindIndirect(values.span))

  // With the CRTP base a reference type too, the base's own wrapper is callable
  // on a derived object handed back as the base. Its static_cast back down to
  // CRTPRefConcrete finds the right object.
  expectEqual(.crtpRefKind, makeCRTPRefBase()!.baseCallKindIndirect(values.span))
}

Suite.test("wrapper on a virtual method of a reference type") {
  // An override does not get a wrapper of its own when it inherits one, so there
  // is a single wrapper per hierarchy and it dispatches to the override.
  expectEqual(.slabKind, Slab.create().callKindDirect(values.span))
  expectEqual(.cubeKind, Cube.create().callKindDirect(values.span))
  expectEqual(.multiKind, MultiDerived.create().callKindDirect(values.span))
  expectEqual(.templatedDerivedKind,
              TemplatedDerivedInt.create().callKindDirect(values.span))

  // CRTPBase is a value type, so its wrapper is cloned rather than inherited and
  // CRTPConcrete's override keeps one of its own.
  expectEqual(.crtpKind, CRTPConcrete.create().baseCallKindDirect(values.span))
  // Called on the base class, whose wrapper dispatches to the override.
  expectEqual(.crtpRefKind, makeCRTPRefBase()!.baseCallKindDirect(values.span))
  // ValueBase is a value type, so again RefDerived's override keeps its own.
  expectEqual(.refDerivedKind, RefDerived.create().callKindDirect(values.span))
}

Suite.test("non-const this") {
  let cube = Cube.create()
  cube.recorded = .unwritten
  cube.recordKindIndirect(values.span)
  expectEqual(.cubeKind, cube.recorded)
}

Suite.test("chained calls through a returned this") {
  let cube = Cube.create()
  cube.recorded = .unwritten
  expectEqual(.cubeKind, cube.chainIndirect(values.span)!.callKindIndirect(values.span))
  // chainIndirect records the kind through `this->recorded`.
  expectEqual(.cubeKind, cube.recorded)
}

Suite.test("static member function") {
  expectEqual(.shapeStaticKind, Shape.staticKind(values.span))
}

// MARK: - Value types

Suite.test("virtual dispatch through a wrapper on a value type") {
  expectEqual(.valueBaseKind, ValueBase().callKindIndirect(values.span))
}

Suite.test("wrapper on a virtual method of a value type") {
  // Unlike a reference type, a value type does get wrappers for the Direct
  // flavor, both for the base implementation and for the override.
  expectEqual(.valueBaseKind, ValueBase().callKindDirect(values.span))
  expectEqual(.valueDerivedKind, ValueDerived().callKindDirect(values.span))
}

// MARK: - Constructors

Suite.test("constructor") {
  expectEqual(.spanConstructed, SpanConstructed(values.span).kind)
}

runAllTests()
