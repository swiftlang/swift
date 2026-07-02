// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend  -I %t/Inputs -emit-sil %t/test.swift -enable-experimental-feature LifetimeDependence -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend  -I %t/Inputs -emit-sil %t/test.swift -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend  -I %t/Inputs -emit-sil -verify %t/escaping_scopes.swift -enable-experimental-feature Lifetimes -cxx-interoperability-mode=default -diagnostic-style llvm

// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_Lifetimes

//--- Inputs/module.modulemap
module Test {
    header "nonescapable.h"
    requires cplusplus
}

//--- Inputs/nonescapable.h
#include "swift/bridging"

struct SWIFT_NONESCAPABLE View {
    View() : member(nullptr) {}
    View(const int *p [[clang::lifetimebound]]) : member(p) {}
    View(const View&) = default;
private:
    const int *member;
    friend struct OtherView;
};

struct SWIFT_NONESCAPABLE OtherView {
    OtherView(View v [[clang::lifetimebound]]) : member(v.member) {}
    OtherView(const OtherView&) = default;
private:
    const int *member;
};

struct Owner {
    int data;

    View handOutView() const [[clang::lifetimebound]] {
        return View(&data);
    }

    View handOutView2(View v) const [[clang::lifetimebound]] {
        return View(&data);
    }
};

Owner makeOwner() {
    return Owner{42};
}

View getView(const Owner& owner [[clang::lifetimebound]]) {
    return View(&owner.data);
}

View getViewFromFirst(const Owner& owner [[clang::lifetimebound]], const Owner& owner2) {
    return View(&owner.data);
}

bool coinFlip;

View getViewFromEither(const Owner& owner [[clang::lifetimebound]], const Owner& owner2 [[clang::lifetimebound]]) {
    if (coinFlip)
        return View(&owner.data);
    else
        return View(&owner2.data);
}

View getViewFromEither(View view1 [[clang::lifetimebound]], View view2 [[clang::lifetimebound]]) {
    if (coinFlip)
        return view1;
    else
        return view2;
}

struct SWIFT_NONESCAPABLE TestAnnotationTranslation {
    TestAnnotationTranslation() : member(nullptr) {}
    TestAnnotationTranslation(const int *p [[clang::lifetimebound]]) : member(p) {}
    TestAnnotationTranslation(const TestAnnotationTranslation& other [[clang::lifetimebound]]) = default;
private:
    const int *member;
};

View returnsImmortal() SWIFT_RETURNS_INDEPENDENT_VALUE {
    return View();
}

void copyView(View view1 [[clang::lifetime_capture_by(view2)]], View &view2) {
    view2 = view1;
}

struct SWIFT_NONESCAPABLE CaptureView {
    CaptureView() : view(nullptr) {}
    CaptureView(View p [[clang::lifetimebound]]) : view(p) {}

    void captureView(View v [[clang::lifetime_capture_by(this)]]) {
        view = v;
    }

    void handOut(View &v) const [[clang::lifetime_capture_by(v)]] {
       v = view; 
    }

    View view;
};

CaptureView getCaptureView(const Owner& owner [[clang::lifetimebound]]) {
    return CaptureView(View{&owner.data});
}

struct SWIFT_NONESCAPABLE AggregateView {
    const int *member;
};

namespace NS {
    View getView(const Owner& owner [[clang::lifetimebound]]) {
        return View(&owner.data);
    }
}

struct SWIFT_NONCOPYABLE SWIFT_NONESCAPABLE MoveOnly {
    MoveOnly();
    MoveOnly(const MoveOnly& other) = delete;
    MoveOnly& operator=(const MoveOnly&) = delete;
    MoveOnly(MoveOnly&& other) = default;
    MoveOnly& operator=(MoveOnly&& other) = default;
    ~MoveOnly();

private:
    int *i;
};

MoveOnly moveOnlyId(const MoveOnly& p [[clang::lifetimebound]]);

namespace rdar153081347 {
namespace Detail {
template<typename T>
class SWIFT_NONESCAPABLE Span {
public:
   constexpr Span() = default;
   constexpr Span(T* p [[clang::lifetimebound]], unsigned long s) : m_ptr(p), m_size(s) {}

   template<unsigned long size>
   constexpr Span(T (&a)[size]) : m_ptr(a), m_size(size) {}
protected:
  T* m_ptr { nullptr };
  unsigned long m_size { 0 };
};
} // namespace Detail

template <typename T>
class SWIFT_NONESCAPABLE Span : public Detail::Span<T> {
public:
  using Detail::Span<T>::Span;

  constexpr Span() = default;

  constexpr T const* data() const { return this->m_ptr; }
  constexpr T* data() { return this->m_ptr; }

  constexpr unsigned long size() const { return this->m_size; }

};

template<typename T>
using ReadonlySpan = Span<T const>;

using ReadonlyBytes = ReadonlySpan<unsigned char>;
using Bytes = Span<unsigned char>;
} // namespace rdar153081347

struct SWIFT_NONESCAPABLE NonEscapable {
    const int *p;
};

template<typename T>
struct HasAnonUnion {
    union {
        int known;
        T unknown;
    };
};

template<typename T>
struct HasAnonStruct {
    struct {
        int known;
        T unknown;
    };
};

template<typename T>
struct SWIFT_NONESCAPABLE NonEscapableHasAnonUnion {
    union {
        int known;
        T unknown;
    };
};

using HasAnonUnionNonEscapable = HasAnonUnion<NonEscapable>;
using HasAnonStructNonEscapable = HasAnonStruct<NonEscapable>;
using NonEscapableHasAnonUnionNonEscapable = NonEscapableHasAnonUnion<NonEscapable>;

HasAnonUnionNonEscapable makeAnonUnionNonEscapable(const Owner &owner [[clang::lifetimebound]]) {
    HasAnonUnionNonEscapable result;
    result.unknown = {&owner.data};
    return result;
}

HasAnonStructNonEscapable makeAnonStructNonEscapable(const Owner &owner [[clang::lifetimebound]]) {
    return {1, &owner.data};
}

NonEscapableHasAnonUnionNonEscapable makeNonEscapableHasAnonUnionNonEscapable(
                                        const Owner &owner [[clang::lifetimebound]]) {
    NonEscapableHasAnonUnionNonEscapable result;
    result.unknown = {&owner.data};
    return result;
}

// Non-trivial owner: field access opens a formal access scope.
struct NonTrivialOwner {
    int data;
    NonTrivialOwner() : data(0) {}
    NonTrivialOwner(const NonTrivialOwner &other) : data(other.data) {}
    View handOutView() const [[clang::lifetimebound]] {
        return View(&data);
    }
};

// Transitive: a value type whose field is itself an owner.
struct OwnerBox {
    NonTrivialOwner field;
};

class SharedTrivialOwner {
public:
    Owner field;
} SWIFT_SHARED_REFERENCE(retainSharedTrivialOwner, releaseSharedTrivialOwner);
inline void retainSharedTrivialOwner(SharedTrivialOwner *) {}
inline void releaseSharedTrivialOwner(SharedTrivialOwner *) {}

class SharedOwner {
public:
    NonTrivialOwner field;
} SWIFT_SHARED_REFERENCE(retainSharedOwner, releaseSharedOwner);
inline void retainSharedOwner(SharedOwner *) {}
inline void releaseSharedOwner(SharedOwner *) {}

class SharedOwnerBox {
public:
    OwnerBox field;
} SWIFT_SHARED_REFERENCE(retainSharedOwnerBox, releaseSharedOwnerBox);
inline void retainSharedOwnerBox(SharedOwnerBox *) {}
inline void releaseSharedOwnerBox(SharedOwnerBox *) {}

// CHECK: sil {{.*}}[clang makeOwner] {{.*}}: $@convention(c) () -> Owner
// CHECK: sil {{.*}}[clang getView] {{.*}} : $@convention(c) (@in_guaranteed Owner) -> @lifetime(borrow address 0) @owned View
// CHECK: sil {{.*}}[clang getViewFromFirst] {{.*}} : $@convention(c) (@in_guaranteed Owner, @in_guaranteed Owner) -> @lifetime(borrow address 0) @owned View
// CHECK: sil {{.*}}[clang getViewFromEither] {{.*}} : $@convention(c) (@in_guaranteed Owner, @in_guaranteed Owner) -> @lifetime(borrow address 0, borrow address 1) @owned View
// CHECK: sil {{.*}}[clang Owner.handOutView] {{.*}} : $@convention(cxx_method) (@in_guaranteed Owner) -> @lifetime(borrow address_for_deps 0) @owned View
// CHECK: sil {{.*}}[clang Owner.handOutView2] {{.*}} : $@convention(cxx_method) (View, @in_guaranteed Owner) -> @lifetime(borrow address_for_deps 1) @owned View
// CHECK: sil {{.*}}[clang getViewFromEither] {{.*}} : $@convention(c) (View, View) -> @lifetime(copy 0, copy 1) @owned View
// CHECK: sil {{.*}}[clang View.init] {{.*}} : $@convention(c) () -> @lifetime(immortal) @out View
// CHECK: sil {{.*}}[clang OtherView.init] {{.*}} : $@convention(c) (View) -> @lifetime(copy 0) @out OtherView
// CHECK: sil {{.*}}[clang returnsImmortal] {{.*}} : $@convention(c) () -> @lifetime(immortal) @owned View
// CHECK: sil {{.*}}[clang copyView] {{.*}} : $@convention(c) (View, @lifetime(copy 0) @inout View) -> ()
// CHECK: sil {{.*}}[clang getCaptureView] {{.*}} : $@convention(c) (@in_guaranteed Owner) -> @lifetime(borrow address 0) @owned CaptureView
// CHECK: sil {{.*}}[clang CaptureView.captureView] {{.*}} : $@convention(cxx_method) (View, @lifetime(copy 0) @inout CaptureView) -> ()
// CHECK: sil {{.*}}[clang CaptureView.handOut] {{.*}} : $@convention(cxx_method) (@lifetime(copy 1) @inout View, @in_guaranteed CaptureView) -> ()
// CHECK: sil {{.*}}[clang NS.getView] {{.*}} : $@convention(c) (@in_guaranteed Owner) -> @lifetime(borrow address 0) @owned View
// CHECK: sil {{.*}}[clang moveOnlyId] {{.*}} : $@convention(c) (@in_guaranteed MoveOnly) -> @lifetime(borrow {{.*}}0) @out MoveOnly
// CHECK: sil {{.*}}[clang makeAnonUnionNonEscapable] {{.*}} : $@convention(c) (@in_guaranteed Owner) -> @lifetime(borrow address 0) @owned HasAnonUnion<NonEscapable>
// CHECK: sil {{.*}}[clang makeAnonStructNonEscapable] {{.*}} : $@convention(c) (@in_guaranteed Owner) -> @lifetime(borrow address 0) @owned HasAnonStruct<NonEscapable>
// CHECK: sil {{.*}}[clang makeNonEscapableHasAnonUnionNonEscapable] {{.*}} : $@convention(c) (@in_guaranteed Owner) -> @lifetime(borrow address 0) @owned NonEscapableHasAnonUnion<NonEscapable>

//--- test.swift

import Test

public func test() {
    let o = makeOwner()
    let o2 = makeOwner()
    var v1 = getView(o)
    let v2 = getViewFromFirst(o, o2)
    let _ = getViewFromEither(o, o2)
    let _ = o.handOutView()
    let _ = o.handOutView2(v1)
    let _ = getViewFromEither(v1, v2)
    let defaultView = View()
    let _ = OtherView(defaultView)
    let _ = returnsImmortal()
    copyView(v2, &v1)
    var cv = getCaptureView(o)
    cv.captureView(v1)
    cv.handOut(&v1)
    var _ = NS.getView(o)
}

public func test2(_ x: AggregateView) {
    let _ = AggregateView(member: x.member)
}

func canImportMoveOnlyNonEscapable(_ x: borrowing MoveOnly) {
    let _ = moveOnlyId(x);
}

func testInheritedCtors(_ s: rdar153081347.Bytes) {}

func anonymousUnionsAndStructs(_ v: borrowing View) {
    let o = makeOwner()
    let _ = makeAnonUnionNonEscapable(o)
    let _ = makeAnonStructNonEscapable(o)
    let _ = makeNonEscapableHasAnonUnionNonEscapable(o)
}

//--- escaping_scopes.swift

import Test

var globalOwner = makeOwner()

struct Wrapper { var o: Owner }
var globalWrapper = Wrapper(o: makeOwner())

@_lifetime(immortal)
func viaMethod() -> View {
  return globalOwner.handOutView()
  // expected-error @-1 {{lifetime-dependent value escapes its scope}}
  // expected-note @-2 {{it depends on this scoped access to variable 'globalOwner'}}
  // expected-note @-3 {{this use causes the lifetime-dependent value to escape}}
}

@_lifetime(immortal)
func viaFreeFunc() -> View {
  return getView(globalOwner)
  // expected-error @-1 {{lifetime-dependent value escapes its scope}}
  // expected-note @-2 {{it depends on this scoped access to variable 'globalOwner'}}
  // expected-note @-3 {{this use causes the lifetime-dependent value to escape}}
}

@_lifetime(immortal)
func viaFieldMethod() -> View {
  return globalWrapper.o.handOutView()
  // expected-error @-1 {{lifetime-dependent value escapes its scope}}
  // expected-note @-2 {{it depends on this scoped access to variable 'globalWrapper'}}
  // expected-note @-3 {{this use causes the lifetime-dependent value to escape}}
}

@_lifetime(immortal)
func viaFieldFreeFunc() -> View {
  return getView(globalWrapper.o)
  // expected-error @-1 {{lifetime-dependent value escapes its scope}}
  // expected-note @-2 {{it depends on this scoped access to variable 'globalWrapper'}}
  // expected-note @-3 {{this use causes the lifetime-dependent value to escape}}
}

final class SwiftBoxTrivial { var field = Owner(data: 0) }
final class SwiftBoxOwner { var field = NonTrivialOwner() }
final class SwiftBoxNested { var field = OwnerBox() }

// Foreign reference type base, trivial field.
@available(macOS 13.3, *)
@_lifetime(borrow x)
func frtTrivialField(x: SharedTrivialOwner) -> View {
  return x.field.handOutView()
  // expected-error @-1 {{lifetime-dependent value escapes its scope}}
  // expected-note @-2 {{it depends on this scoped access to variable 'field'}}
  // expected-note @-3 {{this use causes the lifetime-dependent value to escape}}
}

// Foreign reference type base, non-trivial field.
@available(macOS 13.3, *)
@_lifetime(borrow x)
func frtField(x: SharedOwner) -> View {
  return x.field.handOutView()
  // expected-error @-1 {{lifetime-dependent value escapes its scope}}
  // expected-note @-2 {{it depends on this scoped access to variable 'field'}}
  // expected-note @-3 {{this use causes the lifetime-dependent value to escape}}
}

// Foreign reference type base, transitive field access.
@available(macOS 13.3, *)
@_lifetime(borrow x)
func frtTransitiveField(x: SharedOwnerBox) -> View {
  return x.field.field.handOutView()
  // expected-error @-1 {{lifetime-dependent value escapes its scope}}
  // expected-note @-2 {{it depends on this scoped access to variable 'field'}}
  // expected-note @-3 {{this use causes the lifetime-dependent value to escape}}
}

// Swift class base, trivial field.
@_lifetime(borrow b)
func classTrivialField(b: SwiftBoxTrivial) -> View {
  return b.field.handOutView()
  // expected-error @-1 {{lifetime-dependent value escapes its scope}}
  // expected-note @-2 {{it depends on this scoped access to variable 'field'}}
  // expected-note @-3 {{this use causes the lifetime-dependent value to escape}}
}

// Swift class base, non-trivial field.
@_lifetime(borrow b)
func classField(b: SwiftBoxOwner) -> View {
  return b.field.handOutView()
  // expected-error @-1 {{lifetime-dependent value escapes its scope}}
  // expected-note @-2 {{it depends on this scoped access to variable 'field'}}
  // expected-note @-3 {{this use causes the lifetime-dependent value to escape}}
}

// Swift class base, transitive field access.
@_lifetime(borrow b)
func classTransitiveField(b: SwiftBoxNested) -> View {
  return b.field.field.handOutView()
  // expected-error @-1 {{lifetime-dependent value escapes its scope}}
  // expected-note @-2 {{it depends on this scoped access to variable 'field'}}
  // expected-note @-3 {{this use causes the lifetime-dependent value to escape}}
}
