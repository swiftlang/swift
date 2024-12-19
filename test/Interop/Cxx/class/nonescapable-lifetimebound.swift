// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -I %swift_src_root/lib/ClangImporter/SwiftBridging  -I %t/Inputs -emit-sil %t/test.swift -enable-experimental-feature LifetimeDependence -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -I %swift_src_root/lib/ClangImporter/SwiftBridging  -I %t/Inputs -emit-sil %t/test.swift -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s -check-prefix=CHECK-NO-LIFETIMES


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
    OtherView() : member(nullptr) {}
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
    TestAnnotationTranslation(const TestAnnotationTranslation& [[clang::lifetimebound]]) = default;
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

// CHECK: sil [clang makeOwner] {{.*}}: $@convention(c) () -> Owner
// CHECK: sil [clang getView] {{.*}} : $@convention(c) (@in_guaranteed Owner) -> @lifetime(borrow 0) @autoreleased View
// CHECK: sil [clang getViewFromFirst] {{.*}} : $@convention(c) (@in_guaranteed Owner, @in_guaranteed Owner) -> @lifetime(borrow 0) @autoreleased View
// CHECK: sil [clang getViewFromEither] {{.*}} : $@convention(c) (@in_guaranteed Owner, @in_guaranteed Owner) -> @lifetime(borrow 0, borrow 1) @autoreleased View
// CHECK: sil [clang Owner.handOutView] {{.*}} : $@convention(cxx_method) (@in_guaranteed Owner) -> @lifetime(borrow 0) @autoreleased View
// CHECK: sil [clang Owner.handOutView2] {{.*}} : $@convention(cxx_method) (View, @in_guaranteed Owner) -> @lifetime(borrow 1) @autoreleased View
// CHECK: sil [clang getViewFromEither] {{.*}} : $@convention(c) (@guaranteed View, @guaranteed View) -> @lifetime(copy 0, copy 1) @autoreleased View
// CHECK: sil [clang View.init] {{.*}} : $@convention(c) () -> @lifetime(immortal) @out View
// CHECK: sil [clang OtherView.init] {{.*}} : $@convention(c) (@guaranteed View) -> @lifetime(copy 0) @out OtherView
// CHECK: sil [clang returnsImmortal] {{.*}} : $@convention(c) () -> @lifetime(immortal) @autoreleased View
// CHECK: sil [clang copyView] {{.*}} : $@convention(c) (View, @lifetime(copy 0) @inout View) -> ()
// CHECK: sil [clang getCaptureView] {{.*}} : $@convention(c) (@in_guaranteed Owner) -> @lifetime(borrow 0) @autoreleased CaptureView
// CHECK: sil [clang CaptureView.captureView] {{.*}} : $@convention(cxx_method) (View, @lifetime(copy 0) @inout CaptureView) -> ()
// CHECK: sil [clang CaptureView.handOut] {{.*}} : $@convention(cxx_method) (@lifetime(copy 1) @inout View, @in_guaranteed CaptureView) -> ()

// CHECK-NO-LIFETIMES: nonescapable.h:36:6: error: returning ~Escapable type requires '-enable-experimental-feature LifetimeDependence'
// CHECK-NO-LIFETIMES: nonescapable.h:40:6: error: returning ~Escapable type requires '-enable-experimental-feature LifetimeDependence'
// CHECK-NO-LIFETIMES: nonescapable.h:46:6: error: returning ~Escapable type requires '-enable-experimental-feature LifetimeDependence'
// CHECK-NO-LIFETIMES: nonescapable.h:53:6: error: returning ~Escapable type requires '-enable-experimental-feature LifetimeDependence'
// CHECK-NO-LIFETIMES: nonescapable.h:23:10: error: returning ~Escapable type requires '-enable-experimental-feature LifetimeDependence'
// CHECK-NO-LIFETIMES: nonescapable.h:27:10: error: returning ~Escapable type requires '-enable-experimental-feature LifetimeDependence'
// CHECK-NO-LIFETIMES: nonescapable.h:4:5: error: returning ~Escapable type requires '-enable-experimental-feature LifetimeDependence'
// CHECK-NO-LIFETIMES: nonescapable.h:5:5: error: returning ~Escapable type requires '-enable-experimental-feature LifetimeDependence'
// CHECK-NO-LIFETIMES: nonescapable.h:13:5: error: returning ~Escapable type requires '-enable-experimental-feature LifetimeDependence'
// CHECK-NO-LIFETIMES: nonescapable.h:14:5: error: returning ~Escapable type requires '-enable-experimental-feature LifetimeDependence'
// CHECK-NO-LIFETIMES: nonescapable.h:68:6: error: returning ~Escapable type requires '-enable-experimental-feature LifetimeDependence'
// CHECK-NO-LIFETIMES: nonescapable.h:91:13: error: returning ~Escapable type requires '-enable-experimental-feature LifetimeDependence'
// CHECK-NO-LIFETIMES-NOT: error

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
}
