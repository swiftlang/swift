// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -I %swift_src_root/lib/ClangImporter/SwiftBridging  -I %t/Inputs -emit-sil %t/test.swift -enable-experimental-feature LifetimeDependence -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -I %swift_src_root/lib/ClangImporter/SwiftBridging  -I %t/Inputs -emit-sil %t/test.swift -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_LifetimeDependence

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

// CHECK: sil [clang makeOwner] {{.*}}: $@convention(c) () -> Owner
// CHECK: sil [clang getView] {{.*}} : $@convention(c) (@in_guaranteed Owner) -> @lifetime(borrow address 0) @owned View
// CHECK: sil [clang getViewFromFirst] {{.*}} : $@convention(c) (@in_guaranteed Owner, @in_guaranteed Owner) -> @lifetime(borrow address 0) @owned View
// CHECK: sil [clang getViewFromEither] {{.*}} : $@convention(c) (@in_guaranteed Owner, @in_guaranteed Owner) -> @lifetime(borrow address 0, borrow address 1) @owned View
// CHECK: sil [clang Owner.handOutView] {{.*}} : $@convention(cxx_method) (@in_guaranteed Owner) -> @lifetime(borrow 0) @owned View
// CHECK: sil [clang Owner.handOutView2] {{.*}} : $@convention(cxx_method) (View, @in_guaranteed Owner) -> @lifetime(borrow 1) @owned View
// CHECK: sil [clang getViewFromEither] {{.*}} : $@convention(c) (View, View) -> @lifetime(copy 0, copy 1) @owned View
// CHECK: sil [clang View.init] {{.*}} : $@convention(c) () -> @lifetime(immortal) @out View
// CHECK: sil [clang OtherView.init] {{.*}} : $@convention(c) (View) -> @lifetime(copy 0) @out OtherView
// CHECK: sil [clang returnsImmortal] {{.*}} : $@convention(c) () -> @lifetime(immortal) @owned View
// CHECK: sil [clang copyView] {{.*}} : $@convention(c) (View, @lifetime(copy 0) @inout View) -> ()
// CHECK: sil [clang getCaptureView] {{.*}} : $@convention(c) (@in_guaranteed Owner) -> @lifetime(borrow address 0) @owned CaptureView
// CHECK: sil [clang CaptureView.captureView] {{.*}} : $@convention(cxx_method) (View, @lifetime(copy 0) @inout CaptureView) -> ()
// CHECK: sil [clang CaptureView.handOut] {{.*}} : $@convention(cxx_method) (@lifetime(copy 1) @inout View, @in_guaranteed CaptureView) -> ()
// CHECK: sil [clang NS.getView] {{.*}} : $@convention(c) (@in_guaranteed Owner) -> @lifetime(borrow address 0) @owned View
// CHECK: sil [clang moveOnlyId] {{.*}} : $@convention(c) (@in_guaranteed MoveOnly) -> @lifetime(borrow {{.*}}0) @out MoveOnly

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
