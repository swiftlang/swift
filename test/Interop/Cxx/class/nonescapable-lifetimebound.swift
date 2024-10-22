// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -I %swift_src_root/lib/ClangImporter/SwiftBridging  -I %t/Inputs -emit-sil %t/test.swift -enable-experimental-feature NonescapableTypes -enable-experimental-feature AllowUnsafeAttribute  -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1 | %FileCheck %s

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

SWIFT_IMPORT_AS_UNSAFE
View getViewPartiallyAnnotated(const Owner& owner [[clang::lifetimebound]], const Owner& owner2) {
    if (coinFlip)
        return View(&owner.data);
    else
        return View(&owner2.data);
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
// CHECK: sil [clang getViewPartiallyAnnotated] {{.*}} : $@convention(c) (@lifetime(immortal) @in_guaranteed Owner, @in_guaranteed Owner) -> @autoreleased View

//--- test.swift

import Test

public func test() {
    let o = makeOwner()
    let o2 = makeOwner()
    let v1 = getView(o)
    let v2 = getViewFromFirst(o, o2)
    let _ = getViewFromEither(o, o2)
    let _ = o.handOutView()
    let _ = o.handOutView2(v1)
    let _ = getViewFromEither(v1, v2)
    let defaultView = View()
    let _ = OtherView(defaultView)
    let _ = getViewPartiallyAnnotated(o, o2)
}
