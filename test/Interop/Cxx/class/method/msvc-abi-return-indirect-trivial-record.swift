// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-emit-irgen -I %t/Inputs -enable-experimental-cxx-interop %t/test.swift -module-name Test | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-%target-cpu

// REQUIRES: OS=windows-msvc

//--- Inputs/module.modulemap
module MsvcUseVecIt {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h

#pragma once

class str {
    int x;
};

class It {
public:
    int x;

    bool operator ==(const It &other) const ;
    bool operator !=(const It &other) const ;
};

template<class T> class vec {
    int x;
public:
    vec(const vec<T> &);
    ~vec();

    It begin() const;
    It end() const;
};

using VecStr = vec<str>;

struct LoadableIntWrapper {
    int value;

    LoadableIntWrapper operator+=(LoadableIntWrapper rhs) {
        value += rhs.value;
        return *this;
    }
};

//--- test.swift

import MsvcUseVecIt

public func test(_ result: VecStr) -> CInt {
    let begin = result.__beginUnsafe()
    return begin.x
}

// CHECK: swiftcc i32 @"$s4Test4testys5Int32VSo0014vecstr_yuJCataVF"({{.*}} %[[RESULT:.*]])
// CHECK: call void @"?begin@?$vec@Vstr@@@@QEBA?AVIt@@XZ"(ptr %[[RESULT]], ptr {{.*}} sret{{.*}}

public func passTempForIndirectRetToVoidCall() {
    var lhs = LoadableIntWrapper(value: 2)
    let rhs = LoadableIntWrapper(value: 2)
    lhs += rhs
}

// CHECK: i32 @"$sSo18LoadableIntWrapperV2peoiyA2Bz_ABtFZ"(ptr
// CHECK: %[[OPRESULT:.*]] = alloca %TSo18LoadableIntWrapperV, align 4
// CHECK-x86_64: call void @"??YLoadableIntWrapper@@QEAA?AU0@U0@@Z"(ptr {{.*}}, ptr {{.*}} sret(%TSo18LoadableIntWrapperV) %[[OPRESULT]], i32
// CHECK-aarch64: call void @"??YLoadableIntWrapper@@QEAA?AU0@U0@@Z"(ptr {{.*}}, ptr {{.*}} sret(%TSo18LoadableIntWrapperV) %[[OPRESULT]], i64
