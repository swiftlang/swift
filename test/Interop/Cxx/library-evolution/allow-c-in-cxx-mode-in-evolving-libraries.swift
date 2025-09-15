// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/test.swift -I %t/Inputs -typecheck -enable-library-evolution -enable-experimental-cxx-interop -verify

// REQUIRES: rdar144000451

//--- Inputs/module.modulemap
module CxxModule {
    header "header.h"
    requires cplusplus
}

//--- Inputs/header.h

class CxxStruct {
public:
    int x; int y;

    void method() const;
};

enum CEnum {
    a, b
};

enum class CxxEnum {
    aa, bb
};

#if defined(__cplusplus)
extern "C" {
#endif

struct CStruct {
    int x; int y;
};

#if defined(__cplusplus)
}
#endif

typedef void (*my_sighandler_t)(int);
typedef void (*my_other_cb)(CxxStruct);

//--- test.swift

import CxxModule

public func useCStruct(_ x: CStruct) {
}

public func useCEnum(_ x: CEnum) {
}

public func useCxxEnum(_ x: CxxEnum) { // expected-error {{cannot use enum 'CxxEnum' here; C++ types from imported module 'CxxModule' do not support library evolution}}
}

// expected-error@+1 {{cannot use struct 'CxxStruct' here; C++ types from imported module 'CxxModule' do not support library evolution}}
public func usesCxxStruct(_ x: CxxStruct) {
}

public func usesTypeAliasToFuncPtr(_ x: my_sighandler_t) {
}

public func usesTypeAliasToFuncPtr2(_ x: my_other_cb) { // expected-error {{cannot use type alias 'my_other_cb' here; C++ types from imported module 'CxxModule' do not support library evolution}}
}
