// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/test.swift -I %t/Inputs -typecheck -enable-library-evolution -enable-experimental-cxx-interop -verify

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

#if defined(__cplusplus)
extern "C" {
#endif

struct CStruct {
    int x; int y;
};

#if defined(__cplusplus)
}
#endif

//--- test.swift

import CxxModule

public func useCStruct(_ x: CStruct) {
}

// expected-error@+1 {{cannot use struct 'CxxStruct' here; C++ types from imported module 'CxxModule' do not support library evolution}}
public func usesCxxStruct(_ x: CxxStruct) {
}
