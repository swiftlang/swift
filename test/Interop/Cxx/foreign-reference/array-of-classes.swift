// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swiftc_driver -O -I %t/Inputs  %t/test.swift -cxx-interoperability-mode=default -I %swift_src_root/lib/ClangImporter/SwiftBridging -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: executable_test
// UNSUPPORTED: back_deployment_runtime

// Metadata for foreign reference types is not supported on Windows.
// UNSUPPORTED: OS=windows-msvc

//--- Inputs/module.modulemap

module Test {
    header "reftype.h"
    requires cplusplus
}

//--- Inputs/reftype.h

#include "swift/bridging"

class RefType {
public:
    static RefType* _Nonnull makeRefType() SWIFT_RETURNS_RETAINED {
        return new RefType();
    }

private:
    RefType() : _refCount(1) {
    }
    RefType(const RefType&) = delete;
    RefType& operator=(const RefType&) = delete;
    ~RefType() {
    }
    
    inline friend void retainRefType(RefType* _Nonnull x) {
        x->_refCount += 1;

    }
    inline friend void releaseRefType(RefType* _Nonnull x) {
        x->_refCount -= 1;
        if (x->_refCount == 0) {
            delete x;
        }
    }
    
    int _refCount;
} SWIFT_SHARED_REFERENCE(retainRefType, releaseRefType);

//--- test.swift

import Test

@inline(never)
func go() {
    let x: RefType = RefType.makeRefType()
    var y: [RefType] = []
    y.append(x)
// CHECK: 1
    print(y.count)
}

go()

