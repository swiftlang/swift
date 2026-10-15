// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -I %t/Inputs -cxx-interoperability-mode=default -verify -verify-ignore-unrelated %t/test.swift

// REQUIRES: objc_interop

//--- Inputs/header.h

#import <Foundation/Foundation.h>

struct TrivialCxxStruct {
    int x;
    int y;
};

class NonTrivialCxxStruct {
public:
    NonTrivialCxxStruct() : x(0), y(0) {}
    NonTrivialCxxStruct(int x, int y) : x(x), y(y) {}
    NonTrivialCxxStruct(const NonTrivialCxxStruct &other) : x(other.x), y(other.y) {}
    ~NonTrivialCxxStruct() {}

    int x;
    int y;
};

@interface BaseObjCClass : NSObject

- (TrivialCxxStruct)getTrivialStruct;
- (void)processTrivialStruct:(TrivialCxxStruct)s;
- (TrivialCxxStruct)transformTrivial:(TrivialCxxStruct)input;

- (NonTrivialCxxStruct)getNonTrivialStruct;
- (void)processNonTrivialStruct:(NonTrivialCxxStruct)s;
- (NonTrivialCxxStruct)transformNonTrivial:(NonTrivialCxxStruct)input;

@end

//--- Inputs/module.modulemap

module CxxObjCBase {
    header "header.h"
    requires cplusplus
    export *
}

//--- test.swift

import Foundation
import CxxObjCBase

// expected-no-diagnostics

class SwiftSubclass: BaseObjCClass {
    // Trivial C++ struct overrides should work.
    override func getTrivialStruct() -> TrivialCxxStruct {
        return TrivialCxxStruct(x: 42, y: 43)
    }

    override func processTrivialStruct(_ s: TrivialCxxStruct) {
    }

    override func transformTrivial(_ input: TrivialCxxStruct) -> TrivialCxxStruct {
        return TrivialCxxStruct(x: input.x + 1, y: input.y + 1)
    }

    // Non-trivial C++ struct overrides should also work when overriding
    // imported ObjC methods -- Clang already validated these types for ObjC++.
    override func getNonTrivialStruct() -> NonTrivialCxxStruct {
        return NonTrivialCxxStruct(0, 0)
    }

    override func processNonTrivialStruct(_ s: NonTrivialCxxStruct) {
    }

    override func transformNonTrivial(_ input: NonTrivialCxxStruct) -> NonTrivialCxxStruct {
        return input
    }
}
