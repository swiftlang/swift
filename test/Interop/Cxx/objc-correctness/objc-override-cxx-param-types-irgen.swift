// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -I %t/Inputs -import-objc-header %t/Inputs/header.h -cxx-interoperability-mode=default -emit-ir -o - | %FileCheck %s

// REQUIRES: objc_interop

//--- Inputs/header.h

#import <Foundation/Foundation.h>

class CxxRecord {
public:
    CxxRecord() : value(0) {}
    CxxRecord(int v) : value(v) {}
    CxxRecord(const CxxRecord &other) : value(other.value) {}
    ~CxxRecord() {}

    int value;
};

@interface ObjCBase : NSObject
- (CxxRecord)getRecord;
- (void)processRecord:(CxxRecord)record;
@end

//--- Inputs/module.modulemap

module CxxObjCTypes {
    header "header.h"
    requires cplusplus
    export *
}

//--- test.swift

import Foundation
import CxxObjCTypes

class SwiftSub: ObjCBase {
    override func getRecord() -> CxxRecord {
        return CxxRecord(42)
    }

    override func processRecord(_ record: CxxRecord) {
    }
}

// The @objc thunk for getRecord returns via sret (indirect return for non-trivial C++ type).
// CHECK: define internal void @"$s4test8SwiftSubC9getRecordSo03CxxE0VyFTo"(ptr noalias sret({{.*}}) %0, ptr %1, ptr %2)

// The @objc thunk for processRecord takes the C++ record as an indirect parameter.
// CHECK: define internal void @"$s4test8SwiftSubC13processRecordyySo03CxxE0VFTo"(ptr %0, ptr %1, ptr %2)
