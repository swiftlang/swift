// RUN: %empty-directory(%t)
// RUN: split-file %s %t


// RUN: %target-build-swift %t/struct.swift -emit-module -emit-library -static -module-name OpaqueStruct -emit-module-path %t/Inputs/OpaqueStruct.swiftmodule -enable-library-evolution

// RUN: %target-swift-emit-irgen %t/test.swift -I %t/Inputs -enable-experimental-cxx-interop -disable-availability-checking | %FileCheck %s
 
//--- Inputs/module.modulemap
module CxxModule {
    header "header.h"
    requires cplusplus
}

//--- Inputs/header.h

class
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
SingletonReference {
public:
    SingletonReference(const SingletonReference &) = delete;

    void method();
};

class
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainS")))
__attribute__((swift_attr("release:releaseS")))
SingletonReferenceRef {
public:
    SingletonReferenceRef(const SingletonReferenceRef &) = delete;

    void method();
};

void retainS(SingletonReferenceRef *);
void releaseS(SingletonReferenceRef *);

//--- struct.swift

public struct ResilientStruct {
    let x: Int
}

//--- test.swift

import OpaqueStruct
import CxxModule

public struct ImmortalFRT_OpaqueLayout {
    public let y: SingletonReference
    public let x: ResilientStruct
}

public struct SharedFRT_OpaqueLayout {
    public let x: ResilientStruct
    public let y: SingletonReferenceRef
}

// CHECK: define{{.*}} @"$s4test24ImmortalFRT_OpaqueLayoutVMr"
// CHECK: store ptr getelementptr inbounds (ptr, ptr @"$sBpWV", i32

// CHECK: define{{.*}} @"$s4test22SharedFRT_OpaqueLayoutVMr"
// CHECK: store ptr getelementptr inbounds (ptr, ptr @"$sBpWV", i32
