// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/test.swift -I %t/Inputs -typecheck -enable-library-evolution -enable-experimental-cxx-interop -disable-availability-checking -disable-implicit-cxx-module-import -enable-experimental-feature AssumeResilientCxxTypes -verify

// REQUIRES: swift_feature_AssumeResilientCxxTypes
 
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

enum class CxxEnum {
  A, B
};

template<class T>
class CxxTemplate {
    T v;
};

using CxxTemplateInt = CxxTemplate<int>;

class
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
SingletonReference {
public:
    SingletonReference(const SingletonReference &) = delete;
    
    static SingletonReference * _Nonnull create();

    void method();
};

CxxStruct createStruct();

void freeCxxFunction();

using BuiltinIntTypealis = int;

//--- test.swift

import CxxModule

public func usesBuiltinIntTypealis() -> BuiltinIntTypealis {
    return 21
}

public func usesCxxSingletonReference() -> SingletonReference {
    return SingletonReference.create()
}

public func usesCxxStruct(_ x: CxxStruct) {
}

public typealias EnumT = CxxEnum

extension CxxTemplateInt {
    func testInternal() {
        
    }
}

extension CxxTemplateInt {
    public func testPublicExt() {
    }
}

public func publicFuncInternalBody() {
    let s = createStruct()
    s.method()
}

@inlinable
public func publicFuncPublicBody() {
    let value = SingletonReference.create()
    value.method()
}
