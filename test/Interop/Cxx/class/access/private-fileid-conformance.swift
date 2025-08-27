// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -I %t/Cxx/include %t/PrivateFile.swift -cxx-interoperability-mode=default -module-name Module
// RUN: %target-swift-frontend -typecheck -verify -I %t/Cxx/include %t/HasRetroactive.swift -cxx-interoperability-mode=default -module-name Module
// RUN: %target-swift-frontend -typecheck -verify -I %t/Cxx/include %t/NoRetroactive.swift -cxx-interoperability-mode=default -module-name Module

//--- Cxx/include/module.modulemap
module CxxModule {
    requires cplusplus
    header "cxx-header.h"
}

//--- Cxx/include/cxx-header.h
#pragma once

#define SWIFT_PRIVATE_FILEID(_fileID) \
  __attribute__((swift_attr("private_fileid:" _fileID)))

class SWIFT_PRIVATE_FILEID("Module/PrivateFile.swift") Foo { void privateMethod(void) const {} };

//--- PrivateFile.swift
import CxxModule

extension Foo : ExpressibleByIntegerLiteral {
    public typealias IntegerLiteralType = Int
    public init(integerLiteral: Self.IntegerLiteralType) {
        self.init()
        self.privateMethod()
    }
}

//--- HasRetroactive.swift
import CxxModule

extension Foo : @retroactive ExpressibleByIntegerLiteral {
    public typealias IntegerLiteralType = Int
    public init(integerLiteral: Self.IntegerLiteralType) {
        self.init()
        self.privateMethod() // expected-error {{'privateMethod' is inaccessible due to 'private' protection level}}
    }
}

//--- NoRetroactive.swift
import CxxModule

// expected-warning@+2 {{extension declares a conformance of imported type 'Foo' to imported protocol 'ExpressibleByIntegerLiteral'}}
// expected-note@+1 {{add '@retroactive' to silence this warning}}
extension Foo : ExpressibleByIntegerLiteral {
    public typealias IntegerLiteralType = Int
    public init(integerLiteral: Self.IntegerLiteralType) {
        self.init()
        self.privateMethod() // expected-error {{'privateMethod' is inaccessible due to 'private' protection level}}
    }
}
