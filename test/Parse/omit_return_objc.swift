// RUN: %target-swift-frontend %s -typecheck -verify

// REQUIRES: objc_interop

import Foundation

class DynamicSubscriptClass {
    @objc subscript (i : Int) -> String { "howdy" }
}

func ff_implicitDynamicSubscript(_ c: DynamicSubscriptClass) -> String {
    c[13]
}

func ff_implicitObjcSelectorExpr() -> Selector {
    #selector(NSArray.object(at:))
}

func ff_implicitKeyPathExpr() -> String {
    #keyPath(NSArray.count)
}

class SomeClass {}

func ff_implicitClassMetatypeToAnyObjectExpr() -> AnyObject {
    SomeClass.self
}

class DynamicSubscriptClass_ifdecl {
    #if true
    @objc subscript (i : Int) -> String { "howdy" }
    #endif
}

func ff_implicitDynamicSubscript_ifdecl(_ c: DynamicSubscriptClass) -> String {
    #if true
    c[13]
    #endif
}

func ff_implicitObjcSelectorExpr_ifdecl() -> Selector {
    #if true
    #selector(NSArray.object(at:))
    #endif
}

func ff_implicitKeyPathExpr_ifdecl() -> String {
    #if true
    #keyPath(NSArray.count)
    #endif
}

func ff_implicitClassMetatypeToAnyObjectExpr_ifdecl() -> AnyObject {
    #if true
    SomeClass.self
    #endif
}
