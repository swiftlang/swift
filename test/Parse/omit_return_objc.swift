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

