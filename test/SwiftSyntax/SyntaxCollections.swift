// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest
import SwiftSyntax

func integerLiteralElement(_ int: Int) -> ArrayElementSyntax {
    let literal = SyntaxFactory.makeIntegerLiteral("\(int)")
    return SyntaxFactory.makeArrayElement(
        expression: SyntaxFactory.makeIntegerLiteralExpr(digits: literal),
        trailingComma: nil)
}

var SyntaxCollectionsAPI = TestSuite("SyntaxCollectionsAPI")

SyntaxCollectionsAPI.test("AppendingElement") {
    let arrayElementList = SyntaxFactory.makeArrayElementList([
        integerLiteralElement(0)
    ])
    
    let newArrayElementList = arrayElementList.appending(integerLiteralElement(1))
    
    expectEqual(newArrayElementList.count, 2)
    expectNotNil(newArrayElementList.child(at: 1))
    expectEqual("\(newArrayElementList.child(at: 1)!)", "1")
}

SyntaxCollectionsAPI.test("InsertingElement") {
    let arrayElementList = SyntaxFactory.makeArrayElementList([
        integerLiteralElement(1)
    ])
    
    var newArrayElementList = arrayElementList.inserting(integerLiteralElement(0), at: 0)
    
    expectEqual(newArrayElementList.count, 2)
    expectNotNil(newArrayElementList.child(at: 0))
    expectEqual("\(newArrayElementList.child(at: 0)!)", "0")
    
    newArrayElementList = newArrayElementList.inserting(integerLiteralElement(2), at: 2)
    
    expectEqual(newArrayElementList.count, 3)
    expectNotNil(newArrayElementList.child(at: 2))
    expectEqual("\(newArrayElementList.child(at: 2)!)", "2")
}

SyntaxCollectionsAPI.test("PrependingElement") {
    let arrayElementList = SyntaxFactory.makeArrayElementList([
        integerLiteralElement(1)
    ])
    
    let newArrayElementList = arrayElementList.prepending(integerLiteralElement(0))
    
    expectEqual(newArrayElementList.count, 2)
    expectNotNil(newArrayElementList.child(at: 0))
    expectEqual("\(newArrayElementList.child(at: 0)!)", "0")
}

SyntaxCollectionsAPI.test("RemovingFirstElement") {
    let arrayElementList = SyntaxFactory.makeArrayElementList([
        integerLiteralElement(0),
        integerLiteralElement(1)
    ])
    
    let newArrayElementList = arrayElementList.removingFirst()
    
    expectEqual(newArrayElementList.count, 1)
    expectNotNil(newArrayElementList.child(at: 0))
    expectEqual("\(newArrayElementList.child(at: 0)!)", "1")
}

SyntaxCollectionsAPI.test("RemovingLastElement") {
    let arrayElementList = SyntaxFactory.makeArrayElementList([
        integerLiteralElement(0),
        integerLiteralElement(1)
    ])
    
    let newArrayElementList = arrayElementList.removingLast()
    
    expectEqual(newArrayElementList.count, 1)
    expectNotNil(newArrayElementList.child(at: 0))
    expectEqual("\(newArrayElementList.child(at: 0)!)", "0")
}

SyntaxCollectionsAPI.test("RemovingElement") {
    let arrayElementList = SyntaxFactory.makeArrayElementList([
        integerLiteralElement(0)
    ])
    
    let newArrayElementList = arrayElementList.removing(childAt: 0)
    
    expectEqual(newArrayElementList.count, 0)
    expectNil(newArrayElementList.child(at: 0))
}

SyntaxCollectionsAPI.test("ReplacingElement") {
    let arrayElementList = SyntaxFactory.makeArrayElementList([
        integerLiteralElement(0),
        integerLiteralElement(1),
        integerLiteralElement(2)
    ])
    
    let newArrayElementList = arrayElementList.replacing(childAt: 2,
                                                         with: integerLiteralElement(3))
    
    expectNotNil(newArrayElementList.child(at: 2))
    expectEqual("\(newArrayElementList.child(at: 2)!)", "3")
}

runAllTests()
