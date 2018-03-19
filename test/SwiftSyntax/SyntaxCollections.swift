// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest
import SwiftSyntax

var SyntaxCollectionsAPI = TestSuite("SyntaxCollectionsAPI")

SyntaxCollectionsAPI.test("AppendingElement") {
    let arrayElementList = SyntaxFactory.makeArrayElementList([
        SyntaxFactory.makeArrayElement(expression: SyntaxFactory.makeIntegerLiteral("0"), trailingComma: nil)
    ])
    
    let newArrayElementList = arrayElementList.appending(SyntaxFactory.makeIntegerLiteral("1"))
    
    expectEqual(newArrayElementList.count, 2)
    expectEqual("\(arrayElementList.child(at: 1)!)", "1")
}

SyntaxCollectionsAPI.test("InsertingElement") {
    let arrayElementList = SyntaxFactory.makeArrayElementList([
        SyntaxFactory.makeArrayElement(expression: SyntaxFactory.makeIntegerLiteral("1"), trailingComma: nil)
    ])
    
    var newArrayElementList = arrayElementList.inserting(SyntaxFactory.makeIntegerLiteral("0"), at: 0)
    
    expectEqual(newArrayElementList.count, 2)
    expectEqual("\(arrayElementList.child(at: 0)!)", "0")
    
    newArrayElementList = newArrayElementList.inserting(SyntaxFactory.makeIntegerLiteral("2"), at: 2)
    
    expectEqual(newArrayElementList.count, 3)
    expectEqual("\(arrayElementList.child(at: 2)!)", "2")
}

SyntaxCollectionsAPI.test("PrependingElement") {
    let arrayElementList = SyntaxFactory.makeArrayElementList([
        SyntaxFactory.makeArrayElement(expression: SyntaxFactory.makeIntegerLiteral("1"), trailingComma: nil)
    ])
    
    let newArrayElementList = arrayElementList.prepending(SyntaxFactory.makeIntegerLiteral("0"))
    
    expectEqual(newArrayElementList.count, 2)
    expectEqual("\(arrayElementList.child(at: 0)!)", "0")
}

SyntaxCollectionsAPI.test("RemovingFirstElement") {
    let arrayElementList = SyntaxFactory.makeArrayElementList([
        SyntaxFactory.makeArrayElement(expression: SyntaxFactory.makeIntegerLiteral("0"), trailingComma: nil),
        SyntaxFactory.makeArrayElement(expression: SyntaxFactory.makeIntegerLiteral("1"), trailingComma: nil)
    ])
    
    let newArrayElementList = arrayElementList.removingFirst()
    
    expectEqual(newArrayElementList.count, 1)
    expectEqual("\(arrayElementList.child(at: 0)!)", "1")
}

SyntaxCollectionsAPI.test("RemovingLastElement") {
    let arrayElementList = SyntaxFactory.makeArrayElementList([
        SyntaxFactory.makeArrayElement(expression: SyntaxFactory.makeIntegerLiteral("0"), trailingComma: nil),
        SyntaxFactory.makeArrayElement(expression: SyntaxFactory.makeIntegerLiteral("1"), trailingComma: nil)
    ])
    
    let newArrayElementList = arrayElementList.removingLast()
    
    expectEqual(newArrayElementList.count, 1)
    expectEqual("\(arrayElementList.child(at: 0)!)", "0")
}

SyntaxCollectionsAPI.test("RemovingElement") {
    let arrayElementList = SyntaxFactory.makeArrayElementList([
        SyntaxFactory.makeArrayElement(expression: SyntaxFactory.makeIntegerLiteral("0"), trailingComma: nil)
    ])
    
    let newArrayElementList = arrayElementList.removing(childAt: 0)
    
    expectEqual(newArrayElementList.count, 0)
    expectNil(newArrayElementList.child(at: 0))
}

SyntaxCollectionsAPI.test("ReplacingElement") {
    let arrayElementList = SyntaxFactory.makeArrayElementList([
        SyntaxFactory.makeArrayElement(expression: SyntaxFactory.makeIntegerLiteral("0"), trailingComma: nil),
        SyntaxFactory.makeArrayElement(expression: SyntaxFactory.makeIntegerLiteral("1"), trailingComma: nil),
        SyntaxFactory.makeArrayElement(expression: SyntaxFactory.makeIntegerLiteral("2"), trailingComma: nil)
    ])
    
    arrayElementList.replacing(childAt: 2,
                               with: SyntaxFactory.makeIntegerLiteral("3"))
    
    expectEqual("\(arrayElementList.child(at: 2)!)", "3")
}

runAllTests()
