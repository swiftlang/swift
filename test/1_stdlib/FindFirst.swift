//===--- FindFirst.swift - tests for finding first element that matches the predicate --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

let FindFirstTests = TestSuite("FindFirst")

FindFirstTests.test("find first in integers collections") {
    expectEqual([30 ,10, 90].findFirst({ $0 % 3 == 0 }) , 30)
}

FindFirstTests.test("find first in integers collections - finds nothing") {
    expectEqual([33 ,11, 97].findFirst({ $0 % 2 == 0 }) , nil)
}

FindFirstTests.test("find first in String collections") {
    expectEqual(["apple", "iOS", "mac"].findFirst({ $0 == "iOS" }), "iOS")
}

FindFirstTests.test("find first in String collections") {
    expectEqual(["apple", "iOS", "mac"].findFirst({ $0 == "iPhone" }), nil)
}

FindFirstTests.test("find rawvalue in enum which satisfies the predicate") {
    enum MockMenu: String {
        case MenuItem1 = "MenuItem1"
        case MenuItem2 = "MenuItem2"
        case MenuItem3 = "MenuItem3"
        case MenuItem4 = "MenuItem4"
        case MenuItem5 = "MenuItem5"
        
        static func allValues() -> [MockMenu] {
            return [.MenuItem1, .MenuItem2, .MenuItem3, .MenuItem4, .MenuItem5]
        }
    }
    
    func getMenuItemDisplayName(menuItem: MockMenu) -> String {
        return MockMenu.allValues().findFirst({ $0 == menuItem })?.rawValue ?? ""
    }
    
    expectEqual(getMenuItemDisplayName(.MenuItem1), "MenuItem1")
    expectEqual(getMenuItemDisplayName(.MenuItem2), "MenuItem2")
}

runAllTests()
