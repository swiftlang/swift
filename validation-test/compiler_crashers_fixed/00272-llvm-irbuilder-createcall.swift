// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck

// Issue found by https://github.com/fluidsonic (Marc Knaup)

class A {
    class func a() -> Self {
        return b(self.dynamicType) 
    }
}
func b<T>(t: AnyObject.Type) -> T! {
    return nil
}
