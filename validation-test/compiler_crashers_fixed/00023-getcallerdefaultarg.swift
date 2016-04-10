// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -parse -verify

// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/18041799
// https://gist.github.com/stigi/336a9851cd80fdef22ed

func a(b: Int = 0) {
}
let c = a
c() // expected-error {{missing argument for parameter 'b' in call}}
