// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
let c{class B{struct S<T where I:a{class a{class A{struct c{enum b{class c:a}}class B{{}class A{struct Q{struct Q{class A{class a{{}struct S{enum b{struct S{struct S{let:a{b
