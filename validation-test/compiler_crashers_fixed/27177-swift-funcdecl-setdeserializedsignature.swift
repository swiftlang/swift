// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
let c{b<:{var a{{{{var b{protocol a{struct S{{}struct A{class B{struct S{class A{func a{class A{var:{let a{{enum a{let a{{enum S{class A{enum b{var b{{b=a
