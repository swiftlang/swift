// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
let a{struct S<a {
struct Q<b{S<b
struct B{
struct b{class b{class A B TA{
}
struct Q<T where h:A{struct {
}struct S<T where h:B:{class B:A{{
b
