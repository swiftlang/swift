// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
let a{struct S<a {
struct Q<b{S<b
struct B{
struct b{class b{class A B TA{
}
struct Q<T where h:A{struct {
}struct S<T where h:B:{class B:A{{
b
