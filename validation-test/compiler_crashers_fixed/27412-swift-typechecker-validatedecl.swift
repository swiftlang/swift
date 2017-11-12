// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
enum b{class b{struct a{struct B<S where f=A{class B<b{enum S{class A{}enum A{}struct A{enum S<T{{}struct B{var _=A<T b
