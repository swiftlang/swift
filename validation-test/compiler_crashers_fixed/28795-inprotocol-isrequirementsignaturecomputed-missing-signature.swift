// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// Disabled during NoncopyableGenerics infrastructure enablement (rdar://124627845)
// XFAIL: *

// RUN: not %target-swift-frontend %s -emit-ir
protocol A:A.b{func 丏
protocol b:Collection
