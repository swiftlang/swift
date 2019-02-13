// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// This test no longer crashes the compiler, but it does produce an AST that
// the verifier doesn't like.
// REQUIRES: swift_ast_verifier

// RUN: not --crash %target-swift-frontend %s -emit-ir
switch{case.b(u){
