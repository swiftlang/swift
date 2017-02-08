// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// DUPLICATE-OF: 26832-swift-typechecker-conformstoprotocol.swift
// RUN: not %target-swift-frontend %s -typecheck
struct B<T{
class B<I:B{protocol C{struct B<T
var:B<T>
