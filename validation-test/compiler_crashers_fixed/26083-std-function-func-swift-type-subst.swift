// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// DUPLICATE-OF: 01766-swift-typechecker-validatedecl.swift
// RUN: not %target-swift-frontend %s -parse
protocol e:A{protocol e:A class A}protocol A{enum A}protocol B:e
