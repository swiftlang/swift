// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// DUPLICATE-OF: 01766-swift-typechecker-validatedecl.swift
// RUN: not %target-swift-frontend %s -typecheck
protocol e:A{protocol e:A class A}protocol A{enum A}protocol B:e
