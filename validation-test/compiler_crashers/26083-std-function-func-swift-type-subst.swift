// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// DUPLICATE-OF: 01766-swift-typechecker-validatedecl.swift
// RUN: not --crash %target-swift-frontend %s -parse
protocol e:A{protocol e:A class A}protocol A{enum A}protocol B:e
