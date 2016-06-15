// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// DUPLICATE-OF: 10659-swift-printingdiagnosticconsumer-handlediagnostic.timeout.swift
// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts
protocol A:A
protocol a:A
struct c:a{
let h=D
