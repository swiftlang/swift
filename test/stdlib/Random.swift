//===--- Random.swift -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-typecheck-verify-swift

struct Jawn { }

// Me, sobbing: "Look, you can't just point at an empty struct and call it a
// RandomNumberGenerator."
// Swift 5.4, pointing at this Jawn: "RNG."
extension Jawn: RandomNumberGenerator { }
// expected-error@-1 {{type 'Jawn' does not conform to protocol 'RandomNumberGenerator'}}
// expected-error@-2 {{unavailable instance method 'next()' was used to satisfy a requirement of protocol 'RandomNumberGenerator'}}
