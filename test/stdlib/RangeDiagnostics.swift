//===--- RangeDiagnostics.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %swift -verify -parse %s

func assertCollection<C: CollectionType>(_: C) {}
assertCollection(0..<10)

// These error messages are not great, especially the second one.  If
// the test starts failing because the error message improves, update
// the test.

(10..<100)[0]           // expected-error {{could not find an overload for 'subscript' that accepts the supplied arguments}}
(UInt(10)..<100)[0..<4] // expected-error {{'Range<Pos>' is not convertible to 'UInt'}}

