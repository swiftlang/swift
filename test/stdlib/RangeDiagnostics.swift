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

func assertCollection<C: Collection>(_: C) {}
assertCollection(0..<10)

(10..<100)[0] // expected-error {{'Int' is not convertible to 'Range<Pos>'}}

#if SWIFT_DISABLE_INT_RANGE_SLICE
func assertSliceable<C: Sliceable>(_: C) {}
assertSliceable(UInt(0)..<10)

(UInt(10)..<100)[0..<4] // expected -error {{}}
#endif
