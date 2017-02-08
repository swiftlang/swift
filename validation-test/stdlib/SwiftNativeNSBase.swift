//===--- SwiftNativeNSBase.swift - Test _SwiftNativeNS*Base classes -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: rm -rf %t  &&  mkdir -p %t
// 
// RUN: %target-clang %S/Inputs/SwiftNativeNSBase/SwiftNativeNSBase.m -c -o %t/SwiftNativeNSBase.o -g
// RUN: %target-build-swift %s -I %S/Inputs/SwiftNativeNSBase/ -Xlinker %t/SwiftNativeNSBase.o -o %t/SwiftNativeNSBase
// RUN: %target-run %t/SwiftNativeNSBase
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

@_silgen_name("TestSwiftNativeNSBase") 
func TestSwiftNativeNSBase()

TestSwiftNativeNSBase()
// does not return
