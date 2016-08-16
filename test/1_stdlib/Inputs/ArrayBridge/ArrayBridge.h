//===--- ArrayBridge.h ------------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
#import <Foundation/Foundation.h>

id arrayAsID(NSArray* a);
NSArray* idAsArray(id a);

void testSubclass(id thunks);
void testBridgeableValue(id thunks);
