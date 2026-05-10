//===--- ArrayBridge.h ------------------------------------------*- C++ -*-===//
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
#import <Foundation/Foundation.h>

id arrayAsID(NSArray* a);
NSArray* idAsArray(id a);

void testSubclass(id thunks);
void testBridgeableValue(id thunks);
id testHKTFilter(id array);

@interface RDar27905230 : NSObject
+ (NSDictionary<NSString *, NSArray<id> *> *)mutableDictionaryOfMutableLists;
@end
