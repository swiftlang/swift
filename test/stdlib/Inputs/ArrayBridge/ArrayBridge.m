//===--- ArrayBridge.m - Array bridge/cast tests - the ObjC side ----------===//
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
//
//
//
//===----------------------------------------------------------------------===//
#include "ArrayBridge.h"
#include <stdio.h>

@interface Thunks : NSObject
- (id)createSubclass:(NSInteger)value;
- (id)acceptSubclassArray:(NSArray *)bridged expecting:(NSArray*)unbridged;
- (NSArray *)produceSubclassArray:(NSMutableArray *)expectations;
- (void)checkProducedSubclassArray:(NSArray *)produced expecting:(NSArray *)expected;
- (void)checkProducedBridgeableValueArray:(NSArray *)produced;
- (void)acceptBridgeableValueArray:(NSArray *)x;
- (NSArray *)produceBridgeableValueArray;
@end

id arrayAsID(NSArray* a) {
  return a;
}

NSArray* idAsArray(id a) {
  return a;
}

// Call back into thunks, passing arrays in both directions
void testSubclass(id thunks) {
  // Retrieve an array from Swift.
  NSMutableArray* expectations = [[NSMutableArray alloc] init];
  NSArray *fromObjCArr = [thunks produceSubclassArray:expectations];
  [thunks checkProducedSubclassArray:fromObjCArr expecting:expectations];

  // Send an array to swift.
  NSMutableArray *toObjCArr = [[NSMutableArray alloc] init];
  [toObjCArr addObject: [thunks createSubclass:10]];
  [toObjCArr addObject: [thunks createSubclass:11]];
  [toObjCArr addObject: [thunks createSubclass:12]];
  [toObjCArr addObject: [thunks createSubclass:13]];
  [toObjCArr addObject: [thunks createSubclass:14]];

  [thunks acceptSubclassArray: toObjCArr expecting: toObjCArr];
}

void testBridgeableValue(id thunks) {
  // Retrieve an array from Swift.
  NSArray *fromSwiftArr = [thunks produceBridgeableValueArray];
  [thunks checkProducedBridgeableValueArray: fromSwiftArr];

  // Send an array to swift.
  NSMutableArray *toSwiftArr = [[NSMutableArray alloc] init];
  [toSwiftArr addObject: [thunks createSubclass:10]];
  [toSwiftArr addObject: [thunks createSubclass:11]];
  [toSwiftArr addObject: [thunks createSubclass:12]];
  [toSwiftArr addObject: [thunks createSubclass:13]];
  [toSwiftArr addObject: [thunks createSubclass:14]];
  [thunks acceptBridgeableValueArray: toSwiftArr];
}

@implementation RDar27905230

+ (NSDictionary<NSString *, NSArray<id> *> *)mutableDictionaryOfMutableLists  {
    NSMutableArray *arr = [NSMutableArray array];
    [arr addObject:[NSNull null]];
    [arr addObject:@""];
    [arr addObject:@1];
    [arr addObject:@YES];
    [arr addObject:[NSValue valueWithRange:NSMakeRange(0, 1)]];
    [arr addObject:[NSDate dateWithTimeIntervalSince1970: 0]];
    return [NSMutableDictionary dictionaryWithObject:arr forKey:@"list"];
}

@end
