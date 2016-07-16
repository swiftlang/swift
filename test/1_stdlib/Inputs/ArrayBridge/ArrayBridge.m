//===--- ArrayBridge.m - Array bridge/cast tests - the ObjC side ----------===//
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
//
//  
//
//===----------------------------------------------------------------------===//
#include "ArrayBridge.h"
#include <stdio.h>

@interface Thunks : NSObject
- (id)createDerived:(NSInteger)value;
- (id)acceptDerivedArray:(NSArray *)bridged expecting:(NSArray*)unbridged;
- (NSArray *)produceDerivedArray:(NSMutableArray *)expectations;
- (void)checkProducedDerivedArray:(NSArray *)produced expecting:(NSArray *)expected;
- (id)acceptBridgedSwiftArray:(NSArray *)x;
- (NSArray *)produceBridgedSwiftArray:(NSInteger)numItems;
@end

id arrayAsID(NSArray* a) {
  return a;
}

NSArray* idAsArray(id a) {
  return a;
}

// Call back into thunks, passing arrays in both directions
void testDerived(id thunks) {
  // Retrieve an array from Swift.
  NSMutableArray* expectations = [[NSMutableArray alloc] init];
  NSArray *fromObjCArr = [thunks produceDerivedArray:expectations];
  [thunks checkProducedDerivedArray:fromObjCArr expecting:expectations];

  // Send an array to swift.
  NSMutableArray *toObjCArr = [[NSMutableArray alloc] init];
  [toObjCArr addObject: [thunks createDerived:10]];
  [toObjCArr addObject: [thunks createDerived:11]];
  [toObjCArr addObject: [thunks createDerived:12]];
  [toObjCArr addObject: [thunks createDerived:13]];
  [toObjCArr addObject: [thunks createDerived:14]];
  
  [thunks acceptDerivedArray: toObjCArr expecting: toObjCArr];
}

void testBridgedSwift(id thunks) {
  // Retrieve an array from Swift.
  NSArray *fromSwiftArr = [thunks produceBridgedSwiftArray: 5];
  printf("%d elements in the array\n", (int)fromSwiftArr.count);

  for (id obj in fromSwiftArr) {
    printf("%s\n", [obj description].UTF8String);
  }

  // Send an array to swift.
  NSMutableArray *toSwiftArr = [[NSMutableArray alloc] init];
  [toSwiftArr addObject: [thunks createDerived:10]];
  [toSwiftArr addObject: [thunks createDerived:11]];
  [toSwiftArr addObject: [thunks createDerived:12]];
  [toSwiftArr addObject: [thunks createDerived:13]];
  [toSwiftArr addObject: [thunks createDerived:14]];
  [thunks acceptBridgedSwiftArray: toSwiftArr];
}
