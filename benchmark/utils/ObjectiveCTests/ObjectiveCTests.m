//===--- ObjectiveCTests.m ------------------------------------------------===//
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

#import "ObjectiveCTests.h"

@implementation BridgeTester

- (id)init {
  self = [super init];
  if (!self)
    return self;
  myString = @"Default string value no tagged pointer";
  id mutableArray = [NSMutableArray new];
  for (int i = 0; i < 10; ++i) {
    [mutableArray addObject: myString];
  }
  myArrayOfStrings = [mutableArray copy];
  return self;
}

- (NSString *)testToString {
  return myString;
}

- (void)testFromString:(NSString *)str {
  unichar c = [str characterAtIndex:0];
}
- (void)testFromArrayOfStrings:(NSArray<NSString *> *)arr {
  // Get an element to force lazy bridging to happen.
  id str = [arr objectAtIndex:0];
}

- (NSArray<NSString *> *)testToArrayOfStrings {
  return myArrayOfStrings;
}


@end
