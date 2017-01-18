//===--- ObjectiveCTests.m ------------------------------------------------===//
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

  id cal = [NSCalendar currentCalendar];
  myBeginDate = [cal dateWithEra:1
                            year:2016
                           month:1
                             day:29
                            hour:1
                          minute:1
                          second:0
                      nanosecond:0];
  myEndDate = [cal dateWithEra:1
                          year:2016
                         month:1
                           day:29
                          hour:1
                        minute:1
                        second:0
                    nanosecond:10];

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

- (NSDate *)beginDate {
  return myBeginDate;
}

- (NSDate *)endDate {
  return myEndDate;
}

- (void)useDate:(NSDate *)date {
  if ([date isEqualToDate:myBeginDate]) {
    assert(false && "Dates should be different");
  }
}

@end
