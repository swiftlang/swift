//===--- ObjectiveCTests.h ------------------------------------------------===//
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

NS_ASSUME_NONNULL_BEGIN

@interface BridgeTester : NSObject {
  NSString *myString;
  NSArray<NSString *> *myArrayOfStrings;
  NSDate *myBeginDate;
  NSDate *myEndDate;
  NSArray<NSString *> *cornucopiaOfStrings;
  NSArray<NSString *> *bridgedStrings;
}

- (id)init;
- (void)setUpStringTests:(NSArray<NSString *> *)bridgedStrings;
- (void)testFromString:(NSString *) str;
- (NSString *)testToString;
- (void)testFromArrayOfStrings:(NSArray<NSString *> *)arr;
- (NSArray<NSString *> *)testToArrayOfStrings;

- (NSDate *)beginDate;
- (NSDate *)endDate;
- (void)useDate:(NSDate *)date;

- (void)testIsEqualToString;
- (void)testIsEqualToString2;
- (void)testIsEqualToStringAllSwift;
- (void)testUTF8String;
- (void)testCStringUsingEncoding;
- (void)testGetUTF8Contents;
- (void)testGetASCIIContents;
- (void)testRangeOfString;
- (void)testHash;
- (void)testCompare;
- (void)testCompare2;

@end

NS_ASSUME_NONNULL_END
