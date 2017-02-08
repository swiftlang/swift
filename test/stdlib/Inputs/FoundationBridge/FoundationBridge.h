//===--- FoundationBridge.h -------------------------------------*- C++ -*-===//
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

typedef NS_ENUM(NSInteger, ObjectBehaviorAction) {
    ObjectBehaviorActionRetain,
    ObjectBehaviorActionCopy,
    ObjectBehaviorActionMutableCopy
};

// NOTE: this class is NOT meant to be used in threaded contexts.
@interface ObjectBehaviorVerifier : NSObject
@property (readonly) BOOL wasRetained;
@property (readonly) BOOL wasCopied;
@property (readonly) BOOL wasMutableCopied;

- (void)appendAction:(ObjectBehaviorAction)action;
- (void)enumerate:(void (^)(ObjectBehaviorAction))block;
- (void)reset;
- (void)dump;
@end

#pragma mark - NSData verification

@interface ImmutableDataVerifier : NSData {
    ObjectBehaviorVerifier *_verifier;
    NSData *_data;
}
@property (readonly) ObjectBehaviorVerifier *verifier;
@end

@interface MutableDataVerifier : NSMutableData {
    ObjectBehaviorVerifier *_verifier;
    NSMutableData *_data;
}
@property (readonly) ObjectBehaviorVerifier *verifier;
@end

void takesData(NSData *object);
NSData *returnsData();
BOOL identityOfData(NSData *data);

#pragma mark - NSCalendar verification

@interface CalendarBridgingTester : NSObject
- (NSCalendar *)autoupdatingCurrentCalendar;
- (BOOL)verifyAutoupdatingCalendar:(NSCalendar *)calendar;
@end

@interface TimeZoneBridgingTester : NSObject
- (NSTimeZone *)autoupdatingCurrentTimeZone;
- (BOOL)verifyAutoupdatingTimeZone:(NSTimeZone *)tz;
@end

@interface LocaleBridgingTester : NSObject
- (NSLocale *)autoupdatingCurrentLocale;
- (BOOL)verifyAutoupdatingLocale:(NSLocale *)locale;
@end

NS_ASSUME_NONNULL_END
