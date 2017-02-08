//===--- FoundationBridge.m -----------------------------------------------===//
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

#import "FoundationBridge.h"
#import <objc/runtime.h>

@implementation ObjectBehaviorVerifier {
    NSMutableArray *_actions;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        _actions = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)dealloc {
    [_actions release];
    [super dealloc];
}

- (void)appendAction:(ObjectBehaviorAction)action {
    [_actions addObject:@(action)];
    switch (action) {
        case ObjectBehaviorActionRetain:
            _wasRetained = YES;
            break;
        case ObjectBehaviorActionMutableCopy:
            _wasMutableCopied = YES;
            // fall through
        case ObjectBehaviorActionCopy:
            _wasCopied = YES;
            break;
    }
}

- (void)enumerate:(void (^)(ObjectBehaviorAction))block {
    for (NSNumber *action in _actions) {
        block((ObjectBehaviorAction)action.integerValue);
    }
}

- (void)reset {
    [_actions removeAllObjects];
    _wasRetained = NO;
    _wasMutableCopied = NO;
    _wasCopied = NO;
}

- (void)dump {
    [self enumerate:^(ObjectBehaviorAction action) {
        switch (action) {
            case ObjectBehaviorActionRetain:
                printf("retain\n");
                break;
            case ObjectBehaviorActionMutableCopy:
                printf("mutableCopy\n");
                break;
            case ObjectBehaviorActionCopy:
                printf("copy\n");
                break;
        }
    }];
}

@end


@implementation ImmutableDataVerifier

- (instancetype)init {
    self = [super init];
    if (self) {
        _verifier = [[ObjectBehaviorVerifier alloc] init];
        char *bytes = "hello world";
        _data = [[NSData alloc] initWithBytes:bytes length:strlen(bytes)];
    }
    return self;
}

- (void)dealloc {
    [_verifier release];
    [_data release];
    [super dealloc];
}

- (id)retain {
    [_verifier appendAction:ObjectBehaviorActionRetain];
    return [super retain];
}

- (id)copyWithZone:(NSZone *)zone {
    [_verifier appendAction:ObjectBehaviorActionCopy];
    return [super retain];
}

- (id)mutableCopyWithZone:(NSZone *)zone {
    [_verifier appendAction:ObjectBehaviorActionMutableCopy];
    return [[MutableDataVerifier alloc] initWithData:_data];
}

- (NSUInteger)length {
    return _data.length;
}

- (const void *)bytes {
    return _data.bytes;
}

@end

@implementation MutableDataVerifier

- (instancetype)init {
    self = [super init];
    if (self) {
        _verifier = [[ObjectBehaviorVerifier alloc] init];
        char *bytes = "hello world";
        _data = [[NSMutableData alloc] initWithBytes:bytes length:strlen(bytes)];
    }
    return self;
}

- (instancetype)initWithData:(NSData *)data {
    self = [super init];
    if (self) {
        _verifier = [[ObjectBehaviorVerifier alloc] init];
        _data = [data mutableCopyWithZone:nil];
    }
    return self;
}

- (void)dealloc {
    [_verifier release];
    [_data release];
    [super dealloc];
}

- (id)retain {
    [_verifier appendAction:ObjectBehaviorActionRetain];
    return [super retain];
}

- (id)copyWithZone:(NSZone *)zone {
    [_verifier appendAction:ObjectBehaviorActionCopy];
    return [super retain];
}

- (id)mutableCopyWithZone:(NSZone *)zone {
    [_verifier appendAction:ObjectBehaviorActionMutableCopy];
    return [[MutableDataVerifier alloc] initWithData:_data];
}

- (NSUInteger)length {
    return _data.length;
}

- (void)setLength:(NSUInteger)length {
    _data.length = length;
}

- (const void *)bytes {
    return _data.bytes;
}

- (void *)mutableBytes {
    return _data.mutableBytes;
}

@end

void takesData(NSData *object) {
    // do NOTHING here...
}

NSData *returnsData() {
    static dispatch_once_t once = 0L;
    static NSData *data = nil;
    dispatch_once(&once, ^{
        char *bytes = "hello world";
        data = [[NSData alloc] initWithBytes:bytes length:strlen(bytes)];
    });
    return data;
}

BOOL identityOfData(NSData *data) {
    return data == returnsData();
}


@implementation CalendarBridgingTester

- (NSCalendar *)autoupdatingCurrentCalendar {
    return [NSCalendar autoupdatingCurrentCalendar];
}

- (BOOL)verifyAutoupdatingCalendar:(NSCalendar *)calendar {
    Class autoCalendarClass = (Class)objc_lookUpClass("_NSAutoCalendar");
    if (autoCalendarClass && [calendar isKindOfClass:autoCalendarClass]) {
        return YES;
    } else {
        autoCalendarClass = (Class)objc_lookUpClass("NSAutoCalendar");
        return [calendar isKindOfClass:autoCalendarClass];
    }
}

@end

@implementation TimeZoneBridgingTester

- (NSTimeZone *)autoupdatingCurrentTimeZone {
    return [NSTimeZone localTimeZone];
}

- (BOOL)verifyAutoupdatingTimeZone:(NSTimeZone *)tz {
    Class autoTimeZoneClass = (Class)objc_lookUpClass("__NSLocalTimeZone");
    return [tz isKindOfClass:autoTimeZoneClass];

}
@end

@implementation LocaleBridgingTester

- (NSLocale *)autoupdatingCurrentLocale {
    return [NSLocale autoupdatingCurrentLocale];
}

- (BOOL)verifyAutoupdatingLocale:(NSLocale *)locale {
    Class autoLocaleClass = (Class)objc_lookUpClass("NSAutoLocale");
    return [locale isKindOfClass:autoLocaleClass];
}

@end


