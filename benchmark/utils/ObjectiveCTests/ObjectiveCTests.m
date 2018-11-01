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
#import <objc/runtime.h>

@interface CustomString : NSString {
  unichar *_backing;
  NSUInteger _length;
}

@end

@implementation CustomString

- (instancetype)initWithCharacters:(const unichar *)characters length:(NSUInteger)length {
  self = [super init];
  _backing = malloc(sizeof(unichar) * length);
  memcpy(_backing, characters, length * sizeof(unichar));
  _length = length;
  return self;
}

- (void)dealloc {
  free(_backing);
}

- (unichar)characterAtIndex:(NSUInteger)index {
  return _backing[index];
}

- (NSUInteger)length {
  return _length;
}

@end

@interface CustomFastASCIIString : NSString {
  char *_backing;
  NSUInteger _length;
}

@end

@implementation CustomFastASCIIString

- (instancetype)initWithCString:(const char *)bytes length:(NSUInteger)length {
  self = [super init];
  _backing = strdup(bytes);
  _length = length;
  return self;
}

- (void)dealloc {
  free(_backing);
}

- (NSStringEncoding)fastestEncoding {
  return NSASCIIStringEncoding;
}

- (NSUInteger)length {
  return _length;
}

- (unichar)characterAtIndex:(NSUInteger)index {
  return _backing[index];
}

- (const char *)_fastCStringContents:(BOOL)nullTerminationRequired {
  return _backing;
}

@end

@interface CustomFastUnicodeString : CustomString

@end

@implementation CustomFastUnicodeString

- (NSStringEncoding)fastestEncoding {
  return NSUnicodeStringEncoding;
}

- (const unichar *)_fastCharacterContents {
  return _backing;
}

@end

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

- (void)setUpStringTests:(NSArray<NSString *> *)inBridgedStrings {
  
  const char *taggedContents = "hello";
  const char *tagged2Contents = "hella";
  const char *notTaggedContents = "the quick brown fox jumps over the lazy dog";
  const char *notTagged2Contents = "the quick brown fox jumps over the lazy dogabc";
  const char *nonASCIIContents = "the quick brown fox jümps over the lazy dog";
  const char *nonASCII2Contents = "the quick brown fox jumps over the lazy dög";
  const char *longNonASCIIContents = "the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy dïgz";
  const char *longASCIIContents = "the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy the quick brown fox jumps over the lazy dogz";
  
  NSString *tagged = [NSString stringWithUTF8String:taggedContents];
  NSString *tagged2 = [NSString stringWithUTF8String:tagged2Contents];
  NSString *notTagged = [NSString stringWithUTF8String:notTaggedContents];
  NSString *notTagged2 = [NSString stringWithUTF8String:notTaggedContents];
  NSString *notTaggedLonger = [NSString stringWithUTF8String:notTagged2Contents];
  NSString *nonASCII = [NSString stringWithUTF8String:nonASCIIContents];
  NSString *nonASCII2 = [NSString stringWithUTF8String:nonASCIIContents];
  NSString *nonASCIIOther = [NSString stringWithUTF8String:nonASCII2Contents];
  NSString *longNonASCII = [NSString stringWithUTF8String:longNonASCIIContents];
  NSString *noCopyLongNonASCII = [[NSString alloc] initWithBytesNoCopy:(void *)longNonASCIIContents length:strlen(longNonASCIIContents) encoding:NSUTF8StringEncoding freeWhenDone:NO];
  NSString *noCopyLongASCII = [[NSString alloc] initWithBytesNoCopy:(void *)longASCIIContents length:strlen(longASCIIContents) encoding:NSUTF8StringEncoding freeWhenDone:NO];
  NSString *constantTaggable = @"hello";
  NSString *constantASCII = @"the quick brown fox jumps over the lazy dog";
  NSString *constantNonASCII = @"the quick brown fox jümps over the lazy dog";
  
  unichar taggableUnichars[] = {
    'h', 'e', 'l', 'l', 'o'
  };
  unichar nonTaggableUnichars[] = {
    't', 'h', 'e',  'q', 'u', 'i', 'c', 'k',  'b', 'r', 'o', 'w', 'n',  'f', 'o', 'x',  'j', 'u', 'm', 'p', 's',  'o', 'v', 'e', 'r',  't', 'h', 'e',  'l', 'a', 'z', 'y',  'd', 'o', 'g'
  };
  NSString *taggableCustom = [[CustomString alloc] initWithCharacters:&taggableUnichars[0] length:sizeof(taggableUnichars) / sizeof(taggableUnichars[0])];
  NSString *nonTaggableCustom = [[CustomString alloc] initWithCharacters:&nonTaggableUnichars[0] length:sizeof(nonTaggableUnichars) / sizeof(nonTaggableUnichars[0])];
  NSString *taggableFastCustom = [[CustomFastASCIIString alloc] initWithCString:taggedContents length:strlen(taggedContents)];
  NSString *nonTaggableFastCustom = [[CustomFastASCIIString alloc] initWithCString:notTaggedContents length:strlen(notTaggedContents)];
  NSString *taggableUnicodeCustom = [[CustomFastUnicodeString alloc] initWithCharacters:&taggableUnichars[0] length:sizeof(taggableUnichars) / sizeof(taggableUnichars[0])];
  NSString *nonTaggableUnicodeCustom = [[CustomFastUnicodeString alloc] initWithCharacters:&nonTaggableUnichars[0] length:sizeof(nonTaggableUnichars) / sizeof(nonTaggableUnichars[0])];
  
  cornucopiaOfStrings = [NSArray arrayWithObjects: tagged, tagged2, notTagged, notTagged2, notTaggedLonger, nonASCII, nonASCII2, nonASCIIOther, longNonASCII, noCopyLongASCII, noCopyLongNonASCII, constantASCII, constantNonASCII
                         , taggableCustom, nonTaggableCustom, taggableFastCustom, nonTaggableFastCustom, taggableUnicodeCustom, nonTaggableUnicodeCustom, nil];
  bridgedStrings = inBridgedStrings;
}

- (void)testIsEqualToString {
  for (NSString *str1 in cornucopiaOfStrings) {
    for (NSString *str2 in bridgedStrings) {
      @autoreleasepool {
        for (int i = 0; i < 10; i++) {
          [str1 isEqualToString: str2];
        }
      }
    }
  }
}

- (void)testIsEqualToString2 {
  for (NSString *str1 in bridgedStrings) {
    for (NSString *str2 in cornucopiaOfStrings) {
      @autoreleasepool {
        for (int i = 0; i < 20; i++) {
          [str1 isEqualToString: str2];
        }
      }
    }
  }
}

- (void)testIsEqualToStringAllSwift {
  for (NSString *str1 in bridgedStrings) {
    for (NSString *str2 in bridgedStrings) {
      @autoreleasepool {
        for (int i = 0; i < 50; i++) {
          [str1 isEqualToString: str2];
        }
      }
    }
  }
}

- (void)testCompare {
  for (NSString *str1 in cornucopiaOfStrings) {
    for (NSString *str2 in bridgedStrings) {
      @autoreleasepool {
        for (int i = 0; i < 20; i++) {
          (void)[str1 compare: str2];
        }
      }
    }
  }
}

- (void)testCompare2 {
  for (NSString *str1 in bridgedStrings) {
    for (NSString *str2 in cornucopiaOfStrings) {
      @autoreleasepool {
        for (int i = 0; i < 20; i++) {
          (void)[str1 compare: str2];
        }
      }
    }
  }
}


- (void)testUTF8String {
  for (NSString *str1 in bridgedStrings) {
    @autoreleasepool {
      for (int i = 0; i < 100; i++) {
        (void)[str1 UTF8String];
      }
    }
  }
}

- (void)testGetCStringPtr {
  for (NSString *str1 in bridgedStrings) {
    for (int i = 0; i < 20000; i++) {
      (void)CFStringGetCStringPtr((CFStringRef)str1, kCFStringEncodingASCII);
    }
  }
}

- (void)testRangeOfString {
  for (NSString *str1 in bridgedStrings) {
    @autoreleasepool {
      for (int i = 0; i < 300; i++) {
        (void)[str1 rangeOfString:@"z"];
      }
    }
  };
}

- (void) testHash {
  for (NSString *str1 in bridgedStrings) {
    @autoreleasepool {
      for (int i = 0; i < 100; i++) {
        [str1 hash];
      }
    }
  }
}

- (void)testGetCharactersRange {
  unichar *buffer = malloc(10000);
  for (NSString *str1 in bridgedStrings) {
    @autoreleasepool {
      for (int i = 0; i < 100; i++) {
        (void)[str1 getCharacters:buffer range:NSMakeRange(0, [str1 length])];
      }
    }
  };
  free(buffer);
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
