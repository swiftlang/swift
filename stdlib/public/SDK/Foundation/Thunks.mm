//===----------------------------------------------------------------------===//
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
#import <objc/runtime.h>
#include <objc/message.h>

#include "swift/Runtime/Config.h"

SWIFT_CC(swift)
extern "C" void NS_Swift_NSUndoManager_registerUndoWithTargetHandler(
    id NS_RELEASES_ARGUMENT _Nonnull self_,
    id NS_RELEASES_ARGUMENT _Nonnull target,
    void (^_Nonnull handler)(id _Nonnull)) {

  NSUndoManager *undoManager = self_;
  [undoManager registerUndoWithTarget:target handler:handler];

  [self_ release];
  [target release];
  [handler release];
}


// -- NSCoder
SWIFT_CC(swift)
extern "C" NS_RETURNS_RETAINED _Nullable id
NS_Swift_NSCoder_decodeObject(id NS_RELEASES_ARGUMENT _Nonnull self_,
                              NSError *_Nullable *_Nullable error) {
  NSCoder *coder = (NSCoder *)self_;
  id result = nil;
  if (error) {
    result = [coder decodeTopLevelObjectAndReturnError:error];
  } else {
    result = [coder decodeObject];
  }
  [self_ release];
  return [result retain];
}

SWIFT_CC(swift)
extern "C" NS_RETURNS_RETAINED _Nullable id
NS_Swift_NSCoder_decodeObjectForKey(id NS_RELEASES_ARGUMENT _Nonnull self_,
                                    id NS_RELEASES_ARGUMENT _Nonnull key,
                                    NSError *_Nullable *_Nullable error) {
  NSCoder *coder = (NSCoder *)self_;
  id result = nil;
  if (error) {
    result = [coder decodeTopLevelObjectForKey:key error:error];
  } else {
    result = [coder decodeObjectForKey:key];
  }
  [self_ release];
  [key release];
  return [result retain];
}

SWIFT_CC(swift)
extern "C" NS_RETURNS_RETAINED _Nullable id
NS_Swift_NSCoder_decodeObjectOfClassForKey(
    id NS_RELEASES_ARGUMENT _Nonnull self_,
    id NS_RELEASES_ARGUMENT _Nonnull cls, id NS_RELEASES_ARGUMENT _Nonnull key,
    NSError *_Nullable *_Nullable error) {
  NSCoder *coder = (NSCoder *)self_;
  id result = nil;
  if (error) {
    result = [coder decodeTopLevelObjectOfClass:cls forKey:key error:error];
  } else {
    result = [coder decodeObjectOfClass:cls forKey:key];
  }
  [self_ release];
  [key release];
  return [result retain];
}

SWIFT_CC(swift)
extern "C" NS_RETURNS_RETAINED _Nullable id
NS_Swift_NSCoder_decodeObjectOfClassesForKey(
    id NS_RELEASES_ARGUMENT _Nonnull self_,
    NSSet *NS_RELEASES_ARGUMENT _Nullable classes,
    id NS_RELEASES_ARGUMENT _Nonnull key, NSError *_Nullable *_Nullable error) {
  NSCoder *coder = (NSCoder *)self_;
  id result = nil;
  if (error) {
    result = [coder decodeTopLevelObjectOfClasses:classes forKey:key error:error];
  } else {
    result = [coder decodeObjectOfClasses:classes forKey:key];
  }
  [self_ release];
  [classes release];
  [key release];
  return [result retain];
}

// -- NSKeyedUnarchiver
SWIFT_CC(swift)
extern "C" NS_RETURNS_RETAINED _Nullable id
NS_Swift_NSKeyedUnarchiver_unarchiveObjectWithData(
    Class Self_, id NS_RELEASES_ARGUMENT _Nonnull data,
    NSError *_Nullable *_Nullable error) {
  id result = nil;
  if (error) {
    result = [Self_ unarchiveTopLevelObjectWithData:data error:error];
  } else {
    result = [Self_ unarchiveObjectWithData:data];
  }
  [data release];
  return [result retain];
}

// A way to get various Calendar, Locale, and TimeZone singletons without bridging twice

// Unfortunately the importer does not realize that [NSCalendar initWithIdentifier:] has been around forever, and only sees the iOS 8 availability of [NSCalendar calendarWithIdentifier:].
SWIFT_CC(swift)
extern "C" NS_RETURNS_RETAINED NSCalendar *_Nullable __NSCalendarInit(
    NSString *NS_RELEASES_ARGUMENT _Nonnull identifier) {
  NSCalendar *result =
      [[NSCalendar alloc] initWithCalendarIdentifier:identifier];
  [identifier release];
  return result;
}

SWIFT_CC(swift)
extern "C" NS_RETURNS_RETAINED NSCalendar *__NSCalendarAutoupdating() {
    return [[NSCalendar autoupdatingCurrentCalendar] retain];
}

SWIFT_CC(swift)
extern "C" NS_RETURNS_RETAINED NSCalendar *__NSCalendarCurrent() {
    return [[NSCalendar currentCalendar] retain];
}

SWIFT_CC(swift)
extern "C" NS_RETURNS_RETAINED NSTimeZone *__NSTimeZoneAutoupdating() {
    return [[NSTimeZone localTimeZone] retain];
}

SWIFT_CC(swift)
extern "C" NS_RETURNS_RETAINED NSTimeZone *__NSTimeZoneCurrent() {
    return [[NSTimeZone systemTimeZone] retain];
}

SWIFT_CC(swift)
extern "C" NS_RETURNS_RETAINED NSLocale *__NSLocaleCurrent() {
    return [[NSLocale currentLocale] retain];
}

SWIFT_CC(swift)
extern "C" NS_RETURNS_RETAINED NSLocale *__NSLocaleAutoupdating() {
    return [[NSLocale autoupdatingCurrentLocale] retain];
}

// Autoupdating Subclasses
SWIFT_CC(swift)
extern "C" BOOL
__NSCalendarIsAutoupdating(NSCalendar *NS_RELEASES_ARGUMENT _Nonnull calendar) {
  static dispatch_once_t onceToken;
  static Class autoCalendarClass;
  static Class olderAutoCalendarClass; // Pre 10.12/10.0
  dispatch_once(&onceToken, ^{
    autoCalendarClass = (Class)objc_lookUpClass("_NSAutoCalendar");
    olderAutoCalendarClass = (Class)objc_lookUpClass("NSAutoCalendar");
  });
  BOOL result =
      (autoCalendarClass && [calendar isKindOfClass:autoCalendarClass]) ||
      (olderAutoCalendarClass &&
       [calendar isKindOfClass:olderAutoCalendarClass]);
  [calendar release];
  return result;
}

SWIFT_CC(swift)
extern "C" BOOL
__NSTimeZoneIsAutoupdating(NSTimeZone *NS_RELEASES_ARGUMENT _Nonnull timeZone) {
  static dispatch_once_t onceToken;
  static Class autoTimeZoneClass;
  dispatch_once(&onceToken, ^{
    autoTimeZoneClass = (Class)objc_lookUpClass("__NSLocalTimeZone");
  });
  BOOL result = [timeZone isKindOfClass:autoTimeZoneClass];
  [timeZone release];
  return result;
}

SWIFT_CC(swift)
extern "C" BOOL
__NSLocaleIsAutoupdating(NSTimeZone *NS_RELEASES_ARGUMENT _Nonnull locale) {
  static dispatch_once_t onceToken;
  static Class autoLocaleClass;
  dispatch_once(&onceToken, ^{
    autoLocaleClass = (Class)objc_lookUpClass("NSAutoLocale");
  });
  BOOL result = [locale isKindOfClass:autoLocaleClass];
  [locale release];
  return result;
}

// -- NSError
SWIFT_CC(swift)
extern "C" void
NS_Swift_performErrorRecoverySelector(_Nullable id delegate, SEL selector,
                                      BOOL success,
                                      void *_Nullable contextInfo) {
  objc_msgSend(delegate, selector, success, contextInfo);
}

// -- NSDictionary
SWIFT_CC(swift)
extern "C" void
__NSDictionaryGetObjects(NSDictionary *_Nonnull nsDictionary,
                         id *objects, id *keys) {
  [nsDictionary getObjects:objects andKeys:keys];
  [nsDictionary release];
}

