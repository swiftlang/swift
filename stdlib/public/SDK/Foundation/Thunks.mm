//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#import <Foundation/Foundation.h>

extern "C" void
NS_Swift_NSUndoManager_registerUndoWithTargetHandler(
    id NS_RELEASES_ARGUMENT __nonnull self_,
    id NS_RELEASES_ARGUMENT __nonnull target,
    void (^__nonnull handler)(id __nonnull)) {

  NSUndoManager *undoManager = self_;
  [undoManager registerUndoWithTarget:target handler:handler];

  [self_ release];
  [target release];
  [handler release];
}


// -- NSCoder
extern "C" NS_RETURNS_RETAINED __nullable id
NS_Swift_NSCoder_decodeObject(id NS_RELEASES_ARGUMENT __nonnull self_,
                              NSError *__nullable *__nullable error) {
  NSCoder *coder = (NSCoder *)self_;
  id result = nil;
  if (error) {
    result = [coder decodeObjectAndReturnError:error];
  } else {
    result = [coder decodeObject];
  }
  [self_ release];
  return [result retain];
}

extern "C" NS_RETURNS_RETAINED __nullable id
NS_Swift_NSCoder_decodeObjectForKey(id NS_RELEASES_ARGUMENT __nonnull self_,
                                    id NS_RELEASES_ARGUMENT __nonnull key,
                                    NSError *__nullable *__nullable error) {
  NSCoder *coder = (NSCoder *)self_;
  id result = nil;
  if (error) {
    result = [coder decodeObjectForKey:key error:error];
  } else {
    result = [coder decodeObjectForKey:key];
  }
  [self_ release];
  [key release];
  return [result retain];
}

extern "C" NS_RETURNS_RETAINED __nullable id
NS_Swift_NSCoder_decodeObjectOfClassForKey(
    id NS_RELEASES_ARGUMENT __nonnull self_,
    id NS_RELEASES_ARGUMENT __nonnull cls,
    id NS_RELEASES_ARGUMENT __nonnull key,
    NSError *__nullable *__nullable error) {
  NSCoder *coder = (NSCoder *)self_;
  id result = nil;
  if (error) {
    result = [coder decodeObjectOfClass:cls forKey:key error:error];
  } else {
    result = [coder decodeObjectOfClass:cls forKey:key];
  }
  [self_ release];
  [key release];
  return [result retain];
}

extern "C" NS_RETURNS_RETAINED __nullable id
NS_Swift_NSCoder_decodeObjectOfClassesForKey(
    id NS_RELEASES_ARGUMENT __nonnull self_,
    NSSet *NS_RELEASES_ARGUMENT __nullable classes,
    id NS_RELEASES_ARGUMENT __nonnull key,
    NSError *__nullable *__nullable error) {
  NSCoder *coder = (NSCoder *)self_;
  id result = nil;
  if (error) {
    result = [coder decodeObjectOfClasses:classes forKey:key error:error];
  } else {
    result = [coder decodeObjectOfClasses:classes forKey:key];
  }
  [self_ release];
  [classes release];
  [key release];
  return [result retain];
}

// -- NSKeyedUnarchiver
extern "C" NS_RETURNS_RETAINED __nullable id
NS_Swift_NSKeyedUnarchiver_unarchiveObjectWithData(
    Class Self_, id NS_RELEASES_ARGUMENT __nonnull data,
    NSError *__nullable *__nullable error) {
  id result = nil;
  if (error) {
    result = [Self_ unarchiveObjectWithData:data error:error];
  } else {
    result = [Self_ unarchiveObjectWithData:data];
  }
  [data release];
  return [result retain];
}

