//===--- CoreMediaOverlayShims.h ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains replacements for the initializers of some CoreMedia
// objects that return the created instance as an out parameter and thus
// cannot be used in Swift as-is.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_COREMEDIA_OVERLAY_H
#define SWIFT_STDLIB_SHIMS_COREMEDIA_OVERLAY_H

@import CoreMedia;
@import CoreFoundation;

#if __has_feature(nullability)
#pragma clang assume_nonnull begin
#endif

// slurped from CoreAudio headers that are sometime private.
typedef UInt32  AudioObjectID;
typedef AudioObjectID   AudioDeviceID;

// slurped from CoreMedia headers; see https://bugs.swift.org/browse/SR-2999
typedef struct CM_BRIDGED_TYPE(id) OpaqueCMBlockBuffer *CMBlockBufferRef;
typedef struct CM_BRIDGED_TYPE(id) opaqueCMBufferQueue *CMBufferQueueRef;
typedef struct CM_BRIDGED_TYPE(id) OpaqueCMClock* CMClockRef;
typedef const struct CM_BRIDGED_TYPE(id) opaqueCMFormatDescription *CMFormatDescriptionRef;
typedef struct CM_BRIDGED_TYPE(id) opaqueCMSampleBuffer *CMSampleBufferRef;
typedef struct CM_BRIDGED_TYPE(id) opaqueCMSimpleQueue *CMSimpleQueueRef;
typedef struct CM_BRIDGED_TYPE(id) OpaqueCMTimebase* CMTimebaseRef;

#define INIT_REFERENCING(Type, availability) \
CM_INLINE CM_RETURNS_RETAINED_PARAMETER Type##Ref CM_NONNULL \
  Type##Retain( \
  Type##Ref CM_NONNULL object) \
  CF_SWIFT_NAME(Type.init(referencing:)) \
  CF_REFINED_FOR_SWIFT \
  availability ;\
CM_INLINE CM_RETURNS_RETAINED_PARAMETER Type##Ref CM_NONNULL \
  Type##Retain( \
  Type##Ref CM_NONNULL object) \
{ \
  return (Type##Ref)CFRetain(object);\
}

INIT_REFERENCING(CMClock, API_AVAILABLE(macosx(10.8)) API_UNAVAILABLE(ios, tvos, watchos))

INIT_REFERENCING(CMBlockBuffer, API_AVAILABLE(macos(10.7), ios(4.0), tvos(9.0), watchos(6.0)))

INIT_REFERENCING(CMBufferQueue, API_AVAILABLE(macos(10.7), ios(4.0), tvos(9.0), watchos(6.0)))

INIT_REFERENCING(CMFormatDescription, API_AVAILABLE(macos(10.7), ios(4.0), tvos(9.0), watchos(6.0)))

INIT_REFERENCING(CMSampleBuffer, API_AVAILABLE(macos(10.7), ios(4.0), tvos(9.0), watchos(6.0)))

INIT_REFERENCING(CMSimpleQueue, API_AVAILABLE(macos(10.7), ios(4.0), tvos(9.0), watchos(6.0)))

INIT_REFERENCING(CMTimebase, API_AVAILABLE(macos(10.7), ios(4.0), tvos(9.0), watchos(6.0)))

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif


#endif // SWIFT_STDLIB_SHIMS_COREMEDIA_OVERLAY_H
