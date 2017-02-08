//===--- UnavailableFoundationMethodThunks.mm -----------------------------===//
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

#include <Foundation/Foundation.h>

extern "C" void
swift_stdlib_NSArray_getObjects(NSArray NS_RELEASES_ARGUMENT *_Nonnull nsArray,
                                id *objects, NSUInteger rangeLocation,
                                NSUInteger rangeLength) {
  [nsArray getObjects:objects range:NSMakeRange(rangeLocation, rangeLength)];
  [nsArray release];
}

extern "C" void
swift_stdlib_NSDictionary_getObjects(NSDictionary *_Nonnull nsDictionary,
                                     id *objects, id *keys) {
  [nsDictionary getObjects:objects andKeys:keys];
  [nsDictionary release];
}

