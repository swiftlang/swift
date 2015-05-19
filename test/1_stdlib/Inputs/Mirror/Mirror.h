//===--- Mirror.h ---------------------------------------------------------===//
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

#ifndef SWIFT_TEST_1_STDLIB_INPUTS_MIRROR_H
#define SWIFT_TEST_1_STDLIB_INPUTS_MIRROR_H

#import <Foundation/NSObject.h>

@interface HasIVars : NSObject
@end

@interface FooObjCClass : NSObject
@end

@interface FooDerivedObjCClass : FooObjCClass
@end

@interface FooMoreDerivedObjCClass : FooDerivedObjCClass
@end

#endif
