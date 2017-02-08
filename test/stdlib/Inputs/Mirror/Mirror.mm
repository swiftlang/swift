//===--- Mirror.mm --------------------------------------------------------===//
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

#include "Mirror.h"

@implementation HasIVars {
  int ivar;
}
@end

@implementation FooObjCClass : NSObject

-(NSString *) description {
  return @"This is FooObjCClass";
}

@end

@implementation FooDerivedObjCClass : FooObjCClass
@end

@implementation FooMoreDerivedObjCClass : FooDerivedObjCClass
@end

