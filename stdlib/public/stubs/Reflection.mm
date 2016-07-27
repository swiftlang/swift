//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Config.h"
#import <Foundation/Foundation.h>

SWIFT_CC(swift)
SWIFT_RUNTIME_STDLIB_INTERFACE
extern "C" bool _swift_stdlib_NSObject_isKindOfClass(
    id NS_RELEASES_ARGUMENT _Nonnull object,
    NSString *NS_RELEASES_ARGUMENT _Nonnull className) {
  bool result = [object isKindOfClass:NSClassFromString(className)];
  [object release];
  [className release];

  return result;
}

