//===--- shims.h ----------------------------------------------------------===//
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
#include <stdint.h>

// This struct is layout-compatible with NSRange.  Using the name
// "NSRange" here could eliminate some horrible reinterpretCast
// shenanigans in our briging code, but swift's module importer is not
// yet tolerant of the same struct coming in from two different Clang
// modules. <rdar://problem/16294674>
typedef struct {
  intptr_t location;
  intptr_t length;
} _SwiftNSRange;

#ifndef __OBJC_SUPERCLASS_HACK__
#define __OBJC_SUPERCLASS_HACK__
__attribute__((objc_root_class))
@interface _ObjCSuperClassHack
{
  void *isa;
}
@end
#endif
