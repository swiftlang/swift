//===--- Once.h - Runtime support for lazy initialization ------*- C++ -*--===//
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
//
// Swift runtime functions in support of lazy initialization.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_ONCE_H
#define SWIFT_RUNTIME_ONCE_H

#include "swift/Runtime/Alloc.h"

namespace swift {

typedef long swift_once_t;
  
/// Runs the given function with the given context argument exactly once.
/// The predicate argument must point to a global or static variable of static
/// extent of type swift_once_t.
extern "C"
void swift_once(swift_once_t *predicate, void (*fn)(HeapObject *ctx),
                HeapObject *ctx);

}

#endif
