//===--- Private.h - Private runtime declarations --------------*- C++ -*--===//
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
// Private declarations of the Swift runtime.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_PRIVATE_H
#define SWIFT_RUNTIME_PRIVATE_H

#include "swift/Runtime/Config.h"
#include "swift/Runtime/FastEntryPoints.h"
#include "llvm/Support/Compiler.h"

namespace swift {
  extern "C" LLVM_LIBRARY_VISIBILITY LLVM_ATTRIBUTE_NORETURN
  void _swift_abortRetainUnowned(const void *object);

} // end namespace swift

#endif /* SWIFT_RUNTIME_PRIVATE_H */
