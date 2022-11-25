//==-- Musl.h - Threading abstraction implementation for Musl -- -*-C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implements threading support for plain pthreads, adjusting for Musl.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_IMPL_MUSL_H
#define SWIFT_THREADING_IMPL_MUSL_H

#undef SWIFT_LAZY_MUTEX_INITIALIZER 
#define SWIFT_LAZY_MUTEX_INITIALIZER PTHREAD_MUTEX_INITIALIZER

#include "Pthreads.h"

#endif // SWIFT_THREADING_IMPL_MUSL_H
