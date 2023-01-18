//===--- Impl.h - Threading abstraction implementation -------- -*- C++ -*-===//
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
// Includes the relevant implementation file based on the selected threading
// package.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_IMPL_H
#define SWIFT_THREADING_IMPL_H

#include "TLSKeys.h"

namespace swift {
namespace threading_impl {

struct stack_bounds {
  void *low;
  void *high;
};

} // namespace swift
} // namespace threading_impl


// Try to autodetect if we aren't told what to do
#if !defined(SWIFT_THREADING_NONE)  && !defined(SWIFT_THREADING_DARWIN)   && \
    !defined(SWIFT_THREADING_LINUX) && !defined(SWIFT_THREADING_PTHREADS) && \
    !defined(SWIFT_THREADING_C11)   && !defined(SWIFT_THREADING_WIN32)    && \
    !defined(SWIFT_THREADING_MUSL)
#ifdef __APPLE__
#define SWIFT_THREADING_DARWIN
#elif defined(__linux__)
#define SWIFT_THREADING_LINUX
#elif defined(_WIN32)
#define SWIFT_THREADING_WIN32
#elif defined(__wasi__)
#define SWIFT_THREADING_NONE
#elif __has_include(<threads.h>)
#define SWIFT_THREADING_C11
#elif __has_include(<pthread.h>)
#define SWIFT_THREADING_PTHREADS
#else
#error Unable to autodetect threading package.  Please define SWIFT_THREADING_x as appropriate for your platform.
#endif
#endif

#if defined(SWIFT_THREADING_NONE)
#include "Impl/Nothreads.h"
#elif defined(SWIFT_THREADING_DARWIN)
#include "Impl/Darwin.h"
#elif defined(SWIFT_THREADING_LINUX)
#include "Impl/Linux.h"
#elif defined(SWIFT_THREADING_PTHREADS)
#include "Impl/Pthreads.h"
#elif defined(SWIFT_THREADING_MUSL)
#include "Impl/Musl.h"
#elif defined(SWIFT_THREADING_C11)
#include "Impl/C11.h"
#elif defined(SWIFT_THREADING_WIN32)
#include "Impl/Win32.h"
#else
#error You need to implement Threading/Impl.h for your threading package.
#endif

#endif // SWIFT_THREADING_IMPL_H
