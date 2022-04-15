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

#if SWIFT_THREADING_NONE
#include "Impl/Nothreads.h"
#elif SWIFT_THREADING_DARWIN
#include "Impl/Darwin.h"
#elif SWIFT_THREADING_LINUX
#include "Impl/Linux.h"
#elif SWIFT_THREADING_PTHREADS
#include "Impl/Pthreads.h"
#elif SWIFT_THREADING_C11
#include "Impl/C11.h"
#elif SWIFT_THREADING_WIN32
#include "Impl/Win32.h"
#else
#error You need to implement Threading/Impl.h for your threading package.
#endif

#endif // SWIFT_THREADING_IMPL_H
