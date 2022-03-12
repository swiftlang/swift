//===--- MutexWin32.cpp - -------------------------------------------------===//
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
//
// Mutex, Read/Write lock, and Scoped lock implementations
// using Windows Slim Reader/Writer Locks.
//
//===----------------------------------------------------------------------===//

#if defined(_WIN32)

// Notes: swift::fatalError is not shared between libswiftCore and libswift_Concurrency
// and libswift_Concurrency uses swift_Concurrency_fatalError instead.
#ifndef SWIFT_FATAL_ERROR
#define SWIFT_FATAL_ERROR swift::fatalError
#endif

#include "swift/Runtime/Mutex.h"
#include "swift/Runtime/Debug.h"

using namespace swift;

#endif
