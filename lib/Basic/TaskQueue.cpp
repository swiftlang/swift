//===--- TaskQueue.cpp - Task Execution Work Queue ------------------------===//
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
///
/// \file
/// \brief This file includes the appropriate platform-specific TaskQueue
/// implementation (or the default serial fallback if one is not available),
/// as well as any platform-agnostic TaskQueue functionality.
///
//===----------------------------------------------------------------------===//

#include "swift/Basic/TaskQueue.h"

using namespace swift;
using namespace swift::sys;

// Include the correct TaskQueue implementation.
// TODO: re-enable Unix implementation once output buffering works correctly
// (<rdar://problem/15795234>).
#if 0 && LLVM_ON_UNIX
#include "Unix/TaskQueue.inc"
#else
#include "Default/TaskQueue.inc"
#endif

TaskQueue::TaskQueue(unsigned NumberOfParallelTasks)
  : NumberOfParallelTasks(NumberOfParallelTasks) {}

TaskQueue::~TaskQueue() = default;
