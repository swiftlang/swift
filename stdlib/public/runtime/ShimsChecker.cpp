//===--- ShimsChecker.cpp -------------------------------------------------===//
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
// This file checks the correctness of certain declarations from SwiftShims.
//
//===----------------------------------------------------------------------===//

#include <type_traits>
#include <unistd.h>
#include "../SwiftShims/DarwinShims.h"

static_assert(std::is_same<ssize_t, __swift_ssize_t>::value,
              "__swift_ssize_t is wrong");

