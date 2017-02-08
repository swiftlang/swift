//===------------------------------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_STDLIB_SHIMS_SWIFTSTDBOOL_H_
#define SWIFT_STDLIB_SHIMS_SWIFTSTDBOOL_H_

#ifdef __cplusplus
typedef bool __swift_bool;
#else
typedef _Bool __swift_bool;
#endif

#endif

