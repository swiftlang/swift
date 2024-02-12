//===--- NoDiscard.h ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_NODISCARD_H
#define SWIFT_BASIC_NODISCARD_H

#if __cplusplus > 201402l && __has_cpp_attribute(nodiscard)
#define SWIFT_NODISCARD [[nodiscard]]
#elif __has_cpp_attribute(clang::warn_unused_result)
#define SWIFT_NODISCARD [[clang::warn_unused_result]]
#else
#define SWIFT_NODISCARD
#endif

#endif
