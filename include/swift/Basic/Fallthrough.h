//===- Fallthrough.h - switch fallthrough annotation macro ------*- C++ -*-===//
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
// This filed defines a SWIFT_FALLTHROUGH macro to annotate intentional
// fallthrough between switch cases. For compilers that support the
// "clang::fallthrough" attribute, it expands to an empty statement with the
// attribute applied; otherwise, it expands to just an empty statement.
//
//===----------------------------------------------------------------------===//

#ifndef __SWIFT_FALLTHROUGH_H__
#define __SWIFT_FALLTHROUGH_H__

#ifndef __has_attribute
# define __has_attribute(x) 0
#endif

#ifndef __has_cpp_attribute
# define __has_cpp_attribute(x) 0
#endif

#if __has_attribute(fallthrough)
# define SWIFT_FALLTHROUGH [[clang::fallthrough]]
#elif __has_cpp_attribute(clang::fallthrough)
# define SWIFT_FALLTHROUGH [[clang::fallthrough]]
#else
# define SWIFT_FALLTHROUGH
#endif

#endif
