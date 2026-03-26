//===-- DependencyScanMacros.h - Swift Dependency Scanning Macros -*- C -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifdef  __cplusplus
# define SWIFTSCAN_BEGIN_DECLS  extern "C" {
# define SWIFTSCAN_END_DECLS    }
#else
# define SWIFTSCAN_BEGIN_DECLS
# define SWIFTSCAN_END_DECLS
#endif

#ifndef SWIFTSCAN_PUBLIC
# ifdef SWIFTSCAN_NO_EXPORTS
#  define SWIFTSCAN_PUBLIC
# else  // SWIFTSCAN_NO_EXPORTS
#  ifdef _WIN32
#   ifdef SWIFTSCAN_EXPORTS
#     define SWIFTSCAN_PUBLIC __declspec(dllexport)
#   else  // SWIFTSCAN_EXPORTS
#     define SWIFTSCAN_PUBLIC __declspec(dllimport)
#   endif  // SWIFTSCAN_EXPORTS
#  else  // _WIN32
#   define SWIFTSCAN_PUBLIC
#  endif  // _WIN32
# endif  // SWIFTSCAN_NO_EXPORTS
#endif  // SWIFTSCAN_PUBLIC
