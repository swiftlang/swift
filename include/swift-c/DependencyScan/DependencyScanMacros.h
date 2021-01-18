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
# ifdef _WIN32
#  ifdef libSwiftScan_EXPORTS
#    define SWIFTSCAN_PUBLIC __declspec(dllexport)
#  else
#    define SWIFTSCAN_PUBLIC __declspec(dllimport)
#  endif
# else
#  define SWIFTSCAN_PUBLIC
# endif
#endif
