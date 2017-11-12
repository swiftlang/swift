//===--- MacroTable.def - Macros suppressed during import -------*- C++ -*-===//
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
// This file defines the database of macros that should be suppressed during
// API import.
//
//===----------------------------------------------------------------------===//

#ifndef SUPPRESS_MACRO
/// Describes a macro that should be suppressed.
# define SUPPRESS_MACRO(MacroName)
#endif

SUPPRESS_MACRO(CF_USE_OSBYTEORDER_H)
SUPPRESS_MACRO(CGVECTOR_DEFINED)
SUPPRESS_MACRO(false)
SUPPRESS_MACRO(FALSE)
SUPPRESS_MACRO(NSEDGEINSETS_DEFINED)
SUPPRESS_MACRO(NSGEOMETRY_TYPES_SAME_AS_CGGEOMETRY_TYPES)
SUPPRESS_MACRO(NSINTEGER_DEFINED)
SUPPRESS_MACRO(NS_BLOCKS_AVAILABLE)
SUPPRESS_MACRO(NS_UNICHAR_IS_EIGHT_BIT)
SUPPRESS_MACRO(true)
SUPPRESS_MACRO(TRUE)
SUPPRESS_MACRO(DISPATCH_SWIFT_OVERLAY)

#ifndef NIL_MACRO
/// Describes a macro whose function is similar to Swift's \c nil.
///
/// These are treated the same as any other suppressed macro by default.
# define NIL_MACRO(MacroName) SUPPRESS_MACRO(MacroName)
#endif

NIL_MACRO(NULL)
NIL_MACRO(nil)
NIL_MACRO(Nil)

#undef NIL_MACRO
#undef SUPPRESS_MACRO
