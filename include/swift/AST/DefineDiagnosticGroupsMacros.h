//===--- DefineDiagnosticGroupsMacros.def -----------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines macros defining diagnostic groups.
//
//===----------------------------------------------------------------------===//

// Define macros
#if defined(DEFINE_DIAGNOSTIC_GROUPS_MACROS) &&                                \
    !defined(UNDEFINE_DIAGNOSTIC_GROUPS_MACROS)

#undef DEFINE_DIAGNOSTIC_GROUPS_MACROS

#if !(defined(GROUP) || defined(GROUP_LINK))
#error No reqired macros defined. Define at least one of the following macros: GROUP(Name, DocsFile), GROUP_LINK(Parent, Child)
#endif

// GROUP macro:
// Declares a diagnostic group.
// Parameters:
//   Name - group name as it appears in DiagGroupID enum and DiagGroupInfo.name
//   DocsFile - file with a human readable description for the group located in
//   userdocs/diagnostic_groups
#ifndef GROUP
#define GROUP(Name, DocsFile)
#endif

// GROUP_LINK macro:
// Establishes an edge in the diagnostic group graph between
// a supergroup(Parent) and its subgroup(Child).
// Parameters:
//   Parent - parent group name
//   Child - child group name
#ifndef GROUP_LINK
#define GROUP_LINK(Parent, Child)
#endif

// Undefine macros
#elif defined(UNDEFINE_DIAGNOSTIC_GROUPS_MACROS) &&                            \
    !defined(DEFINE_DIAGNOSTIC_GROUPS_MACROS)

#undef UNDEFINE_DIAGNOSTIC_GROUPS_MACROS

#ifdef GROUP
#undef GROUP
#else
#error Trying to undefine the diagnostic groups macros, but GROUP macro wasn't defined
#endif

#ifdef GROUP_LINK
#undef GROUP_LINK
#else
#error Trying to undefine the diagnostic groups macros, but GROUP_LINK macro wasn't defined
#endif

#else
#error Invalid DefineDiagnosticGroupsMacros.h inclusion
#endif
