//===--- DiagnosticsRefactoring.def - Diagnostics Text ----------*- C++ -*-===//
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
//  This file defines diagnostics emitted during refactoring.
//  Each diagnostic is described using one of three kinds (error, warning, or
//  note) along with a unique identifier, category, options, and text, and is
//  followed by a signature describing the diagnostic argument kinds.
//
//===----------------------------------------------------------------------===//

#if !(defined(DIAG) || (defined(ERROR) && defined(WARNING) && defined(NOTE)))
#  error Must define either DIAG or the set {ERROR,WARNING,NOTE}
#endif

#ifndef ERROR
#  define ERROR(ID,Options,Text,Signature)   \
  DIAG(ERROR,ID,Options,Text,Signature)
#endif

#ifndef WARNING
#  define WARNING(ID,Options,Text,Signature) \
  DIAG(WARNING,ID,Options,Text,Signature)
#endif

#ifndef NOTE
#  define NOTE(ID,Options,Text,Signature) \
  DIAG(NOTE,ID,Options,Text,Signature)
#endif

//==============================================================================
// Refactoring diagnostics
//==============================================================================

ERROR(invalid_name, none, "'%0' is not a valid name", (StringRef))

ERROR(invalid_location, none, "given location is not valid", ())

ERROR(arity_mismatch, none, "the given new name '%0' does not match the arity of the old name '%1'", (StringRef, StringRef))

ERROR(name_not_functionlike, none, "the 'call' name usage cannot be used with a non-function-like name '%0'", (StringRef))

ERROR(unresolved_location, none, "cannot resolve location as name", ())

ERROR(location_module_mismatch, none, "given location does not belong to module '%0'", (StringRef))

ERROR(value_decl_no_loc, none, "value decl '%0' has no declaration location", (DeclName))

ERROR(value_decl_referenced_out_of_range, none, "value decl '%0' is referenced out of range", (DeclName))

ERROR(multi_entry_range, none, "selected range has more than one entry point", ())

ERROR(orphan_loop_keyword, none, "selected range contains %0 but not its target loop", (StringRef))

ERROR(invalid_default_location, none, "given location is not on a default statement", ())

ERROR(no_parent_switch, none, "cannot find enclosing switch statement", ())

ERROR(no_remaining_cases, none, "no remaining cases to expand", ())

WARNING(mismatched_rename, none, "the name at the given location cannot be renamed to '%0'", (StringRef))

ERROR(no_insert_position, none, "cannot find inserting position", ())
