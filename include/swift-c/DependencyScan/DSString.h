//===-- swift-c/DSString.h - Managed C String  --------------------*- C -*-===//
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

#ifndef SWIFT_C_DSSTRING_H
#define SWIFT_C_DSSTRING_H

#include "DependencyScanMacros.h"

DEPSCAN_BEGIN_DECLS

/**
 * A character string used to pass around dependency scan result metadata.
 * A slightly-reduced variation of clang's CXString
 * Use \c depscan_get_C_string() to retrieve the string data and, once finished
 * with the string data, call \c depscan_dispose_string() to free the string.
 */
typedef struct {
  const void *data;
  unsigned private_flags;
} ds_string_t;

typedef struct {
  ds_string_t *strings;
  unsigned count;
} ds_string_set_t;

//=== Public API ----------------------------------------------------------===//
/// Retrieve the character data associated with the given string.
DEPSCAN_PUBLIC const char *depscan_get_C_string(ds_string_t string);


/// Free the given string.
DEPSCAN_PUBLIC void depscan_string_dispose(ds_string_t string);


/// Free the given string set.
DEPSCAN_PUBLIC void depscan_string_set_dispose(ds_string_set_t *set);

DEPSCAN_END_DECLS

#endif // SWIFT_C_DSSTRING_H


