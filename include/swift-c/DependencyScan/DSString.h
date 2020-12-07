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

#ifndef SWIFT_C_SWIFTSCANSTRING_H
#define SWIFT_C_SWIFTSCANSTRING_H

#include "DependencyScanMacros.h"

SWIFTSCAN_BEGIN_DECLS

/**
 * A character string used to pass around dependency scan result metadata.
 * A slightly-reduced variation of clang's CXString
 * Use \c swiftscan_get_C_string() to retrieve the string data and, once finished
 * with the string data, call \c swiftscan_dispose_string() to free the string.
 */
typedef struct {
  const void *data;
  unsigned private_flags;
} swiftscan_string_t;

typedef struct {
  swiftscan_string_t *strings;
  unsigned count;
} swiftscan_string_set_t;

//=== Public API ----------------------------------------------------------===//
/// Retrieve the character data associated with the given string.
SWIFTSCAN_PUBLIC const char *swiftscan_get_C_string(swiftscan_string_t string);


/// Free the given string.
SWIFTSCAN_PUBLIC void swiftscan_string_dispose(swiftscan_string_t string);


/// Free the given string set.
SWIFTSCAN_PUBLIC void swiftscan_string_set_dispose(swiftscan_string_set_t *set);

SWIFTSCAN_END_DECLS

#endif // SWIFT_C_SWIFTSCANSTRING_H
