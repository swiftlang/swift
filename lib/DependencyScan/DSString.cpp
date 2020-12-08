//===- SCString.cpp - Routines for manipulating c_string_t ----------------===//
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

#include "swift/DependencyScan/DSStringImpl.h"

/// Describes the kind of underlying data in swiftscan_string_t.
enum swiftscan_string_management {
  /// c_string_t contains a 'const char *' that it doesn't own.
  swiftscan_string_unmanaged,
  /// c_string_t contains a 'const char *' that it allocated with malloc().
  swiftscan_string_malloc
};

namespace swift {
namespace dependencies {
swiftscan_string_t create_empty() {
  swiftscan_string_t str;
  str.data = "";
  str.private_flags = swiftscan_string_unmanaged;
  return str;
}

swiftscan_string_t create_null() {
  swiftscan_string_t str;
  str.data = nullptr;
  str.private_flags = swiftscan_string_unmanaged;
  return str;
}

swiftscan_string_t create_ref(const char *string) {
  if (string && string[0] == '\0')
    return create_empty();

  swiftscan_string_t str;
  str.data = string;
  str.private_flags = swiftscan_string_unmanaged;
  return str;
}

swiftscan_string_t create_dup(const char *string) {
  if (!string)
    return create_null();

  if (string[0] == '\0')
    return create_empty();

  swiftscan_string_t str;
  str.data = strdup(string);
  str.private_flags = swiftscan_string_malloc;
  return str;
}

swiftscan_string_set_t *create_set(const std::vector<std::string> &strings) {
  swiftscan_string_set_t *set = new swiftscan_string_set_t;
  set->count = strings.size();
  set->strings = new swiftscan_string_t[set->count];
  for (unsigned SI = 0, SE = set->count; SI < SE; ++SI)
    set->strings[SI] = create_dup(strings[SI].c_str());
  return set;
}
} // namespace dependencies
} // namespace swift

//===----------------------------------------------------------------------===//
// libSwiftScan public APIs.
//===----------------------------------------------------------------------===//

const char *swiftscan_get_C_string(swiftscan_string_t string) {
  return static_cast<const char *>(string.data);
}

/// Free the given string.
void swiftscan_string_dispose(swiftscan_string_t string) {
  switch ((swiftscan_string_management)string.private_flags) {
  case swiftscan_string_unmanaged:
    break;
  case swiftscan_string_malloc:
    if (string.data)
      free(const_cast<void *>(string.data));
    break;
  }
}

/// Free the given string set.
void swiftscan_string_set_dispose(swiftscan_string_set_t *set) {
  for (unsigned SI = 0, SE = set->count; SI < SE; ++SI)
    swiftscan_string_dispose(set->strings[SI]);
  delete[] set->strings;
  delete set;
}
