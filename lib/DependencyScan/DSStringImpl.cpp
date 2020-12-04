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

/// Describes the kind of underlying data in ds_string_t.
enum ds_string_management {
  /// c_string_t contains a 'const char *' that it doesn't own.
  ds_string_unmanaged,
  /// c_string_t contains a 'const char *' that it allocated with malloc().
  ds_string_malloc
};

namespace swift {
namespace dependencies {
ds_string_t create_empty() {
  ds_string_t str;
  str.data = "";
  str.private_flags = ds_string_unmanaged;
  return str;
}

ds_string_t create_null() {
  ds_string_t str;
  str.data = nullptr;
  str.private_flags = ds_string_unmanaged;
  return str;
}

ds_string_t create_ref(const char *string) {
  if (string && string[0] == '\0')
    return create_empty();

  ds_string_t str;
  str.data = string;
  str.private_flags = ds_string_unmanaged;
  return str;
}

ds_string_t create_dup(const char *string) {
  if (!string)
    return create_null();

  if (string[0] == '\0')
    return create_empty();

  ds_string_t str;
  str.data = strdup(string);
  str.private_flags = ds_string_malloc;
  return str;
}

ds_string_set_t *create_set(const std::vector<std::string> &strings) {
  ds_string_set_t *set = new ds_string_set_t;
  set->count = strings.size();
  set->strings = new ds_string_t[set->count];
  for (unsigned SI = 0, SE = set->count; SI < SE; ++SI)
    set->strings[SI] = create_dup(strings[SI].c_str());
  return set;
}
} // namespace dependencies
} // namespace swift

//===----------------------------------------------------------------------===//
// libSwiftScan public APIs.
//===----------------------------------------------------------------------===//

const char *ds_get_C_string(ds_string_t string) {
  return static_cast<const char *>(string.data);
}

/// Free the given string.
void ds_string_dispose(ds_string_t string) {
  switch ((ds_string_management)string.private_flags) {
  case ds_string_unmanaged:
    break;
  case ds_string_malloc:
    if (string.data)
      free(const_cast<void *>(string.data));
    break;
  }
}

/// Free the given string set.
void ds_string_set_dispose(ds_string_set_t *set) {
  for (unsigned SI = 0, SE = set->count; SI < SE; ++SI)
    ds_string_dispose(set->strings[SI]);
  delete[] set->strings;
  delete set;
}
