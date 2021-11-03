//===- StringUtils.cpp - Routines for manipulating swiftscan_string_ref_t -===//
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

#include "swift/DependencyScan/StringUtils.h"
#include <string>
#include <vector>

namespace swift {
namespace dependencies {

swiftscan_string_ref_t create_null() {
  swiftscan_string_ref_t str;
  str.data = nullptr;
  str.length = 0;
  return str;
}

swiftscan_string_ref_t create_clone(const char *string) {
  if (!string)
    return create_null();

  if (string[0] == '\0')
    return create_null();

  swiftscan_string_ref_t str;
  str.data = strdup(string);
  str.length = strlen(string);
  return str;
}

swiftscan_string_set_t *create_set(const std::vector<std::string> &strings) {
  swiftscan_string_set_t *set = new swiftscan_string_set_t;
  set->count = strings.size();
  set->strings = new swiftscan_string_ref_t[set->count];
  for (unsigned SI = 0, SE = set->count; SI < SE; ++SI)
    set->strings[SI] = create_clone(strings[SI].c_str());
  return set;
}

swiftscan_string_set_t *create_set(int count, const char **strings) {
  swiftscan_string_set_t *set = new swiftscan_string_set_t;
  set->count = count;
  set->strings = new swiftscan_string_ref_t[set->count];
  for (unsigned SI = 0, SE = set->count; SI < SE; ++SI)
    set->strings[SI] = create_clone(strings[SI]);
  return set;
}

swiftscan_string_set_t *create_empty_set() {
  swiftscan_string_set_t *set = new swiftscan_string_set_t;
  set->count = 0;
  return set;
}

const char *get_C_string(swiftscan_string_ref_t string) {
  return static_cast<const char *>(string.data);
}
} // namespace dependencies
} // namespace swift
