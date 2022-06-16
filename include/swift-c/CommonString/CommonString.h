//===--- CommonString.h - C API for Swift Dependency Scanning ---*- C -*-===//
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

 #ifndef SWIFT_C_LIB_SWIFT_COMMON_STRING_H
 #define SWIFT_C_LIB_SWIFT_COMMON_STRING_H

 #include <stdbool.h>
 #include <stddef.h>
 #include <stdint.h>

 /**
  * A character string used to pass around dependency scan result metadata.
  * Lifetime of the string is strictly tied to the object whose field it
  * represents. When the owning object is released, string memory is freed.
  */
 typedef struct {
   const void *data;
   size_t length;
 } swiftscan_string_ref_t;

 typedef struct {
   swiftscan_string_ref_t *strings;
   size_t count;
 } swiftscan_string_set_t;

 #endif // SWIFT_C_LIB_SWIFT_COMMON_STRING_H
