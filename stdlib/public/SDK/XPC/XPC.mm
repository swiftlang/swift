//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <xpc/xpc.h>

__attribute__((visibility("hidden")))
extern "C" xpc_type_t
_swift_xpc_get_type(xpc_object_t object) {
  return xpc_get_type(object);
}

__attribute__((visibility("hidden")))
extern "C" xpc_object_t
_swift_xpc_bool_true() {
  return XPC_BOOL_TRUE;
}

__attribute__((visibility("hidden")))
extern "C" xpc_object_t
_swift_xpc_bool_false() {
  return XPC_BOOL_FALSE;
}

#define TYPE(t) \
  __attribute__((visibility("hidden"))) \
  extern "C" xpc_type_t \
  _swift_xpc_type_##t(void) { \
    return XPC_TYPE_##t; \
  }

TYPE(CONNECTION)
TYPE(ENDPOINT)
TYPE(NULL)
TYPE(BOOL)
TYPE(INT64)
TYPE(UINT64)
TYPE(DOUBLE)
TYPE(DATE)
TYPE(DATA)
TYPE(STRING)
TYPE(UUID)
TYPE(FD)
TYPE(SHMEM)
TYPE(ARRAY)
TYPE(DICTIONARY)
TYPE(ERROR)
TYPE(ACTIVITY)

__attribute__((visibility("hidden")))
extern "C" xpc_object_t
_swift_xpc_connection_interrupted(void) {
  return XPC_ERROR_CONNECTION_INTERRUPTED;
}

__attribute__((visibility("hidden")))
extern "C" xpc_object_t
_swift_xpc_connection_invalid(void) {
  return XPC_ERROR_CONNECTION_INVALID;
}

__attribute__((visibility("hidden")))
extern "C" xpc_object_t
_swift_xpc_connection_termination_imminent(void) {
  return XPC_ERROR_TERMINATION_IMMINENT;
}
