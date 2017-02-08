//===--- XPCOverlayShims.h --------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_STDLIB_SHIMS_XPC_OVERLAY_H
#define SWIFT_STDLIB_SHIMS_XPC_OVERLAY_H

@import XPC;

static inline xpc_type_t
_swift_xpc_get_type(xpc_object_t object) {
  return xpc_get_type(object);
}

static inline xpc_object_t
_swift_xpc_bool_true() {
  return XPC_BOOL_TRUE;
}

static inline xpc_object_t
_swift_xpc_bool_false() {
  return XPC_BOOL_FALSE;
}

#define SWIFT_XPC_TYPE(t) \
  static inline xpc_type_t \
  _swift_xpc_type_##t(void) { \
    return XPC_TYPE_##t; \
  }

SWIFT_XPC_TYPE(CONNECTION)
SWIFT_XPC_TYPE(ENDPOINT)
SWIFT_XPC_TYPE(NULL)
SWIFT_XPC_TYPE(BOOL)
SWIFT_XPC_TYPE(INT64)
SWIFT_XPC_TYPE(UINT64)
SWIFT_XPC_TYPE(DOUBLE)
SWIFT_XPC_TYPE(DATE)
SWIFT_XPC_TYPE(DATA)
SWIFT_XPC_TYPE(STRING)
SWIFT_XPC_TYPE(UUID)
SWIFT_XPC_TYPE(FD)
SWIFT_XPC_TYPE(SHMEM)
SWIFT_XPC_TYPE(ARRAY)
SWIFT_XPC_TYPE(DICTIONARY)
SWIFT_XPC_TYPE(ERROR)
SWIFT_XPC_TYPE(ACTIVITY)

#undef SWIFT_XPC_TYPE

static inline xpc_object_t
_swift_xpc_connection_interrupted(void) {
  return XPC_ERROR_CONNECTION_INTERRUPTED;
}

static inline xpc_object_t
_swift_xpc_connection_invalid(void) {
  return XPC_ERROR_CONNECTION_INVALID;
}

static inline xpc_object_t
_swift_xpc_connection_termination_imminent(void) {
  return XPC_ERROR_TERMINATION_IMMINENT;
}

#endif // SWIFT_STDLIB_SHIMS_XPC_OVERLAY_H

