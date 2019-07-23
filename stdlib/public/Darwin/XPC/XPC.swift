//===----------------------------------------------------------------------===//
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

@_exported import XPC
import _SwiftXPCOverlayShims

//===----------------------------------------------------------------------===//
// XPC Types
//===----------------------------------------------------------------------===//
public var XPC_TYPE_CONNECTION: xpc_type_t {
	return _swift_xpc_type_CONNECTION()
}

public var XPC_TYPE_ENDPOINT: xpc_type_t {
	return _swift_xpc_type_ENDPOINT()
}

public var XPC_TYPE_NULL: xpc_type_t {
	return _swift_xpc_type_NULL()
}

public var XPC_TYPE_BOOL: xpc_type_t {
	return _swift_xpc_type_BOOL()
}

public var XPC_TYPE_INT64: xpc_type_t {
	return _swift_xpc_type_INT64()
}

public var XPC_TYPE_UINT64: xpc_type_t {
	return _swift_xpc_type_UINT64()
}

public var XPC_TYPE_DOUBLE: xpc_type_t {
	return _swift_xpc_type_DOUBLE()
}

public var XPC_TYPE_DATE: xpc_type_t {
	return _swift_xpc_type_DATE()
}

public var XPC_TYPE_DATA: xpc_type_t {
	return _swift_xpc_type_DATA()
}

public var XPC_TYPE_STRING: xpc_type_t {
	return _swift_xpc_type_STRING()
}

public var XPC_TYPE_UUID: xpc_type_t {
	return _swift_xpc_type_UUID()
}

public var XPC_TYPE_FD: xpc_type_t {
	return _swift_xpc_type_FD()
}

public var XPC_TYPE_SHMEM: xpc_type_t {
	return _swift_xpc_type_SHMEM()
}

public var XPC_TYPE_ARRAY: xpc_type_t {
	return _swift_xpc_type_ARRAY()
}

public var XPC_TYPE_DICTIONARY: xpc_type_t {
	return _swift_xpc_type_DICTIONARY()
}

public var XPC_TYPE_ERROR: xpc_type_t {
	return _swift_xpc_type_ERROR()
}

public var XPC_TYPE_ACTIVITY: xpc_type_t {
	return _swift_xpc_type_ACTIVITY()
}

//===----------------------------------------------------------------------===//
// Macros
//===----------------------------------------------------------------------===//

// xpc/xpc.h
public let XPC_ERROR_KEY_DESCRIPTION: UnsafePointer<Int8> = _xpc_error_key_description
public let XPC_EVENT_KEY_NAME: UnsafePointer<Int8> = _xpc_event_key_name

public var XPC_BOOL_TRUE: xpc_object_t {
	return _swift_xpc_bool_true()
}

public var XPC_BOOL_FALSE: xpc_object_t {
	return _swift_xpc_bool_false()
}

public var XPC_ARRAY_APPEND: size_t {
	return -1
}

// xpc/connection.h

public var XPC_ERROR_CONNECTION_INTERRUPTED: xpc_object_t {
	return _swift_xpc_connection_interrupted()
}

public var XPC_ERROR_CONNECTION_INVALID: xpc_object_t {
	return _swift_xpc_connection_invalid()
}

public var XPC_ERROR_TERMINATION_IMMINENT: xpc_object_t {
	return _swift_xpc_connection_termination_imminent()
}
