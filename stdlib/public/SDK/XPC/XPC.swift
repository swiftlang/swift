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

@_exported import XPC

//===----------------------------------------------------------------------===//
// XPC Types
//===----------------------------------------------------------------------===//
public var XPC_TYPE_CONNECTION: xpc_type_t {
	return _swift_xpc_type_connection()
}

public var XPC_TYPE_ENDPOINT: xpc_type_t {
	return _swift_xpc_type_endpoint()
}

public var XPC_TYPE_NULL: xpc_type_t {
	return _swift_xpc_type_null()
}

public var XPC_TYPE_BOOL: xpc_type_t {
	return _swift_xpc_type_bool()
}

public var XPC_TYPE_INT64: xpc_type_t {
	return _swift_xpc_type_int64()
}

public var XPC_TYPE_UINT64: xpc_type_t {
	return _swift_xpc_type_uint64()
}

public var XPC_TYPE_DOUBLE: xpc_type_t {
	return _swift_xpc_type_double()
}

public var XPC_TYPE_DATE: xpc_type_t {
	return _swift_xpc_type_date()
}

public var XPC_TYPE_DATA: xpc_type_t {
	return _swift_xpc_type_data()
}

public var XPC_TYPE_STRING: xpc_type_t {
	return _swift_xpc_type_string()
}

public var XPC_TYPE_UUID: xpc_type_t {
	return _swift_xpc_type_uuid()
}

public var XPC_TYPE_FD: xpc_type_t {
	return _swift_xpc_type_fd()
}

public var XPC_TYPE_SHMEM: xpc_type_t {
	return _swift_xpc_type_shmem()
}

public var XPC_TYPE_ARRAY: xpc_type_t {
	return _swift_xpc_type_array()
}

public var XPC_TYPE_DICTIONARY: xpc_type_t {
	return _swift_xpc_type_dictionary()
}

public var XPC_TYPE_ERROR: xpc_type_t {
	return _swift_xpc_type_error()
}

public var XPC_TYPE_ACTIVITY: xpc_type_t {
	return _swift_xpc_type_activity()
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

@_silgen_name("_swift_xpc_bool_true")
internal func _swift_xpc_bool_true() -> xpc_object_t

@_silgen_name("_swift_xpc_bool_false")
internal func _swift_xpc_bool_false() -> xpc_object_t

@_silgen_name("_swift_xpc_type_CONNECTION")
internal func _swift_xpc_type_connection() -> xpc_type_t

@_silgen_name("_swift_xpc_type_ENDPOINT")
internal func _swift_xpc_type_endpoint() -> xpc_type_t

@_silgen_name("_swift_xpc_type_NULL")
internal func _swift_xpc_type_null() -> xpc_type_t

@_silgen_name("_swift_xpc_type_BOOL")
internal func _swift_xpc_type_bool() -> xpc_type_t

@_silgen_name("_swift_xpc_type_INT64")
internal func _swift_xpc_type_int64() -> xpc_type_t

@_silgen_name("_swift_xpc_type_UINT64")
internal func _swift_xpc_type_uint64() -> xpc_type_t

@_silgen_name("_swift_xpc_type_DOUBLE")
internal func _swift_xpc_type_double() -> xpc_type_t

@_silgen_name("_swift_xpc_type_DATE")
internal func _swift_xpc_type_date() -> xpc_type_t

@_silgen_name("_swift_xpc_type_DATA")
internal func _swift_xpc_type_data() -> xpc_type_t

@_silgen_name("_swift_xpc_type_STRING")
internal func _swift_xpc_type_string() -> xpc_type_t

@_silgen_name("_swift_xpc_type_UUID")
internal func _swift_xpc_type_uuid() -> xpc_type_t

@_silgen_name("_swift_xpc_type_FD")
internal func _swift_xpc_type_fd() -> xpc_type_t

@_silgen_name("_swift_xpc_type_SHMEM")
internal func _swift_xpc_type_shmem() -> xpc_type_t

@_silgen_name("_swift_xpc_type_ARRAY")
internal func _swift_xpc_type_array() -> xpc_type_t

@_silgen_name("_swift_xpc_type_DICTIONARY")
internal func _swift_xpc_type_dictionary() -> xpc_type_t

@_silgen_name("_swift_xpc_type_ERROR")
internal func _swift_xpc_type_error() -> xpc_type_t

@_silgen_name("_swift_xpc_type_ACTIVITY")
internal func _swift_xpc_type_activity() -> xpc_type_t

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

@_silgen_name("_swift_xpc_connection_interrupted")
internal func _swift_xpc_connection_interrupted() -> xpc_object_t

@_silgen_name("_swift_xpc_connection_invalid")
internal func _swift_xpc_connection_invalid() -> xpc_object_t

@_silgen_name("_swift_xpc_connection_termination_imminent")
internal func _swift_xpc_connection_termination_imminent() -> xpc_object_t
