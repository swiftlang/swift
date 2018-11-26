//===--- NetworkOverlayShims.h ---------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//


#ifndef SWIFT_STDLIB_SHIMS_NETWORKSHIMS_H
#define SWIFT_STDLIB_SHIMS_NETWORKSHIMS_H

@import Network;

#ifdef __OBJC__
#define SWIFT_NW_RETURNS_RETAINED __attribute__((__ns_returns_retained__))
#else
#define SWIFT_NW_RETURNS_RETAINED
#endif

#pragma clang assume_nonnull begin

typedef void (^__swift_nw_connection_send_completion_t)(_Nullable nw_error_t error);

static inline SWIFT_NW_RETURNS_RETAINED nw_content_context_t
_swift_nw_content_context_default_message(void) {
	return _nw_content_context_default_message;
}

static inline SWIFT_NW_RETURNS_RETAINED nw_content_context_t
_swift_nw_content_context_final_message(void) {
	return _nw_content_context_final_send;
}

static inline SWIFT_NW_RETURNS_RETAINED nw_content_context_t
_swift_nw_content_context_default_stream(void) {
	return _nw_content_context_default_stream;
}

static inline void
_swift_nw_connection_send_idempotent(nw_connection_t connection, _Nullable dispatch_data_t content, _Nullable nw_content_context_t context, bool is_complete) {
	nw_connection_send(connection, content, context, is_complete, _nw_connection_send_idempotent_content);
}

static inline void
_swift_nw_connection_send(nw_connection_t connection, _Nullable dispatch_data_t content, nw_content_context_t context, bool is_complete, __swift_nw_connection_send_completion_t completion) {
	nw_connection_send(connection, content, context, is_complete, completion);
}

API_AVAILABLE(macos(10.14), ios(12.0), watchos(5.0), tvos(12.0))
_Nullable SWIFT_NW_RETURNS_RETAINED nw_endpoint_t
nw_endpoint_create_unix(const char *path);

API_AVAILABLE(macos(10.14), ios(12.0), watchos(5.0), tvos(12.0))
_Nullable SWIFT_NW_RETURNS_RETAINED nw_interface_t
nw_endpoint_copy_interface(nw_endpoint_t endpoint);

API_AVAILABLE(macos(10.14), ios(12.0), watchos(5.0), tvos(12.0))
void
nw_endpoint_set_interface(nw_endpoint_t endpoint,
						  _Nullable nw_interface_t interface);

API_AVAILABLE(macos(10.14), ios(12.0), watchos(5.0), tvos(12.0))
_Nullable SWIFT_NW_RETURNS_RETAINED nw_interface_t
nw_interface_create_with_name(const char *interface_name);

API_AVAILABLE(macos(10.14), ios(12.0), watchos(5.0), tvos(12.0))
_Nullable SWIFT_NW_RETURNS_RETAINED nw_interface_t
nw_interface_create_with_index(uint32_t interface_index);

API_AVAILABLE(macos(10.14), ios(12.0), watchos(5.0), tvos(12.0))
SWIFT_NW_RETURNS_RETAINED NSData * _Nullable
NWCreateNSDataFromDispatchData(_Nullable dispatch_data_t data);

API_AVAILABLE(macos(10.14), ios(12.0), watchos(5.0), tvos(12.0))
_Nullable SWIFT_NW_RETURNS_RETAINED dispatch_data_t
NWCreateDispatchDataFromNSData(NSData * _Nullable data);

API_AVAILABLE(macos(10.14), ios(12.0), watchos(5.0), tvos(12.0))
const char *
nwlog_get_string_for_dns_service_error(int32_t err);

#pragma clang assume_nonnull end

#endif // SWIFT_STDLIB_SHIMS_NETWORKSHIMS_H

