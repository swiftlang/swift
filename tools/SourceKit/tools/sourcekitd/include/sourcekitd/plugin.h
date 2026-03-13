//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_SOURCEKITD_PLUGIN_H
#define LLVM_SOURCEKITD_PLUGIN_H

#include "sourcekitd/sourcekitd.h"

SOURCEKITD_BEGIN_DECLS

typedef void *sourcekitd_variant_functions_t;

typedef sourcekitd_variant_type_t (*sourcekitd_variant_functions_get_type_t)(
    sourcekitd_variant_t obj);
typedef bool (*sourcekitd_variant_functions_array_apply_t)(
    sourcekitd_variant_t array, sourcekitd_variant_array_applier_f_t applier,
    void *context);
typedef bool (*sourcekitd_variant_functions_array_get_bool_t)(
    sourcekitd_variant_t array, size_t index);
typedef double (*sourcekitd_variant_functions_array_get_double_t)(
    sourcekitd_variant_t array, size_t index);
typedef size_t (*sourcekitd_variant_functions_array_get_count_t)(
    sourcekitd_variant_t array);
typedef int64_t (*sourcekitd_variant_functions_array_get_int64_t)(
    sourcekitd_variant_t array, size_t index);
typedef const char *(*sourcekitd_variant_functions_array_get_string_t)(
    sourcekitd_variant_t array, size_t index);
typedef sourcekitd_uid_t (*sourcekitd_variant_functions_array_get_uid_t)(
    sourcekitd_variant_t array, size_t index);
typedef sourcekitd_variant_t (*sourcekitd_variant_functions_array_get_value_t)(
    sourcekitd_variant_t array, size_t index);
typedef bool (*sourcekitd_variant_functions_bool_get_value_t)(
    sourcekitd_variant_t obj);
typedef double (*sourcekitd_variant_functions_double_get_value_t)(
    sourcekitd_variant_t obj);
typedef bool (*sourcekitd_variant_functions_dictionary_apply_t)(
    sourcekitd_variant_t dict,
    sourcekitd_variant_dictionary_applier_f_t applier, void *context);
typedef bool (*sourcekitd_variant_functions_dictionary_get_bool_t)(
    sourcekitd_variant_t dict, sourcekitd_uid_t key);
typedef double (*sourcekitd_variant_functions_dictionary_get_double_t)(
    sourcekitd_variant_t dict, sourcekitd_uid_t key);
typedef int64_t (*sourcekitd_variant_functions_dictionary_get_int64_t)(
    sourcekitd_variant_t dict, sourcekitd_uid_t key);
typedef const char *(*sourcekitd_variant_functions_dictionary_get_string_t)(
    sourcekitd_variant_t dict, sourcekitd_uid_t key);
typedef sourcekitd_variant_t (
    *sourcekitd_variant_functions_dictionary_get_value_t)(
    sourcekitd_variant_t dict, sourcekitd_uid_t key);
typedef sourcekitd_uid_t (*sourcekitd_variant_functions_dictionary_get_uid_t)(
    sourcekitd_variant_t dict, sourcekitd_uid_t key);
typedef size_t (*sourcekitd_variant_functions_string_get_length_t)(
    sourcekitd_variant_t obj);
typedef const char *(*sourcekitd_variant_functions_string_get_ptr_t)(
    sourcekitd_variant_t obj);
typedef int64_t (*sourcekitd_variant_functions_int64_get_value_t)(
    sourcekitd_variant_t obj);
typedef sourcekitd_uid_t (*sourcekitd_variant_functions_uid_get_value_t)(
    sourcekitd_variant_t obj);
typedef size_t (*sourcekitd_variant_functions_data_get_size_t)(
    sourcekitd_variant_t obj);
typedef const void *(*sourcekitd_variant_functions_data_get_ptr_t)(
    sourcekitd_variant_t obj);

SOURCEKITD_PUBLIC SOURCEKITD_WARN_RESULT sourcekitd_variant_functions_t
sourcekitd_variant_functions_create();

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_get_type(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_get_type_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_array_apply(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_apply_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_array_get_bool(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_get_bool_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_array_get_double(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_get_double_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_array_get_count(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_get_count_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_array_get_int64(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_get_int64_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_array_get_string(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_get_string_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_array_get_uid(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_get_uid_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_array_get_value(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_array_get_value_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_bool_get_value(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_bool_get_value_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_double_get_value(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_double_get_value_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_dictionary_apply(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_dictionary_apply_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_dictionary_get_bool(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_dictionary_get_bool_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_dictionary_get_double(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_dictionary_get_double_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_dictionary_get_int64(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_dictionary_get_int64_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_dictionary_get_string(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_dictionary_get_string_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_dictionary_get_value(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_dictionary_get_value_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_dictionary_get_uid(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_dictionary_get_uid_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_string_get_length(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_string_get_length_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_string_get_ptr(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_string_get_ptr_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_int64_get_value(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_int64_get_value_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_uid_get_value(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_uid_get_value_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_data_get_size(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_data_get_size_t f);
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_variant_functions_set_data_get_ptr(
    sourcekitd_variant_functions_t funcs,
    sourcekitd_variant_functions_data_get_ptr_t f);

#if SOURCEKITD_HAS_BLOCKS

/// Legacy request handler predating cancellation support. Should be removed
/// when all clients have migrated to sourcekitd_cancellable_request_handler_t.
typedef bool (^sourcekitd_request_handler_t)(sourcekitd_object_t,
                                             void (^)(sourcekitd_response_t));
/// Handle the request specified by the \c sourcekitd_object_t and keep track
/// of it using the \c sourcekitd_request_handle_t. If the cancellation handler
/// specified by \c sourcekitd_plugin_initialize_register_cancellation_handler
/// is called with the this request handle, the request should be cancelled.
typedef bool (^sourcekitd_cancellable_request_handler_t)(
    sourcekitd_object_t, sourcekitd_request_handle_t,
    void (^)(sourcekitd_response_t));
typedef void (^sourcekitd_cancellation_handler_t)(sourcekitd_request_handle_t);
typedef sourcekitd_uid_t (*sourcekitd_uid_get_from_cstr_t)(const char *string);
typedef const char *(*sourcekitd_uid_get_string_ptr_t)(sourcekitd_uid_t);

typedef void *sourcekitd_plugin_initialize_params_t;
typedef void (*sourcekitd_plugin_initialize_t)(
    sourcekitd_plugin_initialize_params_t);
typedef void (*sourcekitd_plugin_initialize_2_t)(
    sourcekitd_plugin_initialize_params_t, const char *sourcekit_path);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL bool
    sourcekitd_plugin_initialize_is_client_only(
        sourcekitd_plugin_initialize_params_t);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL uint64_t
    sourcekitd_plugin_initialize_custom_buffer_start(
        sourcekitd_plugin_initialize_params_t);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL sourcekitd_uid_get_from_cstr_t
    sourcekitd_plugin_initialize_uid_get_from_cstr(
        sourcekitd_plugin_initialize_params_t);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL sourcekitd_uid_get_string_ptr_t
    sourcekitd_plugin_initialize_uid_get_string_ptr(
        sourcekitd_plugin_initialize_params_t);

/// Legacy registration of request handlers predating cancellation support.
/// Should be removed when all clients have migrated to cancellable request
/// handlers.
SOURCEKITD_DEPRECATED(
    "Use sourcekitd_plugin_initialize_register_cancellable_request_handler "
    "instead")
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
    sourcekitd_plugin_initialize_register_request_handler(
        sourcekitd_plugin_initialize_params_t, sourcekitd_request_handler_t);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
    sourcekitd_plugin_initialize_register_cancellable_request_handler(
        sourcekitd_plugin_initialize_params_t,
        sourcekitd_cancellable_request_handler_t);

/// Adds a function that will be called when a request is cancelled.
/// The cancellation handler is called even for cancelled requests that are
/// handled by sourcekitd itself and not the plugin. If the plugin doesn't know
/// the request handle to be cancelled, it should ignore the cancellation
/// request.
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
    sourcekitd_plugin_initialize_register_cancellation_handler(
        sourcekitd_plugin_initialize_params_t,
        sourcekitd_cancellation_handler_t);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_plugin_initialize_register_custom_buffer(
    sourcekitd_plugin_initialize_params_t, uint64_t kind,
    sourcekitd_variant_functions_t funcs);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void *
    sourcekitd_plugin_initialize_get_swift_ide_inspection_instance(
        sourcekitd_plugin_initialize_params_t);

#endif // SOURCEKITD_HAS_BLOCKS

//============================================================================//
// Request
//============================================================================//

SOURCEKITD_PUBLIC SOURCEKITD_WARN_RESULT sourcekitd_variant_type_t
sourcekitd_request_get_type(sourcekitd_object_t obj);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL2 SOURCEKITD_WARN_RESULT sourcekitd_object_t
sourcekitd_request_dictionary_get_value(sourcekitd_object_t dict,
                                        sourcekitd_uid_t key);

/// The underlying C string for the specified key. NULL if the value for the
/// specified key is not a C string value or if there is no value for the
/// specified key.
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL SOURCEKITD_WARN_RESULT const char *
sourcekitd_request_dictionary_get_string(sourcekitd_object_t dict,
                                         sourcekitd_uid_t key);

/// The underlying \c int64 value for the specified key. 0 if the
/// value for the specified key is not an integer value or if there is no
/// value for the specified key.
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL SOURCEKITD_WARN_RESULT int64_t
sourcekitd_request_dictionary_get_int64(sourcekitd_object_t dict,
                                        sourcekitd_uid_t key);

/// The underlying \c bool value for the specified key. false if the
/// value for the specified key is not a Boolean value or if there is no
/// value for the specified key.
SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL SOURCEKITD_WARN_RESULT bool
sourcekitd_request_dictionary_get_bool(sourcekitd_object_t dict,
                                       sourcekitd_uid_t key);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL sourcekitd_uid_t
sourcekitd_request_dictionary_get_uid(sourcekitd_object_t dict,
                                      sourcekitd_uid_t key);

SOURCEKITD_PUBLIC SOURCEKITD_WARN_RESULT size_t
sourcekitd_request_array_get_count(sourcekitd_object_t array);

SOURCEKITD_PUBLIC SOURCEKITD_WARN_RESULT sourcekitd_object_t
sourcekitd_request_array_get_value(sourcekitd_object_t array, size_t index);

SOURCEKITD_PUBLIC SOURCEKITD_WARN_RESULT const char *
sourcekitd_request_array_get_string(sourcekitd_object_t array, size_t index);

SOURCEKITD_PUBLIC SOURCEKITD_WARN_RESULT int64_t
sourcekitd_request_array_get_int64(sourcekitd_object_t array, size_t index);

SOURCEKITD_PUBLIC SOURCEKITD_WARN_RESULT bool
sourcekitd_request_array_get_bool(sourcekitd_object_t array, size_t index);

SOURCEKITD_PUBLIC SOURCEKITD_WARN_RESULT sourcekitd_uid_t
sourcekitd_request_array_get_uid(sourcekitd_object_t array, size_t index);

SOURCEKITD_PUBLIC SOURCEKITD_WARN_RESULT int64_t
sourcekitd_request_int64_get_value(sourcekitd_object_t obj);

SOURCEKITD_PUBLIC SOURCEKITD_WARN_RESULT bool
sourcekitd_request_bool_get_value(sourcekitd_object_t obj);

SOURCEKITD_PUBLIC SOURCEKITD_WARN_RESULT size_t
sourcekitd_request_string_get_length(sourcekitd_object_t obj);

SOURCEKITD_PUBLIC SOURCEKITD_WARN_RESULT const char *
sourcekitd_request_string_get_ptr(sourcekitd_object_t obj);

SOURCEKITD_PUBLIC SOURCEKITD_WARN_RESULT sourcekitd_uid_t
sourcekitd_request_uid_get_value(sourcekitd_object_t obj);

//============================================================================//
// Response
//============================================================================//

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL1 sourcekitd_response_t
sourcekitd_response_retain(sourcekitd_response_t object);

SOURCEKITD_PUBLIC
sourcekitd_response_t sourcekitd_response_error_create(sourcekitd_error_t kind,
                                                       const char *description);

SOURCEKITD_PUBLIC SOURCEKITD_WARN_RESULT sourcekitd_response_t
sourcekitd_response_dictionary_create(const sourcekitd_uid_t *keys,
                                      const sourcekitd_response_t *values,
                                      size_t count);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_response_dictionary_set_value(sourcekitd_response_t dict,
                                         sourcekitd_uid_t key,
                                         sourcekitd_response_t value);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_response_dictionary_set_string(sourcekitd_response_t dict,
                                          sourcekitd_uid_t key,
                                          const char *string);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_response_dictionary_set_stringbuf(sourcekitd_response_t dict,
                                             sourcekitd_uid_t key,
                                             const char *buf, size_t length);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL1 SOURCEKITD_NONNULL2 void
sourcekitd_response_dictionary_set_int64(sourcekitd_response_t dict,
                                         sourcekitd_uid_t key, int64_t val);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL1 SOURCEKITD_NONNULL2 void
sourcekitd_response_dictionary_set_bool(sourcekitd_response_t dict,
                                        sourcekitd_uid_t key, bool val);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL1 SOURCEKITD_NONNULL2 void
sourcekitd_response_dictionary_set_double(sourcekitd_response_t dict,
                                          sourcekitd_uid_t key, double val);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_response_dictionary_set_uid(sourcekitd_response_t dict,
                                       sourcekitd_uid_t key,
                                       sourcekitd_uid_t uid);

SOURCEKITD_PUBLIC SOURCEKITD_WARN_RESULT sourcekitd_response_t
sourcekitd_response_array_create(const sourcekitd_response_t *objects,
                                 size_t count);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL1 SOURCEKITD_NONNULL3 void
sourcekitd_response_array_set_value(sourcekitd_response_t array, size_t index,
                                    sourcekitd_response_t value);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL1 SOURCEKITD_NONNULL3 void
sourcekitd_response_array_set_string(sourcekitd_response_t array, size_t index,
                                     const char *string);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL1 SOURCEKITD_NONNULL3 void
sourcekitd_response_array_set_stringbuf(sourcekitd_response_t array,
                                        size_t index, const char *buf,
                                        size_t length);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL1 void
sourcekitd_response_array_set_int64(sourcekitd_response_t array, size_t index,
                                    int64_t val);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL1 void
sourcekitd_response_array_set_double(sourcekitd_response_t array, size_t index,
                                     double val);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL1 SOURCEKITD_NONNULL3 void
sourcekitd_response_array_set_uid(sourcekitd_response_t array, size_t index,
                                  sourcekitd_uid_t uid);

SOURCEKITD_PUBLIC SOURCEKITD_NONNULL_ALL void
sourcekitd_response_dictionary_set_custom_buffer(sourcekitd_response_t dict,
                                                 sourcekitd_uid_t key,
                                                 const void *ptr, size_t size);

SOURCEKITD_END_DECLS

#endif
