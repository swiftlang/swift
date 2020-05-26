//===--- SwiftSyntaxParser.h - C API for Swift Syntax Parsing -----*- C -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This C API is primarily intended to serve as the Swift parsing component
// of SwiftSyntax (https://github.com/apple/swift-syntax).
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_C_SYNTAX_PARSER_H
#define SWIFT_C_SYNTAX_PARSER_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/// The version constants for the SwiftSyntaxParser C API.
/// SWIFTPARSE_VERSION_MINOR should increase when there are API additions.
/// SWIFTPARSE_VERSION_MAJOR is intended for "major" source/ABI breaking changes.
#define SWIFTPARSE_VERSION_MAJOR 0
#define SWIFTPARSE_VERSION_MINOR 1

#define SWIFTPARSE_VERSION_ENCODE(major, minor) ( \
      ((major) * 10000)                           \
    + ((minor) *     1))

#define SWIFTPARSE_VERSION SWIFTPARSE_VERSION_ENCODE( \
    SWIFTPARSE_VERSION_MAJOR,                         \
    SWIFTPARSE_VERSION_MINOR )

#define SWIFTPARSE_VERSION_STRINGIZE_(major, minor)   \
    #major"."#minor
#define SWIFTPARSE_VERSION_STRINGIZE(major, minor)    \
    SWIFTPARSE_VERSION_STRINGIZE_(major, minor)

#define SWIFTPARSE_VERSION_STRING SWIFTPARSE_VERSION_STRINGIZE( \
    SWIFTPARSE_VERSION_MAJOR,                                   \
    SWIFTPARSE_VERSION_MINOR)

#ifdef  __cplusplus
# define SWIFTPARSE_BEGIN_DECLS  extern "C" {
# define SWIFTPARSE_END_DECLS    }
#else
# define SWIFTPARSE_BEGIN_DECLS
# define SWIFTPARSE_END_DECLS
#endif

#ifndef SWIFTPARSE_PUBLIC
# ifdef _WIN32
#  ifdef libSwiftSyntaxParser_EXPORTS
#    define SWIFTPARSE_PUBLIC __declspec(dllexport)
#  else
#    define SWIFTPARSE_PUBLIC __declspec(dllimport)
#  endif
# else
#  define SWIFTPARSE_PUBLIC
# endif
#endif

#ifndef __has_feature
# define __has_feature(x) 0
#endif

#if !__has_feature(blocks)
# error -fblocks is a requirement to use this library
#endif

SWIFTPARSE_BEGIN_DECLS

//=== Syntax Data Types ---------------------------------------------------===//

/// Offset+length in UTF8 bytes.
typedef struct {
  uint32_t offset;
  uint32_t length;
} swiftparse_range_t;

typedef uint8_t swiftparse_trivia_kind_t;
typedef uint8_t swiftparse_token_kind_t;
typedef uint16_t swiftparse_syntax_kind_t;

/// This is for the client to provide an opaque pointer that the parser will
/// associate with a syntax node.
typedef void *swiftparse_client_node_t;

typedef struct {
  /// The length in source this trivia piece occupies, in UTF8 bytes.
  uint32_t length;
  swiftparse_trivia_kind_t kind;
} swiftparse_trivia_piece_t;

typedef struct {
  const swiftparse_trivia_piece_t *leading_trivia;
  const swiftparse_trivia_piece_t *trailing_trivia;
  uint16_t leading_trivia_count;
  uint16_t trailing_trivia_count;
  swiftparse_token_kind_t kind;
} swiftparse_token_data_t;

typedef struct {
  const swiftparse_client_node_t *nodes;
  uint32_t nodes_count;
} swiftparse_layout_data_t;

typedef struct {
  union {
    swiftparse_token_data_t token_data;
    swiftparse_layout_data_t layout_data;
  };
  /// Represents the range for the node. For a token node the range includes
  /// the trivia associated with it.
  swiftparse_range_t range;
  /// The syntax kind. A value of '0' means this is a token node.
  swiftparse_syntax_kind_t kind;
  bool present;
} swiftparse_syntax_node_t;

//=== Parser Functions ----------------------------------------------------===//

/// Container of the configuration state for parsing.
///
/// It keeps track of the callback blocks for parsing. Any other additional
/// configuration option (like parsing options) could be set by a adding a
/// function that accepts a \c swiftparse_parser_t object and modifies its
/// state.
typedef void *swiftparse_parser_t;

SWIFTPARSE_PUBLIC swiftparse_parser_t
swiftparse_parser_create(void);

SWIFTPARSE_PUBLIC void
swiftparse_parser_dispose(swiftparse_parser_t);

/// Invoked by the parser when a syntax node is parsed. The client should
/// return a pointer to associate with that particular node.
typedef swiftparse_client_node_t
    (^swiftparse_node_handler_t)(const swiftparse_syntax_node_t *);

/// Set the \c swiftparse_node_handler_t block to be used by the parser.
///
/// It is required to set a \c swiftparse_node_handler_t block before any calls
/// to \c swiftparse_parse_string. \c swiftparse_parser_set_node_handler can be
/// called multiple times to change the block before subsequent parses.
SWIFTPARSE_PUBLIC void
swiftparse_parser_set_node_handler(swiftparse_parser_t,
                                   swiftparse_node_handler_t);

typedef struct {
  /// Length of the source region in UTF8 bytes that the parser should skip.
  /// If it is set to 0 it indicates that the parser should continue parsing
  /// and the \c node object is ignored.
  size_t length;
  /// Node to associate for the skipped source region. It will be ignored if
  /// \c length is 0.
  swiftparse_client_node_t node;
} swiftparse_lookup_result_t;

/// Invoked by the parser at certain points to query whether a source region
/// should be skipped. See \c swiftparse_lookup_result_t.
typedef swiftparse_lookup_result_t
    (^swiftparse_node_lookup_t)(size_t offset, swiftparse_syntax_kind_t);

/// Set the \c swiftparse_node_lookup_t block to be used by the parser.
///
/// It is not required to set a \c swiftparse_node_lookup_t block before calling
/// \c swiftparse_parse_string. Not setting a \c swiftparse_node_lookup_t block
/// has same semantics as never skipping any source regions.
/// \c swiftparse_parser_set_node_lookup can be called multiple times to change
/// the block before subsequent parses.
SWIFTPARSE_PUBLIC void
swiftparse_parser_set_node_lookup(swiftparse_parser_t,
                                  swiftparse_node_lookup_t);

/// Parse the provided \p source and invoke the callback that was set via
/// \c swiftparse_parser_set_node_handler as each syntax node is parsed.
///
/// Syntax nodes are provided in a depth-first, source order. For example,
/// token nodes will be provided ahead of the syntax node whose layout they are
/// a part of. The memory that \c swiftparse_syntax_node_t points to is only
/// valid to access for the duration of the \c swiftparse_node_handler_t block
/// execution, the client should copy the data if it wants to persist it beyond
/// the duration of the block.
///
/// The client provides \c swiftparse_client_node_t pointers to associate with
/// each syntax node and the parser will pass back these pointers as part of the
/// \c swiftparse_layout_data_t of the syntax node that they are a part of.
/// \c swiftparse_client_node_t pointers are completely opaque to the parser,
/// it doesn't try to interpret them in any way. There is no requirement that
/// each node gets a unique \c swiftparse_client_node_t, it is up to the client
/// to reuse \c swiftparse_client_node_t pointers as it deems necessary.
///
/// If the \c swiftparse_client_node_t pointers represent managed memory, the
/// semantics of interacting with the parser should be considered as follows:
///
/// * \c swiftparse_node_handler_t and \c swiftparse_node_lookup_t return a
/// \c swiftparse_client_node_t and transfer ownership of the pointer to the
/// parser.
///
/// * The array of \c swiftparse_client_node_t pointers in
/// \c swiftparse_layout_data_t should be considered as the parser transferring
/// ownership of those pointers back to the client.
///
/// * The \c swiftparse_client_node_t returned by \c swiftparse_parse_string
/// should be considered as the parser transferring back ownership of the
/// pointer.
///
/// The parser guarantees that any \c swiftparse_client_node_t, given to the
/// parser by \c swiftparse_node_handler_t or \c swiftparse_node_lookup_t, will
/// be returned back to the client, either via \c swiftparse_layout_data_t or
/// via the return value of \c swiftparse_parse_string.
///
/// \param source a null-terminated UTF8 string buffer.
SWIFTPARSE_PUBLIC swiftparse_client_node_t
swiftparse_parse_string(swiftparse_parser_t, const char *source);

/// Returns a constant string pointer for verification purposes.
///
/// Working as a hash value, the constant string is calculated during compilation
/// time from syntax node declarations.
///
/// During runtime, SwiftSyntax client can compare its own version of the hash
/// value with the result of the function call. Mismatch indicates the parser
/// library isn't compatible with the client side, e.g. added/removed node
/// declarations, etc.
SWIFTPARSE_PUBLIC const char* swiftparse_syntax_structure_versioning_identifier(void);

typedef struct {
  /// Represents the range for the fixit.
  swiftparse_range_t range;
  /// Represent the text for replacement.
  const char* text;
} swiftparse_diagnostic_fixit_t;

typedef enum {
  SWIFTPARSER_DIAGNOSTIC_SEVERITY_ERROR = 0,
  SWIFTPARSER_DIAGNOSTIC_SEVERITY_WARNING = 1,
  SWIFTPARSER_DIAGNOSTIC_SEVERITY_NOTE = 2,
} swiftparser_diagnostic_severity_t;

/// This is for the client to ask further information about a diagnostic that is
/// associated with the pointer.
/// This pointer is only valid to access from within the
/// swiftparse_diagnostic_handler_t block
typedef const void* swiftparser_diagnostic_t;

/// Invoked by the parser when a diagnostic is emitted.
typedef void(^swiftparse_diagnostic_handler_t)(swiftparser_diagnostic_t);

/// Set the \c swiftparse_diagnostic_handler_t block to be used by the parser.
///
/// It isn't required to set a \c swiftparse_diagnostic_handler_t block.
SWIFTPARSE_PUBLIC void
swiftparse_parser_set_diagnostic_handler(swiftparse_parser_t,
                                         swiftparse_diagnostic_handler_t);

/// Get the message of a swiftparser_diagnostic_t
SWIFTPARSE_PUBLIC const char*
swiftparse_diagnostic_get_message(swiftparser_diagnostic_t);

/// Get the source location in byte offset to where the diagnostic is issued
/// in the source buffer.
SWIFTPARSE_PUBLIC
unsigned swiftparse_diagnostic_get_source_loc(swiftparser_diagnostic_t diag);

/// Get the number of fixits of a swiftparser_diagnostic_t
SWIFTPARSE_PUBLIC unsigned
swiftparse_diagnostic_get_fixit_count(swiftparser_diagnostic_t);

/// Get the fixit at the specified index of a swiftparser_diagnostic_t
SWIFTPARSE_PUBLIC swiftparse_diagnostic_fixit_t
swiftparse_diagnostic_get_fixit(swiftparser_diagnostic_t, unsigned);

/// Get the number of highlight ranges of a swiftparser_diagnostic_t
SWIFTPARSE_PUBLIC unsigned
swiftparse_diagnostic_get_range_count(swiftparser_diagnostic_t);

/// Get the highlight range at the specified index of a swiftparser_diagnostic_t
SWIFTPARSE_PUBLIC swiftparse_range_t
swiftparse_diagnostic_get_range(swiftparser_diagnostic_t, unsigned);

/// Get the severity of a swiftparser_diagnostic_t
SWIFTPARSE_PUBLIC swiftparser_diagnostic_severity_t
swiftparse_diagnostic_get_severity(swiftparser_diagnostic_t diag);

SWIFTPARSE_END_DECLS

#endif
