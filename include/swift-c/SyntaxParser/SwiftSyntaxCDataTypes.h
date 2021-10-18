//===------------------ SwiftSyntaxCDataTypes.h -------------------*- C -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// There are two copies of this file in the Swift compiler repository and in  //
// the SwiftSyntax repository. They need to always share the same contents.   //
//                                                                            //
// The reason behind this is that the SwiftSyntax module should be able to    //
// build with no dependencies to the compiler repo (in particular not         //
// _InternalSwiftSyntaxParser) so that it can be used for code generation     //
// without a matching toolchain. But to make SwiftSyntax parsing efficient,   //
// we need to create RawSyntax nodes from the C nodes without any conversion  //
// and thus SwiftSyntax needs to have knowledge about the layout of these C   //
// types. Thus the two copies of the file. The equality of these files is     //
// checked in CI (verify the source matches) and at runtime by                //
// SwiftSyntaxParser (verify that a hash generated from the layouts matches)  //
//===----------------------------------------------------------------------===//

#ifndef SWIFT_C_SYNTAX_C_DATA_TYPES_H
#define SWIFT_C_SYNTAX_C_DATA_TYPES_H

#include <stdbool.h>

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
  /// Represents the range for the node, including trivia.
  swiftparse_range_t range;
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
  /// The syntax kind. A value of '0' means this is a token node.
  swiftparse_syntax_kind_t kind;
  bool present;
} swiftparse_syntax_node_t;

#endif
