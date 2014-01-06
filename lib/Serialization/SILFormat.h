//===--- SILFormat.h - The internals of serialized SILs --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file Contains various constants and helper types to deal with serialized
/// SILs.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_SILFORMAT_H
#define SWIFT_SERIALIZATION_SILFORMAT_H

#include "ModuleFormat.h"

namespace swift {
namespace serialization {

using ValueID = DeclID;
using ValueIDField = DeclIDField;

using SILInstOpCodeField = BCFixed<8>;
using SILTypeCategoryField = BCFixed<2>;
using SILValueResultField = BCFixed<8>;

enum SILStringEncoding : uint8_t {
  SIL_UTF8,
  SIL_UTF16
};

/// The record types within the "sil-index" block.
///
/// \sa SIL_INDEX_BLOCK_ID
namespace sil_index_block {
  // These IDs must \em not be renumbered or reordered without incrementing
  // VERSION_MAJOR.
  enum RecordKind {
    SIL_FUNC_NAMES = 1,
    SIL_FUNC_OFFSETS,
    SIL_VTABLE_NAMES,
    SIL_VTABLE_OFFSETS,
    SIL_GLOBALVAR_NAMES,
    SIL_GLOBALVAR_OFFSETS
  };

  using ListLayout = BCGenericRecordLayout<
    BCFixed<3>,  // record ID
    BCVBR<16>,  // table offset within the blob
    BCBlob      // map from identifier strings to IDs.
  >;

  using OffsetLayout = BCGenericRecordLayout<
    BCFixed<3>,  // record ID
    BCArray<BitOffsetField>
  >;
}

/// The record types within the "sil" block.
///
/// \sa SIL_BLOCK_ID
namespace sil_block {
  // These IDs must \em not be renumbered or reordered without incrementing
  // VERSION_MAJOR.
  enum RecordKind : uint8_t {
    SIL_FUNCTION = 1,
    SIL_BASIC_BLOCK,
    SIL_ONE_VALUE_ONE_OPERAND,
    SIL_ONE_TYPE,
    SIL_ONE_OPERAND,
    SIL_ONE_TYPE_ONE_OPERAND,
    SIL_ONE_TYPE_VALUES,
    SIL_TWO_OPERANDS,
    SIL_INST_APPLY,
    SIL_INST_NO_OPERAND,
    SIL_VTABLE,
    SIL_VTABLE_ENTRY,
    SIL_GLOBALVAR,
    SIL_INST_CAST // It has a cast kind instead of an attribute.
  };

  using SILInstNoOperandLayout = BCRecordLayout<
    SIL_INST_NO_OPERAND,
    SILInstOpCodeField
  >;

  using VTableLayout = BCRecordLayout<
    SIL_VTABLE,
    DeclIDField   // Class Decl
  >;

  using VTableEntryLayout = BCRecordLayout<
    SIL_VTABLE_ENTRY,
    DeclIDField,  // SILFunction name
    BCArray<ValueIDField> // SILDeclRef
  >;

  using GlobalVarLayout = BCRecordLayout<
    SIL_GLOBALVAR,
    BCFixed<2>,        // linkage
    BCFixed<1>,        // Optional attributes
    TypeIDField
  >;

  using SILFunctionLayout = BCRecordLayout<
    SIL_FUNCTION,
    BCFixed<2>,        // linkage
    BCFixed<1>,        // Optional attributes
    TypeIDField
  >;

  // Has an optional argument list where each argument is a typed valueref.
  using SILBasicBlockLayout = BCRecordLayout<
    SIL_BASIC_BLOCK,
    BCArray<DeclIDField> // The array contains type-value pairs.
  >;

  // SIL instructions with one valueref and one typed valueref.
  // (store)
  using SILOneValueOneOperandLayout = BCRecordLayout<
    SIL_ONE_VALUE_ONE_OPERAND,
    SILInstOpCodeField,
    BCFixed<2>,          // Optional attributes
    ValueIDField,
    SILValueResultField,
    TypeIDField,
    SILTypeCategoryField,
    ValueIDField,
    SILValueResultField
  >;

  // SIL instructions with one type and one typed valueref.
  using SILOneTypeOneOperandLayout = BCRecordLayout<
    SIL_ONE_TYPE_ONE_OPERAND,
    SILInstOpCodeField,
    BCFixed<2>,          // Optional attributes
    TypeIDField,
    SILTypeCategoryField,
    TypeIDField,
    SILTypeCategoryField,
    ValueIDField,
    SILValueResultField
  >;

  // SIL Cast instructions with a cast kind, one type and one typed valueref.
  using SILInstCastLayout = BCRecordLayout<
    SIL_INST_CAST,
    SILInstOpCodeField,
    BCFixed<4>,          // Cast kind
    TypeIDField,
    SILTypeCategoryField,
    TypeIDField,
    SILTypeCategoryField,
    ValueIDField,
    SILValueResultField
  >;

  // SIL instructions with one type and a list of values.
  using SILOneTypeValuesLayout = BCRecordLayout<
    SIL_ONE_TYPE_VALUES,
    SILInstOpCodeField,
    TypeIDField,
    SILTypeCategoryField,
    BCArray<ValueIDField>
  >;

  using SILInstApplyLayout = BCRecordLayout<
    SIL_INST_APPLY,
    BCFixed<1>,           // is partial apply?
    BCFixed<1>,           // transparent
    BCFixed<31>,          // num substitutions
    TypeIDField,          // callee unsubstituted type
    TypeIDField,          // callee substituted type
    ValueIDField,         // callee value
    SILValueResultField,
    BCArray<ValueIDField> // a list of arguments
  >;

  // SIL instructions with one type. (alloc_stack)
  using SILOneTypeLayout = BCRecordLayout<
    SIL_ONE_TYPE,
    SILInstOpCodeField,
    TypeIDField,
    SILTypeCategoryField
  >;

  // SIL instructions with one typed valueref. (dealloc_stack, return)
  using SILOneOperandLayout = BCRecordLayout<
    SIL_ONE_OPERAND,
    SILInstOpCodeField,
    BCFixed<2>,          // Optional attributes
    TypeIDField,
    SILTypeCategoryField,
    ValueIDField,
    SILValueResultField
  >;

  // SIL instructions with two typed values.
  using SILTwoOperandsLayout = BCRecordLayout<
    SIL_TWO_OPERANDS,
    SILInstOpCodeField,
    BCFixed<2>,          // Optional attributes
    TypeIDField,
    SILTypeCategoryField,
    ValueIDField,
    SILValueResultField,
    TypeIDField,
    SILTypeCategoryField,
    ValueIDField,
    SILValueResultField
  >;
}

} // end namespace serialization
} // end namespace swift

#endif
