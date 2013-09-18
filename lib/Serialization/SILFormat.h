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
    SIL_INST_TODO,
    SIL_FUNC_NAMES,
    SIL_FUNC_OFFSETS
  };

  using SILInstTodoLayout = BCRecordLayout<
    SIL_INST_TODO,
    SILInstOpCodeField
  >;

  using FuncListLayout = BCRecordLayout<
    SIL_FUNC_NAMES,
    BCVBR<16>,  // table offset within the blob
    BCBlob // map from identifier strings to func IDs.
  >;

  using FuncOffsetLayout = BCRecordLayout<
    SIL_FUNC_OFFSETS,
    BCArray<BitOffsetField>
  >;

  using SILFunctionLayout = BCRecordLayout<
    SIL_FUNCTION,
    BCFixed<2>,        //linkage
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
    TypeIDField,          // callee type
    SILTypeCategoryField,
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
