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

#include "swift/Serialization/ModuleFormat.h"

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

enum SILLinkageEncoding : uint8_t {
  SIL_LINKAGE_PUBLIC,
  SIL_LINKAGE_HIDDEN,
  SIL_LINKAGE_SHARED,
  SIL_LINKAGE_PRIVATE,
  SIL_LINKAGE_PUBLIC_EXTERNAL,
  SIL_LINKAGE_HIDDEN_EXTERNAL,
};
using SILLinkageField = BCFixed<3>;

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
    SIL_GLOBALVAR_OFFSETS,
    SIL_WITNESSTABLE_NAMES,
    SIL_WITNESSTABLE_OFFSETS
  };

  using ListLayout = BCGenericRecordLayout<
    BCFixed<4>,  // record ID
    BCVBR<16>,  // table offset within the blob
    BCBlob      // map from identifier strings to IDs.
  >;

  using OffsetLayout = BCGenericRecordLayout<
    BCFixed<4>,  // record ID
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
    SIL_INST_CAST, // It has a cast kind instead of an attribute.
    SIL_INIT_EXISTENTIAL,
    SIL_WITNESSTABLE,
    SIL_WITNESS_METHOD_ENTRY,
    // To avoid overlapping with BOUND_GENERIC_SUBSTITUTION, we start from +1.
    // FIXME: another option is to start SIL_FUNCTION at a large number.
    SIL_WITNESS_BASE_ENTRY = decls_block::BOUND_GENERIC_SUBSTITUTION + 1,
    SIL_WITNESS_ASSOC_PROTOCOL,
    SIL_WITNESS_ASSOC_ENTRY,

    // We also share these layouts from the decls block. Their enumerators must
    // not overlap with ours.
    BOUND_GENERIC_SUBSTITUTION = decls_block::BOUND_GENERIC_SUBSTITUTION,
    NO_CONFORMANCE = decls_block::NO_CONFORMANCE,
    NORMAL_PROTOCOL_CONFORMANCE = decls_block::NORMAL_PROTOCOL_CONFORMANCE,
    SPECIALIZED_PROTOCOL_CONFORMANCE
      = decls_block::SPECIALIZED_PROTOCOL_CONFORMANCE,
    INHERITED_PROTOCOL_CONFORMANCE
      = decls_block::INHERITED_PROTOCOL_CONFORMANCE,
    GENERIC_PARAM_LIST = decls_block::GENERIC_PARAM_LIST,
    GENERIC_PARAM = decls_block::GENERIC_PARAM,
    GENERIC_REQUIREMENT = decls_block::GENERIC_REQUIREMENT,
    LAST_GENERIC_REQUIREMENT = decls_block::LAST_GENERIC_REQUIREMENT,
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

  using WitnessTableLayout = BCRecordLayout<
    SIL_WITNESSTABLE,
    TypeIDField          // Conforming Type.
    // Normal Protocol Conformance will be serialized immediately after.
  >;

  using WitnessMethodEntryLayout = BCRecordLayout<
    SIL_WITNESS_METHOD_ENTRY,
    DeclIDField,  // SILFunction name
    BCArray<ValueIDField> // SILDeclRef
  >;

  using WitnessBaseEntryLayout = BCRecordLayout<
    SIL_WITNESS_BASE_ENTRY,
    DeclIDField,  // ID of protocol decl
    TypeIDField   // ID of conforming type
    // Protocol Conformance will be serialized immediately after.
  >;

  using WitnessAssocProtocolLayout = BCRecordLayout<
    SIL_WITNESS_ASSOC_PROTOCOL,
    DeclIDField,  // ID of AssocaitedTypeDecl
    DeclIDField,  // ID of ProtocolDecl
    TypeIDField   // ID of conforming type
    // Protocol Conformance will be serialized immediately after.
  >;

  using WitnessAssocEntryLayout = BCRecordLayout<
    SIL_WITNESS_ASSOC_ENTRY,
    DeclIDField,  // ID of AssociatedTypeDecl
    TypeIDField
  >;

  using GlobalVarLayout = BCRecordLayout<
    SIL_GLOBALVAR,
    SILLinkageField,
    TypeIDField
  >;

  using SILFunctionLayout = BCRecordLayout<
    SIL_FUNCTION,
    SILLinkageField,
    BCFixed<1>,        // transparent
    TypeIDField
                       // followed by generic param list, if any
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

  // SIL instructions that construct existential values.
  using SILInitExistentialLayout = BCRecordLayout<
    SIL_INIT_EXISTENTIAL,
    SILInstOpCodeField,   // opcode
    TypeIDField,          // result type
    SILTypeCategoryField, // result type category
    TypeIDField,          // operand type
    SILTypeCategoryField, // operand type category
    ValueIDField,         // operand id
    SILValueResultField,  // operand result id
    BCFixed<32>           // number of conformances
                          // followed by conformances
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
    BCFixed<3>,          // Optional attributes
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
