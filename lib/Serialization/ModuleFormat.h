//===--- ModuleFormat.h - The internals of serialized modules ---*- C++ -*-===//
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
/// modules.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_MODULEFORMAT_H
#define SWIFT_SERIALIZATION_MODULEFORMAT_H

#include "swift/Serialization/BCRecordLayout.h"
#include "llvm/Bitcode/BitCodes.h"

namespace swift {
namespace serialization {

/// Magic number for serialized module files.
const unsigned char SIGNATURE[] = { 0xE2, 0x9C, 0xA8, 0x0E };

/// Serialized module format major version number.
///
/// When the format changes in such a way that older compilers will not be
/// able to read the file at all, this number should be incremented.
const unsigned VERSION_MAJOR = 1;

/// Serialized module format minor version number.
///
/// When the format changes in a backwards-compatible way, this number should
/// be incremented.
const unsigned VERSION_MINOR = 0;

using DeclID = Fixnum<31>;
using DeclIDField = BCFixed<31>;

// TypeID must be the same as DeclID because it is stored in the same way.
using TypeID = DeclID;
using TypeIDField = DeclIDField;

using IdentifierID = Fixnum<31>;
using IdentifierIDField = BCFixed<31>;

using BitOffset = Fixnum<31>;
using BitOffsetField = BCFixed<31>;

// CharOffset must be the same as BitOffset because it is stored in the
// same way.
using CharOffset = BitOffset;
using CharOffsetField = BitOffsetField;


// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum AbstractCC : uint8_t {
  C = 0,
  ObjCMethod,
  Freestanding,
  Method
};
using AbstractCCField = BCFixed<2>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum XRefKind : uint8_t {
  SwiftValue = 0,
  SwiftOperator
};
using XRefKindField = BCFixed<1>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum OperatorKind : uint8_t {
  Infix = 0,
  Prefix,
  Postfix
};
static_assert(sizeof(OperatorKind) <= sizeof(TypeID),
              "too many operator kinds");

/// The various types of blocks that can occur within a serialized Swift
/// module.
///
/// These IDs must \em not be renumbered or reordered without incrementing
/// VERSION_MAJOR.
enum BlockID {
  /// The control block, which contains all of the information that needs to
  /// be validated prior to committing to loading the serialized module.
  ///
  /// \sa control_block
  CONTROL_BLOCK_ID = llvm::bitc::FIRST_APPLICATION_BLOCKID,

  /// The input block, which contains all the files this module depends on.
  ///
  /// \sa input_block
  INPUT_BLOCK_ID,

  /// The "decls-and-types" block, which contains all of the declarations that
  /// come from this module.
  ///
  /// Types are also stored here, so that types that just wrap a Decl don't need
  /// a separate entry in the file.
  ///
  /// \sa decls_block
  DECLS_AND_TYPES_BLOCK_ID,

  /// The identifier block, which contains all of the strings used in
  /// identifiers in the module.
  ///
  /// Unlike other blocks in the file, all data within this block is completely
  /// opaque. Offsets into this block should point directly into the blob at a
  /// null-terminated UTF-8 string.
  IDENTIFIER_DATA_BLOCK_ID,

  /// The index block, which contains cross-referencing information for the
  /// module.
  ///
  /// \sa index_block
  INDEX_BLOCK_ID,

  /// An empty block that signals to the reader to throw away the module and
  /// reparse the source files in the input block.
  ///
  /// This is a bring-up hack and will eventually go away.
  FALL_BACK_TO_TRANSLATION_UNIT_ID = 100
};

/// The record types within the control block.
///
/// \sa CONTROL_BLOCK_ID
namespace control_block {
  // These IDs must \em not be renumbered or reordered without incrementing
  // VERSION_MAJOR.
  enum {
    METADATA = 1
  };

  using MetadataLayout = BCRecordLayout<
    METADATA, // ID
    BCFixed<16>, // Module format major version
    BCFixed<16>, // Module format minor version
    BCBlob // misc. version information
  >;
}

/// The record types within the input block.
///
/// \sa INPUT_BLOCK_ID
namespace input_block {
  // These IDs must \em not be renumbered or reordered without incrementing
  // VERSION_MAJOR.
  enum {
    SOURCE_FILE = 1,
    IMPORTED_MODULE
  };

  using SourceFileLayout = BCRecordLayout<
    SOURCE_FILE, // ID
    BCBlob // path
  >;

  using ImportedModuleLayout = BCRecordLayout<
    IMPORTED_MODULE,
    BCBlob // module name
  >;
}

/// The record types within the "decls-and-types" block.
///
/// \sa DECLS_AND_TYPES_BLOCK_ID
namespace decls_block {
  // These IDs must \em not be renumbered or reordered without incrementing
  // VERSION_MAJOR.
  enum : uint8_t {
    NAME_ALIAS_TYPE = 1,
    STRUCT_TYPE,
    PAREN_TYPE,
    TUPLE_TYPE,
    TUPLE_TYPE_ELT,
    IDENTIFIER_TYPE,
    FUNCTION_TYPE,
    METATYPE_TYPE,
    LVALUE_TYPE,

    TYPE_ALIAS_DECL = 100,
    STRUCT_DECL,
    CONSTRUCTOR_DECL,
    VAR_DECL,
    FUNC_DECL,
    PATTERN_BINDING_DECL,

    PAREN_PATTERN = 200,
    TUPLE_PATTERN,
    TUPLE_PATTERN_ELT,
    NAMED_PATTERN,
    ANY_PATTERN,
    TYPED_PATTERN,
    ISA_PATTERN,
    NOMINAL_TYPE_PATTERN,
    VAR_PATTERN,

    XREF = 254,
    DECL_CONTEXT = 255
  };

  using NameAliasTypeLayout = BCRecordLayout<
    NAME_ALIAS_TYPE,
    DeclIDField // typealias decl
  >;

  using StructTypeLayout = BCRecordLayout<
    STRUCT_TYPE,
    DeclIDField, // struct decl
    TypeIDField  // parent
  >;

  using ParenTypeLayout = BCRecordLayout<
    PAREN_TYPE,
    TypeIDField  // inner type
  >;

  using TupleTypeLayout = BCRecordLayout<
    TUPLE_TYPE
  >;

  using TupleTypeEltLayout = BCRecordLayout<
    TUPLE_TYPE_ELT,
    IdentifierIDField, // name
    TypeIDField,       // type
    TypeIDField        // vararg base type, or 0
  >;

  using IdentifierTypeLayout = BCRecordLayout<
    IDENTIFIER_TYPE,
    TypeIDField  // underlying mapped type
    // FIXME: Include the identifier chain for diagnostic purposes.
  >;

  using FunctionTypeLayout = BCRecordLayout<
    FUNCTION_TYPE,
    TypeIDField, // input
    TypeIDField, // output
    AbstractCCField, // calling convention
    BCFixed<1>,  // auto-closure?
    BCFixed<1>,  // thin?
    BCFixed<1>  // block-compatible?
  >;

  using MetaTypeTypeLayout = BCRecordLayout<
    METATYPE_TYPE,
    TypeIDField  // instance type
  >;

  using LValueTypeLayout = BCRecordLayout<
    LVALUE_TYPE,
    TypeIDField, // object type
    BCFixed<1>,  // implicit?
    BCFixed<1>   // non-settable?
  >;

  using TypeAliasLayout = BCRecordLayout<
    TYPE_ALIAS_DECL,
    IdentifierIDField, // name
    DeclIDField, // context decl
    TypeIDField, // underlying type
    BCFixed<1>,  // generic flag
    BCFixed<1>,  // implicit flag
    BCArray<TypeIDField> // inherited types
  >;

  using StructLayout = BCRecordLayout<
    STRUCT_DECL,
    IdentifierIDField, // name
    DeclIDField, // context decl
    BCFixed<1>,  // implicit flag
    BCArray<TypeIDField> // inherited types
  >;

  using ConstructorLayout = BCRecordLayout<
    CONSTRUCTOR_DECL,
    DeclIDField, // context decl
    BCFixed<1>,  // implicit flag
    TypeIDField, // type (signature)
    DeclIDField  // implicit this decl
  >;

  using VarLayout = BCRecordLayout<
    VAR_DECL,
    IdentifierIDField, // name
    DeclIDField,  // context decl
    BCFixed<1>,   // implicit flag
    BCFixed<1>,   // never lvalue flag
    TypeIDField,  // type
    DeclIDField,  // getter
    DeclIDField,  // setter
    DeclIDField   // overridden decl
  >;

  using FuncLayout = BCRecordLayout<
    FUNC_DECL,
    IdentifierIDField, // name
    DeclIDField,  // context decl
    BCFixed<1>,   // implicit flag
    BCFixed<1>,   // never lvalue flag
    TypeIDField,  // type (signature)
    BCFixed<1>,   // class method
    DeclIDField,  // associated decl (for get/set or operators)
    DeclIDField   // overridden function
  >;

  using PatternBindingLayout = BCRecordLayout<
    PATTERN_BINDING_DECL,
    DeclIDField, // context decl
    BCFixed<1>   // implicit flag
    // The pattern trails the record.
  >;

  using ParenPatternLayout = BCRecordLayout<
    PAREN_PATTERN
    // The sub-pattern trails the record.
  >;

  using TuplePatternLayout = BCRecordLayout<
    TUPLE_PATTERN,
    TypeIDField, // type
    BCVBR<5>     // arity
    // The elements trail the record.
  >;

  using TuplePatternEltLayout = BCRecordLayout<
    TUPLE_PATTERN_ELT,
    TypeIDField  // vararg base type, or 0
    // The element pattern trails the record.
  >;

  using NamedPatternLayout = BCRecordLayout<
    NAMED_PATTERN,
    DeclIDField // associated VarDecl
  >;

  using AnyPatternLayout = BCRecordLayout<
    ANY_PATTERN,
    TypeIDField // type
    // FIXME: is the type necessary?
  >;

  using TypedPatternLayout = BCRecordLayout<
    TYPED_PATTERN,
    TypeIDField // associated type
    // The sub-pattern trails the record.
  >;
  
  using IsaPatternLayout = BCRecordLayout<
    ISA_PATTERN,
    TypeIDField // type
  >;
  
  using NominalTypePatternLayout = BCRecordLayout<
    NOMINAL_TYPE_PATTERN,
    TypeIDField
    // The sub-pattern trails the record.
  >;

  using VarPatternLayout = BCRecordLayout<
    VAR_PATTERN
    // The sub-pattern trails the record.
  >;

  using XRefLayout = BCRecordLayout<
    XREF,
    XRefKindField, // reference kind
    TypeIDField,   // type if value, operator kind if operator
    BCArray<IdentifierIDField> // access path
  >;

  using DeclContextLayout = BCRecordLayout<
    DECL_CONTEXT,
    BCArray<DeclIDField>
  >;
}

/// The record types within the identifier block.
///
/// \sa IDENTIFIER_BLOCK_ID
namespace identifier_block {
  enum {
    IDENTIFIER_DATA = 1
  };

  using IdentifierDataLayout = BCRecordLayout<IDENTIFIER_DATA, BCBlob>;
};

/// The record types within the index block.
///
/// \sa INDEX_BLOCK_ID
namespace index_block {
  // These IDs must \em not be renumbered or reordered without incrementing
  // VERSION_MAJOR.
  enum {
    TYPE_OFFSETS = 1,
    DECL_OFFSETS,
    IDENTIFIER_OFFSETS,
    TOP_LEVEL_DECLS
  };

  using OffsetsLayout = BCGenericRecordLayout<
    BCFixed<2>,  // record ID
    BCArray<BitOffsetField>
  >;

  using TopLevelDeclsLayout = BCRecordLayout<
    TOP_LEVEL_DECLS,
    BCArray<DeclIDField>
  >;
}

} // end namespace serialization
} // end namespace swift

#endif
