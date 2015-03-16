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
/// \file
/// \brief Contains various constants and helper types to deal with serialized
/// modules.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_MODULEFORMAT_H
#define SWIFT_SERIALIZATION_MODULEFORMAT_H

#include "swift/AST/Decl.h"
#include "llvm/Bitcode/RecordLayout.h"
#include "llvm/Bitcode/BitCodes.h"

namespace swift {
namespace serialization {

using llvm::Fixnum;
using llvm::BCArray;
using llvm::BCBlob;
using llvm::BCFixed;
using llvm::BCGenericRecordLayout;
using llvm::BCRecordLayout;
using llvm::BCVBR;

/// Magic number for serialized module files.
const unsigned char MODULE_SIGNATURE[] = { 0xE2, 0x9C, 0xA8, 0x0E };

/// Magic number for serialized documentation files.
const unsigned char MODULE_DOC_SIGNATURE[] = { 0xE2, 0x9C, 0xA8, 0x07 };

/// Serialized module format major version number.
///
/// Always 0 for Swift 1.0.
const uint16_t VERSION_MAJOR = 0;

/// Serialized module format minor version number.
///
/// When the format changes IN ANY WAY, this number should be incremented.
/// To ensure that two separate changes don't silently get merged into one
/// in source control, you should also update the comment to briefly
/// describe what change you made.
const uint16_t VERSION_MINOR = 180; // Last change: Added deallocating
                                    // parameter convention

using DeclID = Fixnum<31>;
using DeclIDField = BCFixed<31>;

// TypeID must be the same as DeclID because it is stored in the same way.
using TypeID = DeclID;
using TypeIDField = DeclIDField;

// IdentifierID must be the same as DeclID because it is stored in the same way.
using IdentifierID = DeclID;
using IdentifierIDField = DeclIDField;

// DeclContextID must be the same as DeclID because it is stored in the same way.
using DeclContextID = DeclID;
using DeclContextIDField = DeclIDField;

// ModuleID must be the same as IdentifierID because it is stored the same way.
using ModuleID = IdentifierID;
using ModuleIDField = IdentifierIDField;

using BitOffset = Fixnum<31>;
using BitOffsetField = BCFixed<31>;

// CharOffset must be the same as BitOffset because it is stored in the
// same way.
using CharOffset = BitOffset;
using CharOffsetField = BitOffsetField;

using FileSizeField = BCVBR<16>;
using FileModTimeField = BCVBR<16>;

enum class StorageKind : uint8_t {
  Stored, StoredWithTrivialAccessors, StoredWithObservers,
  InheritedWithObservers,
  Computed, ComputedWithMutableAddress,
  Addressed, AddressedWithTrivialAccessors, AddressedWithObservers,
};
using StorageKindField = BCFixed<4>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum class StaticSpellingKind : uint8_t {
  None = 0,
  KeywordStatic,
  KeywordClass,
};
using StaticSpellingKindField = BCFixed<2>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum AbstractCC : uint8_t {
  C = 0,
  ObjCMethod,
  Freestanding,
  Method,
  WitnessMethod,
};
using AbstractCCField = BCFixed<3>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum OperatorKind : uint8_t {
  Infix = 0,
  Prefix,
  Postfix
};
// This is currently required to have the same width as AccessorKindField.
using OperatorKindField = BCFixed<3>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum AccessorKind : uint8_t {
  Getter = 0,
  Setter,
  WillSet,
  DidSet,
  MaterializeForSet,
  Addressor,
  MutableAddressor,
};
using AccessorKindField = BCFixed<3>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum CtorInitializerKind : uint8_t {
  Designated = 0,
  Convenience = 1
};
using CtorInitializerKindField = BCFixed<1>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum class ParameterConvention : uint8_t {
  Indirect_In,
  Indirect_Out,
  Indirect_Inout,
  Direct_Owned,
  Direct_Unowned,
  Direct_Guaranteed,
  Indirect_In_Guaranteed,
  Direct_Deallocating,
};
using ParameterConventionField = BCFixed<3>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum class ResultConvention : uint8_t {
  Owned,
  Unowned,
  UnownedInnerPointer,
  Autoreleased,
};
using ResultConventionField = BCFixed<2>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum MetatypeRepresentation : uint8_t {
  MR_None, MR_Thin, MR_Thick, MR_ObjC
};
using MetatypeRepresentationField = BCFixed<2>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum class AddressorKind : uint8_t {
  NotAddressor, Unsafe, Owning, NativeOwning, NativePinning
};
using AddressorKindField = BCFixed<3>;

/// Translates an operator DeclKind to a Serialization fixity, whose values are
/// guaranteed to be stable.
static inline OperatorKind getStableFixity(DeclKind kind) {
  switch (kind) {
  case DeclKind::PrefixOperator:
    return Prefix;
  case DeclKind::PostfixOperator:
    return Postfix;
  case DeclKind::InfixOperator:
    return Infix;
  default:
    llvm_unreachable("unknown operator fixity");
  }
}

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum GenericRequirementKind : uint8_t {
  Conformance = 0,
  SameType,
  WitnessMarker
};
using GenericRequirementKindField = BCFixed<2>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum Associativity : uint8_t {
  NonAssociative = 0,
  LeftAssociative,
  RightAssociative
};
using AssociativityField = BCFixed<2>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum Ownership : uint8_t {
  Strong = 0,
  Weak,
  Unowned,
  Unmanaged,
};
using OwnershipField = BCFixed<2>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum class DefaultArgumentKind : uint8_t {
  None = 0,
  Normal,
  File,
  Line,
  Column,
  Function,
  Inherited,
  DSOHandle,
};
using DefaultArgumentField = BCFixed<3>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum LibraryKind : uint8_t {
  Library = 0,
  Framework
};
using LibraryKindField = BCFixed<1>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum class AccessibilityKind : uint8_t {
  Private = 0,
  Internal,
  Public,
};
using AccessibilityKindField = BCFixed<2>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum class OptionalTypeKind : uint8_t {
  None,
  Optional,
  ImplicitlyUnwrappedOptional
};
using OptionalTypeKindField = BCFixed<2>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum SpecialModuleID : uint8_t {
  /// Special IdentifierID value for the Builtin module.
  BUILTIN_MODULE_ID = 0,
  /// Special IdentifierID value for the current module.
  CURRENT_MODULE_ID,
  /// Special value for the module for imported Objective-C headers.
  OBJC_HEADER_MODULE_ID,

  /// The number of special modules. This value should never be encoded;
  /// it should only be used to count the number of names above. As such, it
  /// is correct and necessary to add new values above this one.
  NUM_SPECIAL_MODULES
};

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum class EnumElementRawValueKind : uint8_t {
  /// No raw value serialized.
  None = 0,
  /// Integer literal.
  IntegerLiteral,
  /// TODO: Float, string, char, etc.
};

using EnumElementRawValueKindField = BCFixed<4>;

/// The various types of blocks that can occur within a serialized Swift
/// module.
///
/// These IDs must \em not be renumbered or reordered without incrementing
/// VERSION_MAJOR.
enum BlockID {
  /// The module block, which contains all of the other blocks (and in theory
  /// allows a single file to contain multiple modules).
  MODULE_BLOCK_ID = llvm::bitc::FIRST_APPLICATION_BLOCKID,

  /// The control block, which contains all of the information that needs to
  /// be validated prior to committing to loading the serialized module.
  ///
  /// \sa control_block
  CONTROL_BLOCK_ID,

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

  /// The block for SIL functions.
  ///
  /// \sa sil_block
  SIL_BLOCK_ID,

  /// The index block for SIL functions.
  ///
  /// \sa sil_index_block
  SIL_INDEX_BLOCK_ID,

  /// A sub-block of the control block that contains configuration options
  /// needed to successfully load this module.
  ///
  /// \sa options_block
  OPTIONS_BLOCK_ID,

  /// The module documentation container block, which contains all other
  /// documentation blocks.
  MODULE_DOC_BLOCK_ID = 96,

  /// The comment block, which contains documentation comments.
  ///
  /// \sa comment_block
  COMMENT_BLOCK_ID
};

/// The record types within the control block.
///
/// \sa CONTROL_BLOCK_ID
namespace control_block {
  // These IDs must \em not be renumbered or reordered without incrementing
  // VERSION_MAJOR.
  enum {
    METADATA = 1,
    MODULE_NAME,
    TARGET
  };

  using MetadataLayout = BCRecordLayout<
    METADATA, // ID
    BCFixed<16>, // Module format major version
    BCFixed<16>, // Module format minor version
    BCBlob // misc. version information
  >;

  using ModuleNameLayout = BCRecordLayout<
    MODULE_NAME,
    BCBlob
  >;

  using TargetLayout = BCRecordLayout<
    TARGET,
    BCBlob // LLVM triple
  >;
}

/// The record types within the options block (a sub-block of the control
/// block).
///
/// \sa OPTIONS_BLOCK_ID
namespace options_block {
  // These IDs must \em not be renumbered or reordered without incrementing
  // VERSION_MAJOR.
  enum {
    SDK_PATH = 1,
    XCC,
    IS_SIB
  };

  using SDKPathLayout = BCRecordLayout<
    SDK_PATH,
    BCBlob // path
  >;

  using XCCLayout = BCRecordLayout<
    XCC,
    BCBlob // -Xcc flag, as string
  >;

  using IsSIBLayout = BCRecordLayout<
    IS_SIB,
    BCFixed<1> // Is this an intermediate file?
  >;
}

/// The record types within the input block.
///
/// \sa INPUT_BLOCK_ID
namespace input_block {
  // These IDs must \em not be renumbered or reordered without incrementing
  // VERSION_MAJOR.
  enum {
    IMPORTED_MODULE = 1,
    LINK_LIBRARY,
    IMPORTED_HEADER,
    IMPORTED_HEADER_CONTENTS,
    MODULE_FLAGS,
    SEARCH_PATH
  };

  using ImportedModuleLayout = BCRecordLayout<
    IMPORTED_MODULE,
    BCFixed<1>, // exported?
    BCFixed<1>, // scoped?
    BCBlob // module name, with submodule path pieces separated by \0s.
           // If the 'scoped' flag is set, the final path piece is an access
           // path within the module.
  >;

  using LinkLibraryLayout = BCRecordLayout<
    LINK_LIBRARY,
    LibraryKindField, // kind
    BCFixed<1>, // forced?
    BCBlob // library name
  >;

  using ImportedHeaderLayout = BCRecordLayout<
    IMPORTED_HEADER,
    BCFixed<1>, // exported?
    FileSizeField, // file size (for validation)
    FileModTimeField, // file mtime (for validation)
    BCBlob // file path
  >;

  using ImportedHeaderContentsLayout = BCRecordLayout<
    IMPORTED_HEADER_CONTENTS,
    BCBlob
  >;

  using ModuleFlagsLayout = BCRecordLayout<
    MODULE_FLAGS,
    BCFixed<1> // has underlying module?
  >;

  using SearchPathLayout = BCRecordLayout<
    SEARCH_PATH,
    BCFixed<1>, // framework?
    BCBlob      // path
  >;
}

/// The record types within the "decls-and-types" block.
///
/// \sa DECLS_AND_TYPES_BLOCK_ID
namespace decls_block {
  // These IDs must \em not be renumbered or reordered without incrementing
  // VERSION_MAJOR.
  enum RecordKind : uint8_t {
#define RECORD(Id) Id,
#define RECORD_VAL(Id, Value) Id = Value,
#include "swift/Serialization/DeclTypeRecordNodes.def"
  };

  using NameAliasTypeLayout = BCRecordLayout<
    NAME_ALIAS_TYPE,
    DeclIDField // typealias decl
  >;

  using GenericTypeParamTypeLayout = BCRecordLayout<
    GENERIC_TYPE_PARAM_TYPE,
    DeclIDField, // generic type parameter decl or depth
    BCVBR<4>     // index + 1, or zero if we have a generic type parameter decl
  >;

  using AssociatedTypeTypeLayout = BCRecordLayout<
    ASSOCIATED_TYPE_TYPE,
    DeclIDField // associated type decl
  >;

  using DependentMemberTypeLayout = BCRecordLayout<
    DEPENDENT_MEMBER_TYPE,
    TypeIDField,      // base type
    DeclIDField       // associated type decl
  >;
  using NominalTypeLayout = BCRecordLayout<
    NOMINAL_TYPE,
    DeclIDField, // decl
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
    IdentifierIDField,    // name
    TypeIDField,          // type
    DefaultArgumentField, // default argument
    BCFixed<1>            // vararg?
  >;

  using FunctionTypeLayout = BCRecordLayout<
    FUNCTION_TYPE,
    TypeIDField, // input
    TypeIDField, // output
    AbstractCCField, // calling convention
    BCFixed<1>,  // auto-closure?
    BCFixed<1>,  // thin?
    BCFixed<1>,  // noreturn?
    BCFixed<1>,  // block-compatible?
    BCFixed<1>   // noescape?
  >;

  using MetatypeTypeLayout = BCRecordLayout<
    METATYPE_TYPE,
    TypeIDField,                       // instance type
    MetatypeRepresentationField        // representation
  >;

  using ExistentialMetatypeTypeLayout = BCRecordLayout<
    EXISTENTIAL_METATYPE_TYPE,
    TypeIDField,                       // instance type
    MetatypeRepresentationField        // representation
  >;

  using LValueTypeLayout = BCRecordLayout<
    LVALUE_TYPE,
    TypeIDField // object type
  >;

  using InOutTypeLayout = BCRecordLayout<
    INOUT_TYPE,
    TypeIDField // object type
  >;

  using ArchetypeTypeLayout = BCRecordLayout<
    ARCHETYPE_TYPE,
    IdentifierIDField,   // name
    TypeIDField,         // index if primary, parent if non-primary
    DeclIDField,         // associated type or protocol decl
    TypeIDField,         // superclass
    BCArray<DeclIDField> // conformances
    // Trailed by the nested types record.
  >;

  using OpenedExistentialTypeLayout = BCRecordLayout<
    OPENED_EXISTENTIAL_TYPE,
    TypeIDField         // the existential type
  >;

  using DynamicSelfTypeLayout = BCRecordLayout<
    DYNAMIC_SELF_TYPE,
    TypeIDField          // self type
  >;

  using ArchetypeNestedTypeNamesLayout = BCRecordLayout<
    ARCHETYPE_NESTED_TYPE_NAMES,
    BCArray<IdentifierIDField>
  >;

  using ArchetypeNestedTypesLayout = BCRecordLayout<
    ARCHETYPE_NESTED_TYPES,
    BCArray<TypeIDField>
  >;
  
  using ArchetypeNestedTypesAreArchetypesLayout = BCRecordLayout<
    ARCHETYPE_NESTED_TYPES_ARE_ARCHETYPES,
    BCArray<TypeIDField>
  >;

  using ProtocolCompositionTypeLayout = BCRecordLayout<
    PROTOCOL_COMPOSITION_TYPE,
    BCArray<TypeIDField> // protocols
  >;

  using SubstitutedTypeLayout = BCRecordLayout<
    SUBSTITUTED_TYPE,
    TypeIDField, // original
    TypeIDField  // substitution
  >;

  using BoundGenericTypeLayout = BCRecordLayout<
    BOUND_GENERIC_TYPE,
    DeclIDField, // generic decl
    TypeIDField, // parent
    BCArray<TypeIDField> // generic arguments
  >;

  using BoundGenericSubstitutionLayout = BCRecordLayout<
    BOUND_GENERIC_SUBSTITUTION,
    TypeIDField, // archetype
    TypeIDField, // replacement
    BCArray<DeclIDField> // alternating conforming type and conformance module
                         // using the same format as the decl/module fields in
                         // SpecializedProtocolConformanceLayout.
    // Trailed by inline protocol conformance info (if any)
  >;

  using PolymorphicFunctionTypeLayout = BCRecordLayout<
    POLYMORPHIC_FUNCTION_TYPE,
    TypeIDField, // input
    TypeIDField, // output
    DeclIDField, // decl that owns the generic params
    AbstractCCField, // calling convention
    BCFixed<1>,  // thin?
    BCFixed<1>   // noreturn?
    // Trailed by its generic parameters, if the owning decl ID is 0.
  >;

  using GenericFunctionTypeLayout = BCRecordLayout<
    GENERIC_FUNCTION_TYPE,
    TypeIDField,         // input
    TypeIDField,         // output
    AbstractCCField,     // calling convention
    BCFixed<1>,          // thin?
    BCFixed<1>,          // noreturn?
    BCArray<TypeIDField> // generic parameters
                         // followed by requirements
  >;

  using SILFunctionTypeLayout = BCRecordLayout<
    SIL_FUNCTION_TYPE,
    TypeIDField,           // interface result type
    ResultConventionField, // interface result convention
    ParameterConventionField, // callee convention
    AbstractCCField,       // calling convention
    BCFixed<1>,            // thin?
    BCFixed<1>,            // block?
    BCFixed<1>,            // noreturn?
    BCFixed<1>,            // noescape?
    BCFixed<30>,           // number of generic parameters
    BCArray<TypeIDField>   // parameter types and conventions, alternating
                           // followed by generic parameter types
    // Trailed by its generic requirements, if any.
  >;
  
  using SILBlockStorageTypeLayout = BCRecordLayout<
    SIL_BLOCK_STORAGE_TYPE,
    TypeIDField            // capture type
  >;

  template <unsigned Code>
  using SyntaxSugarTypeLayout = BCRecordLayout<
    Code,
    TypeIDField // element type
  >;

  using ArraySliceTypeLayout = SyntaxSugarTypeLayout<ARRAY_SLICE_TYPE>;
  using OptionalTypeLayout = SyntaxSugarTypeLayout<OPTIONAL_TYPE>;
  using ImplicitlyUnwrappedOptionalTypeLayout =
    SyntaxSugarTypeLayout<UNCHECKED_OPTIONAL_TYPE>;

  using DictionaryTypeLayout = BCRecordLayout<
    DICTIONARY_TYPE,
    TypeIDField, // key type
    TypeIDField  // value type
  >;

  using ReferenceStorageTypeLayout = BCRecordLayout<
    REFERENCE_STORAGE_TYPE,
    OwnershipField,  // ownership
    TypeIDField      // implementation type
  >;

  using UnboundGenericTypeLayout = BCRecordLayout<
    UNBOUND_GENERIC_TYPE,
    DeclIDField, // generic decl
    TypeIDField  // parent
  >;

  using TypeAliasLayout = BCRecordLayout<
    TYPE_ALIAS_DECL,
    IdentifierIDField, // name
    DeclContextIDField,// context decl
    TypeIDField, // underlying type
    TypeIDField, // interface type
    BCFixed<1>,  // implicit flag
    AccessibilityKindField // accessibility
  >;

  using GenericTypeParamDeclLayout = BCRecordLayout<
    GENERIC_TYPE_PARAM_DECL,
    IdentifierIDField, // name
    DeclContextIDField,// context decl
    BCFixed<1>,  // implicit flag
    BCVBR<4>,    // depth
    BCVBR<4>,    // index
    TypeIDField, // superclass type
    TypeIDField, // archetype type
    BCArray<DeclIDField> // protocols
  >;

  using AssociatedTypeDeclLayout = BCRecordLayout<
    ASSOCIATED_TYPE_DECL,
    IdentifierIDField, // name
    DeclContextIDField,// context decl
    TypeIDField,       // underlying type
    TypeIDField,       // archetype type
    TypeIDField,       // default definition
    BCFixed<1>,        // implicit flag
    BCArray<DeclIDField> // protocols
  >;

  using StructLayout = BCRecordLayout<
    STRUCT_DECL,
    IdentifierIDField,      // name
    DeclContextIDField,     // context decl
    BCFixed<1>,             // implicit flag
    AccessibilityKindField, // accessibility
    BCArray<DeclIDField>    // protocols
    // Trailed by the generic parameters (if any), the members record, and
    // finally conformance info (if any).
  >;

  using EnumLayout = BCRecordLayout<
    ENUM_DECL,
    IdentifierIDField,      // name
    DeclContextIDField,     // context decl
    BCFixed<1>,             // implicit flag
    TypeIDField,            // raw type
    AccessibilityKindField, // accessibility
    BCArray<DeclIDField>    // protocols
    // Trailed by the generic parameters (if any), the members record, and
    // finally conformance info (if any).
  >;

  using ClassLayout = BCRecordLayout<
    CLASS_DECL,
    IdentifierIDField, // name
    DeclContextIDField,// context decl
    BCFixed<1>,        // implicit?
    BCFixed<1>,        // explicitly objc?
    BCFixed<1>,        // requires stored property initial values
    BCFixed<1>,        // foreign
    TypeIDField,       // superclass
    AccessibilityKindField, // accessibility
    BCArray<DeclIDField>    // protocols
    // Trailed by the generic parameters (if any), the members record, and
    // finally conformance info (if any).
  >;

  using ProtocolLayout = BCRecordLayout<
    PROTOCOL_DECL,
    IdentifierIDField,      // name
    DeclContextIDField,     // context decl
    BCFixed<1>,             // implicit flag
    BCFixed<1>,             // class-bounded?
    BCFixed<1>,             // objc?
    AccessibilityKindField, // accessibility
    BCArray<DeclIDField>    // protocols
    // Trailed by the generic parameters (if any) and the members record
  >;

  using ConstructorLayout = BCRecordLayout<
    CONSTRUCTOR_DECL,
    DeclContextIDField, // context decl
    OptionalTypeKindField,  // failability
    BCFixed<1>,  // implicit?
    BCFixed<1>,  // objc?
    BCFixed<1>,  // stub implementation?
    CtorInitializerKindField,  // initializer kind
    TypeIDField, // type (signature)
    TypeIDField, // type (interface)
    DeclIDField, // overridden decl
    AccessibilityKindField, // accessibility
    BCArray<IdentifierIDField> // argument names
    // Trailed by its generic parameters, if any, followed by the parameter
    // patterns.
  >;

  using VarLayout = BCRecordLayout<
    VAR_DECL,
    IdentifierIDField, // name
    DeclContextIDField,  // context decl
    BCFixed<1>,   // implicit?
    BCFixed<1>,   // explicitly objc?
    BCFixed<1>,   // static?
    BCFixed<1>,   // isLet?
    BCFixed<1>,   // HasNonPatternBindingInit?
    StorageKindField,   // StorageKind
    TypeIDField,  // type
    TypeIDField,  // interface type
    DeclIDField,  // getter
    DeclIDField,  // setter
    DeclIDField,  // materializeForSet
    DeclIDField,  // addressor
    DeclIDField,  // mutableAddressor
    DeclIDField,  // willset
    DeclIDField,  // didset
    DeclIDField,  // overridden decl
    AccessibilityKindField, // accessibility
    AccessibilityKindField // setter accessibility, if applicable
  >;

  using ParamLayout = BCRecordLayout<
    PARAM_DECL,
    IdentifierIDField, // argument name
    IdentifierIDField, // parameter name
    DeclContextIDField,  // context decl
    BCFixed<1>,   // isLet?
    TypeIDField,  // type
    TypeIDField  // interface type
  >;

  using FuncLayout = BCRecordLayout<
    FUNC_DECL,
    DeclContextIDField,  // context decl
    BCFixed<1>,   // implicit?
    BCFixed<1>,   // is 'static' or 'class'?
    StaticSpellingKindField, // spelling of 'static' or 'class'
    BCFixed<1>,   // explicitly objc?
    BCFixed<1>,   // mutating?
    BCFixed<1>,   // has dynamic self?
    BCVBR<5>,     // number of parameter patterns
    TypeIDField,  // type (signature)
    TypeIDField,  // interface type
    DeclIDField,  // operator decl
    DeclIDField,  // overridden function
    DeclIDField,  // AccessorStorageDecl
    BCFixed<1>,   // name is compound?
    AddressorKindField, // addressor kind
    AccessibilityKindField, // accessibility
    BCArray<IdentifierIDField> // name components
    // The record is trailed by:
    // - its asmname, if any
    // - its generic parameters, if any
    // - body parameter patterns
  >;

  using PatternBindingLayout = BCRecordLayout<
    PATTERN_BINDING_DECL,
    DeclContextIDField, // context decl
    BCFixed<1>,  // implicit flag
    BCFixed<1>,  // static?
    StaticSpellingKindField // spelling of 'static' or 'class'
    // The pattern trails the record.
  >;

  template <unsigned Code>
  using UnaryOperatorLayout = BCRecordLayout<
    Code, // ID field
    IdentifierIDField, // name
    DeclContextIDField // context decl
  >;

  using PrefixOperatorLayout = UnaryOperatorLayout<PREFIX_OPERATOR_DECL>;
  using PostfixOperatorLayout = UnaryOperatorLayout<POSTFIX_OPERATOR_DECL>;

  using InfixOperatorLayout = BCRecordLayout<
    INFIX_OPERATOR_DECL,
    IdentifierIDField, // name
    DeclContextIDField,// context decl
    AssociativityField,
    BCFixed<8>,  // precedence
    BCFixed<1>,  // assignment
    BCFixed<1>,  // IsAssocImplicit flag
    BCFixed<1>,  // IsPrecedenceImplicit flag
    BCFixed<1>   // IsAssignmentImplicit flag
  >;

  using EnumElementLayout = BCRecordLayout<
    ENUM_ELEMENT_DECL,
    IdentifierIDField, // name
    DeclContextIDField,// context decl
    TypeIDField, // argument type
    TypeIDField, // constructor type
    TypeIDField, // interface type
    BCFixed<1>,  // implicit?
    EnumElementRawValueKindField,  // raw value kind
    BCFixed<1>,  // negative raw value?
    BCBlob       // raw value
  >;

  using SubscriptLayout = BCRecordLayout<
    SUBSCRIPT_DECL,
    DeclContextIDField, // context decl
    BCFixed<1>,  // implicit?
    BCFixed<1>,  // objc?
    StorageKindField,   // StorageKind
    TypeIDField, // subscript dummy type
    TypeIDField, // element type
    TypeIDField, // interface type
    DeclIDField, // getter
    DeclIDField, // setter
    DeclIDField, // materializeForSet
    DeclIDField, // addressor
    DeclIDField, // mutableAddressor
    DeclIDField, // willSet
    DeclIDField, // didSet
    DeclIDField, // overridden decl
    AccessibilityKindField, // accessibility
    AccessibilityKindField, // setter accessibility, if applicable
    BCArray<IdentifierIDField> // name components
    // The indices pattern trails the record.
  >;

  using ExtensionLayout = BCRecordLayout<
    EXTENSION_DECL,
    TypeIDField, // base type
    DeclContextIDField, // context decl
    BCFixed<1>,  // implicit flag
    BCArray<DeclIDField> // protocols
    // Trailed by the members record and then conformance info (if any).
  >;

  using DestructorLayout = BCRecordLayout<
    DESTRUCTOR_DECL,
    DeclContextIDField, // context decl
    BCFixed<1>,  // implicit?
    BCFixed<1>,  // objc?
    TypeIDField, // type (signature)
    TypeIDField  // interface type
    // Trailed by a pattern for self.
  >;


  using ParenPatternLayout = BCRecordLayout<
    PAREN_PATTERN,
    BCFixed<1> // implicit?
    // The sub-pattern trails the record.
  >;

  using TuplePatternLayout = BCRecordLayout<
    TUPLE_PATTERN,
    TypeIDField, // type
    BCVBR<5>,    // arity
    BCFixed<1>, // implicit?
    BCFixed<1>  // has vararg?
    // The elements trail the record.
  >;

  using TuplePatternEltLayout = BCRecordLayout<
    TUPLE_PATTERN_ELT,
    DefaultArgumentField   // default argument
    // The element pattern trails the record.
  >;

  using NamedPatternLayout = BCRecordLayout<
    NAMED_PATTERN,
    DeclIDField, // associated VarDecl
    BCFixed<1>   // implicit?
  >;

  using AnyPatternLayout = BCRecordLayout<
    ANY_PATTERN,
    TypeIDField, // type
    BCFixed<1>   // implicit?
    // FIXME: is the type necessary?
  >;

  using TypedPatternLayout = BCRecordLayout<
    TYPED_PATTERN,
    TypeIDField, // associated type
    BCFixed<1>   // implicit?
    // The sub-pattern trails the record.
  >;

  using IsPatternLayout = BCRecordLayout<
    ISA_PATTERN,
    TypeIDField, // type
    BCFixed<1>   // implicit?
  >;

  using NominalTypePatternLayout = BCRecordLayout<
    NOMINAL_TYPE_PATTERN,
    TypeIDField, // type
    BCVBR<5>, // number of elements
    BCFixed<1>  // implicit?
    // The elements trail the record.
  >;

  using NominalTypePatternEltLayout = BCRecordLayout<
    NOMINAL_TYPE_PATTERN_ELT,
    DeclIDField // property
    // The element pattern trails the record.
  >;

  using VarPatternLayout = BCRecordLayout<
    VAR_PATTERN,
    BCFixed<1>, // isLet?
    BCFixed<1>  // implicit?
    // The sub-pattern trails the record.
  >;

  using GenericParamListLayout = BCRecordLayout<
    GENERIC_PARAM_LIST,
    BCArray<TypeIDField> // Archetypes
    // The actual parameters and requirements trail the record.
  >;

  using GenericParamLayout = BCRecordLayout<
    GENERIC_PARAM,
    DeclIDField // Typealias
  >;

  using GenericRequirementLayout = BCRecordLayout<
    GENERIC_REQUIREMENT,
    GenericRequirementKindField, // requirement kind
    BCArray<TypeIDField>         // types involved (two for conformance,
                                 // same-type; one for value witness marker)
  >;

  /// Placeholder that marks the last generic requirement in the generic
  /// parameters list.
  ///
  /// Used as a buffer between the generic parameter list's requirements and
  /// the generic signature's requirements for nominal type declarations.
  /// FIXME: Expected to go away once the latter is no longer serialized.
  using LastGenericRequirementLayout = BCRecordLayout<
    LAST_GENERIC_REQUIREMENT,
    BCFixed<1>                 // dummy
  >;

  /// Specifies the private discriminator string for a private declaration. This
  /// identifies the declaration's original source file in some opaque way.
  using PrivateDiscriminatorLayout = BCRecordLayout<
    PRIVATE_DISCRIMINATOR,
    IdentifierIDField  // discriminator string, as an identifier
  >;

  using LocalDiscriminatorLayout = BCRecordLayout<
    LOCAL_DISCRIMINATOR,
    BCVBR<2> // context-scoped discriminator counter
  >;

  /// A placeholder for lack of conformance information. Conformances are
  /// indexed, so simply omitting one would be incorrect.
  using NoConformanceLayout = BCRecordLayout<
    NO_CONFORMANCE,
    DeclIDField  // the protocol
  >;

  using NormalProtocolConformanceLayout = BCRecordLayout<
    NORMAL_PROTOCOL_CONFORMANCE,
    DeclIDField, // the protocol
    DeclContextIDField, // the decl that provided this conformance
    BCVBR<5>, // value mapping count
    BCVBR<5>, // type mapping count
    BCVBR<5>, // inherited conformances count
    BCVBR<5>, // defaulted definitions count
    BCFixed<1>, // incomplete?
                // If set, the data array is empty and only inherited
                // conformances trail the record.
    BCArray<DeclIDField>
    // The array contains protocol-conformer-module triplets for inherited
    // conformances, then value-value-substitutionCount triplets,
    // then type declarations, then defaulted definitions.
    // The inherited conformances that can't be cross-referenced trail the
    // record, followed by substitution records for the values and then types.
  >;

  using SpecializedProtocolConformanceLayout = BCRecordLayout<
    SPECIALIZED_PROTOCOL_CONFORMANCE,
    DeclIDField,         // the protocol
    DeclIDField,         // the nominal type decl for the generic conformance,
                         // or the conforming type for the generic conformance
                         // record that follows
    ModuleIDField,       // the module in which the generic conformance occurs,
                         // or BUILTIN_MODULE_ID to indicate that the generic
                         // conformance is in the following record
    BCVBR<5>             // # of substitutions for the conformance
    // followed by substitution records for the conformance
  >;

  using InheritedProtocolConformanceLayout = BCRecordLayout<
    INHERITED_PROTOCOL_CONFORMANCE,
    DeclIDField,  // the protocol
    DeclIDField,  // the nominal type decl for the inherited conformance,
                  // or the conforming type for the inherited conformance
                  // record that follows
    ModuleIDField // the module in which the inherited conformance occurs,
                  // or BUILTIN_MODULE_ID to indicate that the inherited
                  // conformance is in the following record
  >;

  using MembersLayout = BCRecordLayout<
    MEMBERS,
    BCArray<DeclIDField>
  >;

  using XRefLayout = BCRecordLayout<
    XREF,
    ModuleIDField,  // base module ID
    BCVBR<4>        // xref path length (cannot be 0)
  >;

  using XRefTypePathPieceLayout = BCRecordLayout<
    XREF_TYPE_PATH_PIECE,
    IdentifierIDField // name
  >;

  using XRefValuePathPieceLayout = BCRecordLayout<
    XREF_VALUE_PATH_PIECE,
    TypeIDField,      // type
    IdentifierIDField // name
  >;

  using XRefInitializerPathPieceLayout = BCRecordLayout<
    XREF_INITIALIZER_PATH_PIECE,
    TypeIDField,             // type
    CtorInitializerKindField // initializer kind
  >;

  using XRefExtensionPathPieceLayout = BCRecordLayout<
    XREF_EXTENSION_PATH_PIECE,
    ModuleIDField     // module ID
  >;

  using XRefOperatorOrAccessorPathPieceLayout = BCRecordLayout<
    XREF_OPERATOR_OR_ACCESSOR_PATH_PIECE,
    IdentifierIDField, // name
    AccessorKindField  // accessor kind OR operator fixity
  >;
  static_assert(std::is_same<AccessorKindField, OperatorKindField>::value,
                "accessor kinds and operator kinds are not compatible");

  using XRefGenericParamPathPieceLayout = BCRecordLayout<
    XREF_GENERIC_PARAM_PATH_PIECE,
    BCVBR<5> // index
  >;

  using AsmnameDeclAttrLayout = BCRecordLayout<
    Asmname_DECL_ATTR,
    BCFixed<1>, // implicit flag
    BCBlob      // asmname
  >;

  using SemanticsDeclAttrLayout = BCRecordLayout<
    Semantics_DECL_ATTR,
    BCFixed<1>, // implicit flag
    BCBlob      // semantics value
  >;

  using EffectsDeclAttrLayout = BCRecordLayout<
    Effects_DECL_ATTR,
    BCFixed<2>  // modref value
  >;

  using DeclContextLayout = BCRecordLayout<
    DECL_CONTEXT,
    // If this DeclContext is a local context, this is an
    // index into the local decl context table.
    // If this DeclContext is a Decl (and not a DeclContext
    // *at all*, this is an index into the decl table.
    DeclContextIDField,
    BCFixed<1> // is a decl
  >;

  using AbstractClosureExprLayout = BCRecordLayout<
    ABSTRACT_CLOSURE_EXPR_CONTEXT,
    TypeIDField, // type
    BCFixed<1>, // implicit
    BCVBR<4>, // discriminator
    DeclContextIDField // parent context decl
  >;

  using TopLevelCodeDeclContextLayout = BCRecordLayout<
    TOP_LEVEL_CODE_DECL_CONTEXT,
    DeclContextIDField // parent context decl
  >;

  using PatternBindingInitializerLayout = BCRecordLayout<
    PATTERN_BINDING_INITIALIZER_CONTEXT,
    DeclIDField // parent pattern binding decl
  >;

  using DefaultArgumentInitializerLayout = BCRecordLayout<
    DEFAULT_ARGUMENT_INITIALIZER_CONTEXT,
    DeclContextIDField, // parent context decl
    BCVBR<3> // parameter index
  >;

  // Stub layouts, unused.
  using OwnershipDeclAttrLayout = BCRecordLayout<Ownership_DECL_ATTR>;
  using RawDocCommentDeclAttrLayout = BCRecordLayout<RawDocComment_DECL_ATTR>;
  using AccessibilityDeclAttrLayout = BCRecordLayout<Accessibility_DECL_ATTR>;
  using SetterAccessibilityDeclAttrLayout =
    BCRecordLayout<SetterAccessibility_DECL_ATTR>;
  using ObjCBridgedDeclAttrLayout = BCRecordLayout<ObjCBridged_DECL_ATTR>;

  using InlineDeclAttrLayout = BCRecordLayout<
    Inline_DECL_ATTR,
    BCFixed<2>  // inline value
  >;

  // Encodes a VersionTuple:
  //
  //  Major
  //  Minor
  //  Subminor
  //  HasMinor
  //  HasSubminor
#define BC_AVAIL_TUPLE\
    BCVBR<5>,\
    BCVBR<5>,\
    BCVBR<4>,\
    BCFixed<1>,\
    BCFixed<1>

  using AvailabilityDeclAttrLayout = BCRecordLayout<
    Availability_DECL_ATTR,
    BCFixed<1>, // implicit flag
    BCFixed<1>, // is unconditionally unavailable?
    BC_AVAIL_TUPLE, // Introduced
    BC_AVAIL_TUPLE, // Deprecated
    BC_AVAIL_TUPLE, // Obsoleted
    BCVBR<5>,   // platform
    BCVBR<5>,   // number of bytes in message string
    BCVBR<5>,   // number of bytes in rename string
    BCBlob      // platform, followed by message
  >;

#undef BC_AVAIL_TUPLE

  using AutoClosureDeclAttrLayout = BCRecordLayout<
    AutoClosure_DECL_ATTR,
    BCFixed<1>, // implicit flag
    BCFixed<1>  // escaping
  >;

  using ObjCDeclAttrLayout = BCRecordLayout<
    ObjC_DECL_ATTR,
    BCFixed<1>, // implicit flag
    BCFixed<1>, // implicit name flag
    BCVBR<4>,   // # of arguments (+1) or zero if no name
    BCArray<IdentifierIDField>
  >;

#define SIMPLE_DECL_ATTR(X, CLASS, ...) \
  using CLASS##DeclAttrLayout = BCRecordLayout< \
    CLASS##_DECL_ATTR, \
    BCFixed<1> /* implicit flag */ \
  >;
#include "swift/AST/Attr.def"

}

/// Returns the encoding kind for the given decl.
///
/// Note that this does not work for all encodable decls, only those designed
/// to be stored in a hash table.
static inline decls_block::RecordKind getKindForTable(const Decl *D) {
  using namespace decls_block;

  switch (D->getKind()) {
  case DeclKind::TypeAlias:
    return decls_block::TYPE_ALIAS_DECL;
  case DeclKind::Enum:
    return decls_block::ENUM_DECL;
  case DeclKind::Struct:
    return decls_block::STRUCT_DECL;
  case DeclKind::Class:
    return decls_block::CLASS_DECL;
  case DeclKind::Protocol:
    return decls_block::PROTOCOL_DECL;

  case DeclKind::Func:
    return decls_block::FUNC_DECL;
  case DeclKind::Var:
    return decls_block::VAR_DECL;
  case DeclKind::Param:
    return decls_block::PARAM_DECL;

  case DeclKind::Subscript:
    return decls_block::SUBSCRIPT_DECL;
  case DeclKind::Constructor:
    return decls_block::CONSTRUCTOR_DECL;
  case DeclKind::Destructor:
    return decls_block::DESTRUCTOR_DECL;

  default:
    llvm_unreachable("cannot store this kind of decl in a hash table");
  }
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
  enum RecordKind {
    TYPE_OFFSETS = 1,
    DECL_OFFSETS,
    IDENTIFIER_OFFSETS,
    TOP_LEVEL_DECLS,
    OPERATORS,
    EXTENSIONS,
    CLASS_MEMBERS,
    OPERATOR_METHODS,

    /// The Objective-C method index, which contains a mapping from
    /// Objective-C selectors to the methods/initializers/properties/etc. that
    /// produce Objective-C methods.
    OBJC_METHODS,

    ENTRY_POINT,
    LOCAL_DECL_CONTEXT_OFFSETS,
    DECL_CONTEXT_OFFSETS,
    LOCAL_TYPE_DECLS,
  };

  using OffsetsLayout = BCGenericRecordLayout<
    BCFixed<4>,  // record ID
    BCArray<BitOffsetField>
  >;

  using DeclListLayout = BCGenericRecordLayout<
    BCFixed<4>,  // record ID
    BCVBR<16>,  // table offset within the blob (see below)
    BCBlob  // map from identifier strings to decl kinds / decl IDs
  >;

  using ObjCMethodTableLayout = BCRecordLayout<
    OBJC_METHODS,  // record ID
    BCVBR<16>,     // table offset within the blob (see below)
    BCBlob         // map from Objective-C selectors to methods with that selector
  >;

  using EntryPointLayout = BCRecordLayout<
    ENTRY_POINT,
    DeclIDField  // the ID of the main class; 0 if there was a main source file
  >;
}

/// \sa COMMENT_BLOCK_ID
namespace comment_block {
  enum RecordKind {
    DECL_COMMENTS = 1,
  };

  using DeclCommentListLayout = BCRecordLayout<
    DECL_COMMENTS, // record ID
    BCVBR<16>,     // table offset within the blob (see below)
    BCBlob         // map from Decl IDs to comments
  >;
} // namespace comment_block

} // end namespace serialization
} // end namespace swift

#endif
