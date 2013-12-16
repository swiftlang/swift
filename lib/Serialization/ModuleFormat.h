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

// ModuleID must be the same as IdentifierID because it is stored the same way.
using ModuleID = IdentifierID;
using ModuleIDField = IdentifierIDField;

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
  Method,
  WitnessMethod,
};
using AbstractCCField = BCFixed<3>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum XRefKind : uint8_t {
  SwiftValue = 0,
  SwiftOperator,
  SwiftGenericParameter
};
using XRefKindField = BCFixed<2>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum OperatorKind : uint8_t {
  Infix = 0,
  Prefix,
  Postfix
};
static_assert(sizeof(OperatorKind) <= sizeof(TypeID),
              "too many operator kinds");

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum class ParameterConvention : uint8_t {
  Indirect_In,
  Indirect_Out,
  Indirect_Inout,
  Direct_Owned,
  Direct_Unowned,
  Direct_Guaranteed,
};
using ParameterConventionField = BCFixed<3>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum class ResultConvention : uint8_t {
  Owned,
  Unowned,
  Autoreleased,
};
using ResultConventionField = BCFixed<2>;

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
  ValueWitnessMarker
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
  Unowned
};
using OwnershipField = BCFixed<2>;

// These IDs must \em not be renumbered or reordered without incrementing
// VERSION_MAJOR.
enum DefaultArgumentKind : uint8_t {
  None = 0,
  Normal,
  File,
  Line,
  Column
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
// VERSION_MAJOR. Adding a new ID requires adding a byte of overhead to the
// identifier table in Serializer::writeAllIdentifiers.
enum : uint8_t {
  /// Special IdentifierID value for the Builtin module.
  BUILTIN_MODULE_ID = 0,
  /// Special IdentifierID value for the current module.
  CURRENT_MODULE_ID,

  /// The number of special modules. This value should never be encoded;
  /// it should only be used to count the number of names above. As such, it
  /// is correct and necessary to add new values above this one.
  NUM_SPECIAL_MODULES
};


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

  /// The block for SIL functions.
  ///
  /// \sa sil_block
  SIL_BLOCK_ID,

  /// The index block for SIL functions.
  ///
  /// \sa sil_index_block
  SIL_INDEX_BLOCK_ID,

  /// The known protocol block, which is a sub-block of the index block.
  ///
  /// This contains lists of decls known to conform to each compiler-known
  /// protocol.
  KNOWN_PROTOCOL_BLOCK_ID = 64,
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
    IMPORTED_MODULE,
    LINK_LIBRARY
  };

  using SourceFileLayout = BCRecordLayout<
    SOURCE_FILE, // ID
    BCBlob // path
  >;

  using ImportedModuleLayout = BCRecordLayout<
    IMPORTED_MODULE,
    BCFixed<1>, // exported?
    BCBlob // module name, optionally followed by a null and then an import path
  >;

  using LinkLibraryLayout = BCRecordLayout<
    LINK_LIBRARY,
    LibraryKindField, // kind
    BCBlob // library name
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
#include "DeclTypeRecordNodes.def"
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

  using ArchetypeTypeLayout = BCRecordLayout<
    ARCHETYPE_TYPE,
    IdentifierIDField,   // name
    BCFixed<1>,          // primary?
    TypeIDField,         // index if primary, parent if non-primary
    DeclIDField,         // associated type or protocol decl
    TypeIDField,         // superclass
    BCArray<DeclIDField> // conformances
    // Trailed by the nested types record.
  >;

  using ArchetypeNestedTypeNamesLayout = BCRecordLayout<
    ARCHETYPE_NESTED_TYPE_NAMES,
    BCArray<IdentifierIDField>
  >;

  using ArchetypeNestedTypesLayout = BCRecordLayout<
    ARCHETYPE_NESTED_TYPES,
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
    BCVBR<6>     // # of conformances
    // Trailed by the protocol conformance info (if any)
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
    TypeIDField,           // result type
    ResultConventionField, // result convention
    DeclIDField,           // decl that owns the generic params
    ParameterConventionField, // callee convention
    AbstractCCField,       // calling convention
    BCFixed<1>,            // thin?
    BCFixed<1>,            // noreturn?
    BCArray<TypeIDField>   // parameter types and conventions, alternating.
    // Trailed by its generic parameters, if the owning decl ID is 0.
  >;

  template <unsigned Code>
  using SyntaxSugarTypeLayout = BCRecordLayout<
    Code,
    TypeIDField // element type
  >;

  using ArraySliceTypeLayout = SyntaxSugarTypeLayout<ARRAY_SLICE_TYPE>;
  using OptionalTypeLayout = SyntaxSugarTypeLayout<OPTIONAL_TYPE>;

  using ArrayTypeLayout = BCRecordLayout<
    ARRAY_TYPE,
    TypeIDField, // element type
    BCVBR<8>     // size
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
    DeclIDField, // context decl
    TypeIDField, // underlying type
    TypeIDField, // interface type
    BCFixed<1>  // implicit flag
    // Trailed by the conformance info (if any).
  >;

  using GenericTypeParamDeclLayout = BCRecordLayout<
    GENERIC_TYPE_PARAM_DECL,
    IdentifierIDField, // name
    DeclIDField, // context decl
    BCFixed<1>,  // implicit flag
    BCVBR<4>,    // depth
    BCVBR<4>,    // index
    TypeIDField, // superclass type
    TypeIDField  // archetype type
                 // Trailed by the conformance info (if any).
  >;

  using AssociatedTypeDeclLayout = BCRecordLayout<
    ASSOCIATED_TYPE_DECL,
    IdentifierIDField, // name
    DeclIDField,       // context decl
    TypeIDField,       // underlying type
    TypeIDField,       // archetype type
    BCFixed<1>         // implicit flag
                       // Trailed by the conformance info (if any).
  >;

  template <unsigned Code>
  using NominalLayout = BCRecordLayout<
    Code,
    IdentifierIDField, // name
    DeclIDField, // context decl
    BCFixed<1>  // implicit flag
    // Trailed by the generic parameters (if any), conformance info (if any),
    // and finally the decl context record.
  >;

  using StructLayout = NominalLayout<STRUCT_DECL>;

  using EnumLayout = BCRecordLayout<
    ENUM_DECL,
    IdentifierIDField, // name
    DeclIDField, // context decl
    BCFixed<1>,  // implicit flag
    TypeIDField  // raw type
    // Trailed by the generic parameters (if any), conformance info (if any),
    // and finally the decl context record.
  >;

  using ClassLayout = BCRecordLayout<
    CLASS_DECL,
    IdentifierIDField, // name
    DeclIDField,       // context decl
    BCFixed<1>,        // implicit?
    BCFixed<1>,        // explicitly objc?
    BCFixed<1>,        // IBLiveView?
    BCFixed<2>,        // Resilience kind
    TypeIDField        // superclass
    // Trailed by the generic parameters (if any), conformance info (if any),
    // and finally the decl context record.
  >;

  using ProtocolLayout = BCRecordLayout<
    PROTOCOL_DECL,
    IdentifierIDField, // name
    DeclIDField, // context decl
    BCFixed<1>,  // implicit flag
    BCFixed<1>,  // class protocol?
    BCFixed<1>,  // objc?
    BCArray<DeclIDField> // protocols
  // Trailed by the generic parameters (if any) and the decl context record
  >;

  using ConstructorLayout = BCRecordLayout<
    CONSTRUCTOR_DECL,
    DeclIDField, // context decl
    BCFixed<1>,  // implicit?
    BCFixed<1>,  // has selector-style signature?
    BCFixed<1>,  // objc?
    BCFixed<1>,  // transparent?
    TypeIDField, // type (signature)
    TypeIDField, // type (interface)
    DeclIDField  // implicit this decl
    // Trailed by its generic parameters, if any, followed by the parameter
    // pattern.
  >;

  using VarLayout = BCRecordLayout<
    VAR_DECL,
    IdentifierIDField, // name
    DeclIDField,  // context decl
    BCFixed<1>,   // implicit?
    BCFixed<1>,   // explicitly objc?
    BCFixed<1>,   // IBOutlet?
    BCFixed<1>,   // optional?
    BCFixed<1>,   // static?
    BCFixed<1>,   // isLet?
    TypeIDField,  // type
    TypeIDField,  // interface type
    DeclIDField,  // getter
    DeclIDField,  // setter
    DeclIDField   // overridden decl
  >;

  using FuncLayout = BCRecordLayout<
    FUNC_DECL,
    IdentifierIDField, // name
    DeclIDField,  // context decl
    BCFixed<1>,   // implicit?
    BCFixed<1>,   // has selector-style signature?
    BCFixed<1>,   // class method?
    BCFixed<1>,   // assignment? / conversion?
    BCFixed<1>,   // explicitly objc?
    BCFixed<1>,   // IBAction?
    BCFixed<1>,   // transparent?
    BCFixed<1>,   // inout?
    BCFixed<1>,   // optional?
    BCVBR<5>,     // number of parameter patterns
    TypeIDField,  // type (signature)
    TypeIDField,  // interface type
    DeclIDField,  // operator decl
    DeclIDField,  // overridden function
    BCBlob        // asmname, if any
    // The record is trailed by its generic parameters, if any, followed by its
    // argument and body parameter patterns.
  >;

  using PatternBindingLayout = BCRecordLayout<
    PATTERN_BINDING_DECL,
    DeclIDField, // context decl
    BCFixed<1>,  // implicit flag
    BCFixed<1>   // static?
    // The pattern trails the record.
  >;

  template <unsigned Code>
  using UnaryOperatorLayout = BCRecordLayout<
    Code, // ID field
    IdentifierIDField, // name
    DeclIDField // context decl
  >;

  using PrefixOperatorLayout = UnaryOperatorLayout<PREFIX_OPERATOR_DECL>;
  using PostfixOperatorLayout = UnaryOperatorLayout<POSTFIX_OPERATOR_DECL>;

  using InfixOperatorLayout = BCRecordLayout<
    INFIX_OPERATOR_DECL,
    IdentifierIDField, // name
    DeclIDField, // context decl
    AssociativityField,
    BCFixed<8>   // precedence
  >;

  using EnumElementLayout = BCRecordLayout<
    ENUM_ELEMENT_DECL,
    IdentifierIDField, // name
    DeclIDField, // context decl
    TypeIDField, // argument type
    TypeIDField, // constructor type
    TypeIDField, // interface type
    BCFixed<1>   // implicit?
  >;

  using SubscriptLayout = BCRecordLayout<
    SUBSCRIPT_DECL,
    DeclIDField, // context decl
    BCFixed<1>,  // implicit?
    BCFixed<1>,  // objc?
    BCFixed<1>,  // optional?
    TypeIDField, // subscript dummy type
    TypeIDField, // element type
    TypeIDField, // interface type
    DeclIDField, // getter
    DeclIDField, // setter
    DeclIDField  // overridden decl
    // The indices pattern trails the record.
  >;

  using ExtensionLayout = BCRecordLayout<
    EXTENSION_DECL,
    TypeIDField, // base type
    DeclIDField, // context decl
    BCFixed<1>   // implicit flag
    // Trailed by conformance info (if any), then the decl context record.
  >;

  using DestructorLayout = BCRecordLayout<
    DESTRUCTOR_DECL,
    DeclIDField, // context decl
    BCFixed<1>,  // implicit?
    BCFixed<1>,  // objc?
    TypeIDField, // type (signature)
    DeclIDField  // implicit this decl
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
  
  using IsaPatternLayout = BCRecordLayout<
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

  /// A placeholder for lack of conformance information. Conformances are
  /// indexed, so simply omitting one would be incorrect.
  using NoConformanceLayout = BCRecordLayout<
    NO_CONFORMANCE,
    DeclIDField  // the protocol
  >;

  using NormalProtocolConformanceLayout = BCRecordLayout<
    NORMAL_PROTOCOL_CONFORMANCE,
    DeclIDField, // the protocol
    BCVBR<5>, // value mapping count
    BCVBR<5>, // type mapping count
    BCVBR<5>, // inherited conformances count
    BCVBR<5>, // defaulted definitions count
    BCArray<DeclIDField>
    // The array contains value-value-substitutionCount triplets,
    // then type declarations, then defaulted definitions.
    // The inherited conformances trail the record, followed by substitution
    // records for the values and then types.
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

  using DeclContextLayout = BCRecordLayout<
    DECL_CONTEXT,
    BCArray<DeclIDField>
  >;

  using XRefLayout = BCRecordLayout<
    XREF,
    XRefKindField, // reference kind
    TypeIDField,   // type if value, operator kind if operator,
                   // index if generic parameter
    BCFixed<1>,    // within extension?
    BCArray<IdentifierIDField> // extension module ID (if extension value)
                               // base module ID
                               // access path
  >;
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
    CLASS_MEMBERS
  };

  using OffsetsLayout = BCGenericRecordLayout<
    BCFixed<3>,  // record ID
    BCArray<BitOffsetField>
  >;

  using DeclListLayout = BCGenericRecordLayout<
    BCFixed<3>,  // record ID
    BCVBR<16>,  // table offset within the blob (see below)
    BCBlob  // map from identifier strings to decl kinds / decl IDs
  >;

  /// A stable version of swift::KnownProtocolKind.
  ///
  /// The names should be kept in sync, but the values must \em not be
  /// renumbered or reordered without incrementing VERSION_MAJOR.
  enum KnownProtocolKind : uint8_t {
    ArrayBound = 1,
    Sequence,
    // killed
    LogicValue = 4,

    ArrayLiteralConvertible,
    CharacterLiteralConvertible,
    DictionaryLiteralConvertible,
    FloatLiteralConvertible,
    IntegerLiteralConvertible,
    StringInterpolationConvertible,
    StringLiteralConvertible,

    BuiltinCharacterLiteralConvertible,
    BuiltinFloatLiteralConvertible,
    BuiltinIntegerLiteralConvertible,
    BuiltinStringLiteralConvertible,
    
    DynamicLookup,
    RawRepresentable,
    Generator,
    RawOptionSet,
  };

  using KnownProtocolLayout = BCGenericRecordLayout<
    BCFixed<5>,  // known protocol ID
    BCArray<DeclIDField> // list of conforming decls
  >;
}

} // end namespace serialization
} // end namespace swift

#endif
