//===--- Serialization.h - Read and write Swift modules ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Serializer interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_SERIALIZATION_H
#define SWIFT_SERIALIZATION_SERIALIZATION_H

#include "swift/Serialization/ModuleFormat.h"
#include "swift/Serialization/SerializationOptions.h"
#include "swift/Subsystems.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/MapVector.h"
#include <array>
#include <queue>
#include <tuple>

namespace swift {
  class SILModule;

namespace serialization {

using FilenamesTy = ArrayRef<std::string>;

class SerializerBase {
protected:
  SmallVector<char, 0> Buffer;
  llvm::BitstreamWriter Out{Buffer};

  /// A reusable buffer for emitting records.
  SmallVector<uint64_t, 64> ScratchRecord;

  /// The module currently being serialized.
  const ModuleDecl *M = nullptr;

  /// The SourceFile currently being serialized, if any.
  ///
  /// If this is non-null, only decls actually from this SourceFile will be
  /// serialized. Any other decls will be cross-referenced instead.
  const SourceFile *SF = nullptr;

  /// Record the name of a block.
  void emitBlockID(unsigned ID, StringRef name,
                   SmallVectorImpl<unsigned char> &nameBuffer);

  /// Record the name of a record within a block.
  void emitRecordID(unsigned ID, StringRef name,
                    SmallVectorImpl<unsigned char> &nameBuffer);

  void writeToStream(raw_ostream &os);

public:
  SerializerBase(ArrayRef<unsigned char> signature, ModuleOrSourceFile DC);
};

class Serializer : public SerializerBase {
public:
  /// Stores a declaration or a type to be written to the AST file.
  ///
  /// Convenience wrapper around a PointerUnion.
  class DeclTypeUnion {
    using DataTy = llvm::PointerUnion<const Decl *, Type>;
    DataTy Data;

    explicit DeclTypeUnion(const void *val)
      : Data(DataTy::getFromOpaqueValue(const_cast<void *>(val))) {}

  public:
    /*implicit*/ DeclTypeUnion(const Decl *d)
      : Data(d) { }
    /*implicit*/ DeclTypeUnion(Type ty)
      : Data(ty) { }

    bool isDecl() const { return Data.is<const Decl *>(); }
    bool isType() const { return Data.is<Type>(); }

    Type getType() const { return Data.get<Type>(); }
    const Decl *getDecl() const { return Data.get<const Decl *>(); }

    const void *getOpaqueValue() const { return Data.getOpaqueValue(); }
    static DeclTypeUnion getFromOpaqueValue(void *opaqueVal) {
      return DeclTypeUnion(opaqueVal);
    }

    bool operator==(const DeclTypeUnion &other) const {
      return Data == other.Data;
    }
  };

private:
  /// A map from Types and Decls to their serialized IDs.
  llvm::DenseMap<DeclTypeUnion, DeclID> DeclAndTypeIDs;

  /// A map from non-identifier uniqued strings to their serialized IDs.
  ///
  /// Since we never remove items from this map, we can use a BumpPtrAllocator
  /// to back the entries.
  llvm::StringMap<IdentifierID, llvm::BumpPtrAllocator> UniquedStringIDs;

  /// A map from Identifiers to their serialized IDs.
  ///
  /// This is stored separately from \p UniquedStringIDs because it's faster
  /// to do lookups in, even though that may lead to some duplication between
  /// identifier and non-identifier strings.
  llvm::DenseMap<Identifier, IdentifierID> IdentifierIDs;

  /// A map from DeclContexts to their serialized IDs.
  llvm::DenseMap<const DeclContext*, DeclContextID> DeclContextIDs;

  /// A map from local DeclContexts to their serialized IDs.
  llvm::DenseMap<const DeclContext*, DeclContextID> LocalDeclContextIDs;

  /// A map from generic signatures to their serialized IDs.
  llvm::DenseMap<const GenericSignature *, GenericSignatureID>
    GenericSignatureIDs;

  /// A map from generic environments to their serialized IDs.
  llvm::DenseMap<const GenericEnvironment *, GenericEnvironmentID>
    GenericEnvironmentIDs;

  /// A map from substitution maps to their serialized IDs.
  llvm::DenseMap<SubstitutionMap, SubstitutionMapID> SubstitutionMapIDs;

  // A map from NormalProtocolConformances to their serialized IDs.
  llvm::DenseMap<const NormalProtocolConformance *, NormalConformanceID>
    NormalConformances;

  // A map from SILLayouts to their serialized IDs.
  llvm::DenseMap<SILLayout *, SILLayoutID> SILLayouts;

public:
  using DeclTableData = SmallVector<std::pair<uint8_t, DeclID>, 4>;
  /// The in-memory representation of what will eventually be an on-disk hash
  /// table.
  using DeclTable = llvm::MapVector<DeclBaseName, DeclTableData>;

  using ObjCMethodTableData =
    SmallVector<std::tuple<std::string, bool, DeclID>, 4>;

  // In-memory representation of what will eventually be an on-disk
  // hash table of all defined Objective-C methods.
  using ObjCMethodTable = llvm::MapVector<ObjCSelector, ObjCMethodTableData>;

  using NestedTypeDeclsData = SmallVector<std::pair<DeclID, DeclID>, 4>;
  // In-memory representation of what will eventually be an on-disk
  // hash table of all defined Objective-C methods.
  using NestedTypeDeclsTable = llvm::MapVector<Identifier, NestedTypeDeclsData>;

  using DeclMembersData = SmallVector<DeclID, 2>;
  // In-memory representation of what will eventually be an on-disk
  // hash table of all ValueDecl-members of a paticular DeclBaseName.
  using DeclMembersTable = llvm::MapVector<uint32_t, DeclMembersData>;

  using DeclMemberNamesData = std::pair<serialization::BitOffset,
                                        std::unique_ptr<DeclMembersTable>>;
  // In-memory representation of what will eventually be an on-disk
  // hash table mapping DeclBaseNames to DeclMembersData tables.
  using DeclMemberNamesTable = llvm::MapVector<DeclBaseName, DeclMemberNamesData>;

  using ExtensionTableData =
      SmallVector<std::pair<const NominalTypeDecl *, DeclID>, 4>;
  using ExtensionTable = llvm::MapVector<Identifier, ExtensionTableData>;

private:
  /// A map from identifiers to methods and properties with the given name.
  ///
  /// This is used for id-style lookup.
  DeclTable ClassMembersForDynamicLookup;

  /// A map from DeclBaseNames of members to Decl->members sub-tables.
  ///
  /// This is for Named Lazy Member Loading.
  DeclMemberNamesTable DeclMemberNames;

  /// The queue of types and decls that need to be serialized.
  ///
  /// This is a queue and not simply a vector because serializing one
  /// decl-or-type might trigger the serialization of another one.
  std::queue<DeclTypeUnion> DeclsAndTypesToWrite;

  /// DeclContexts that need to be serialized.
  std::queue<const DeclContext*> DeclContextsToWrite;

  /// Local DeclContexts that need to be serialized.
  std::queue<const DeclContext*> LocalDeclContextsToWrite;

  /// Generic signatures that need to be serialized.
  std::queue<const GenericSignature *> GenericSignaturesToWrite;

  /// Generic environments that need to be serialized.
  std::queue<const GenericEnvironment*> GenericEnvironmentsToWrite;

  /// Substitution maps that need to be serialized.
  std::queue<SubstitutionMap> SubstitutionMapsToWrite;

  /// NormalProtocolConformances that need to be serialized.
  std::queue<const NormalProtocolConformance *> NormalConformancesToWrite;

  /// SILLayouts that need to be serialized.
  std::queue<SILLayout *> SILLayoutsToWrite;

  /// All uniqued strings that need to be serialized (identifiers and
  /// non-identifiers).
  std::vector<StringRef> StringsToWrite;

  /// The abbreviation code for each record in the "decls-and-types" block.
  ///
  /// These are registered up front when entering the block, so they can be
  /// reused.
  std::array<unsigned, 256> DeclTypeAbbrCodes;

  /// The offset of each Decl in the bitstream, indexed by DeclID.
  std::vector<BitOffset> DeclOffsets;

  /// The offset of each Type in the bitstream, indexed by TypeID.
  std::vector<BitOffset> TypeOffsets;

  /// The offset of each DeclContext in the bitstream, indexed by DeclContextID
  std::vector<BitOffset> DeclContextOffsets;

  /// The offset of each localDeclContext in the bitstream,
  /// indexed by DeclContextID
  std::vector<BitOffset> LocalDeclContextOffsets;

  /// The offset of each Identifier in the identifier data block, indexed by
  /// IdentifierID.
  std::vector<CharOffset> IdentifierOffsets;

  /// The offset of each GenericSignature in the bitstream, indexed by
  /// GenericSignatureID.
  std::vector<BitOffset> GenericSignatureOffsets;

  /// The offset of each GenericEnvironment in the bitstream, indexed by
  /// GenericEnvironmentID.
  std::vector<BitOffset> GenericEnvironmentOffsets;

  /// The offset of each SubstitutionMap in the bitstream, indexed by
  /// SubstitutionMapID.
  std::vector<BitOffset> SubstitutionMapOffsets;

  /// The offset of each NormalProtocolConformance in the bitstream, indexed by
  /// NormalConformanceID.
  std::vector<BitOffset> NormalConformanceOffsets;

  /// The offset of each SILLayout in the bitstream, indexed by
  /// SILLayoutID.
  std::vector<BitOffset> SILLayoutOffsets;

  /// The decls that adopt compiler-known protocols.
  SmallVector<DeclID, 2> KnownProtocolAdopters[NumKnownProtocols];

  /// The last assigned DeclID for decls from this module.
  uint32_t /*DeclID*/ LastDeclID = 0;

  /// The last assigned DeclContextID for decl contexts from this module.
  uint32_t /*DeclContextID*/ LastDeclContextID = 0;

  /// The last assigned DeclContextID for local decl contexts from this module.
  uint32_t /*DeclContextID*/ LastLocalDeclContextID = 0;

  /// The last assigned NormalConformanceID for decl contexts from this module.
  uint32_t /*NormalConformanceID*/ LastNormalConformanceID = 0;

  /// The last assigned SILLayoutID for SIL layouts from this module.
  uint32_t /*SILLayoutID*/ LastSILLayoutID = 0;

  /// The last assigned DeclID for types from this module.
  uint32_t /*TypeID*/ LastTypeID = 0;

  /// The last assigned IdentifierID for uniqued strings from this module.
  ///
  /// Note that special module IDs and IDs of special names must not be valid
  /// IdentifierIDs, except that 0 will always represent the empty identifier.
  uint32_t /*IdentifierID*/ LastUniquedStringID =
      serialization::NUM_SPECIAL_IDS - 1;

  /// The last assigned GenericSignatureID for generic signature from this
  /// module.
  uint32_t /*GenericSignatureID*/ LastGenericSignatureID = 0;

  /// The last assigned GenericEnvironmentID for generic environments from this
  /// module.
  uint32_t /*GenericEnvironmentID*/ LastGenericEnvironmentID = 0;

  /// The last assigned SubstitutionMapID for substitution maps from this
  /// module.
  uint32_t /*SubstitutionMapID*/ LastSubstitutionMapID = 0;

  /// Returns the record code for serializing the given vector of offsets.
  ///
  /// This allows the offset-serialization code to be generic over all kinds
  /// of offsets.
  unsigned getOffsetRecordCode(const std::vector<BitOffset> &values) {
    if (&values == &DeclOffsets)
      return index_block::DECL_OFFSETS;
    if (&values == &TypeOffsets)
      return index_block::TYPE_OFFSETS;
    if (&values == &IdentifierOffsets)
      return index_block::IDENTIFIER_OFFSETS;
    if (&values == &DeclContextOffsets)
      return index_block::DECL_CONTEXT_OFFSETS;
    if (&values == &LocalDeclContextOffsets)
      return index_block::LOCAL_DECL_CONTEXT_OFFSETS;
    if (&values == &GenericSignatureOffsets)
      return index_block::GENERIC_SIGNATURE_OFFSETS;
    if (&values == &GenericEnvironmentOffsets)
      return index_block::GENERIC_ENVIRONMENT_OFFSETS;
    if (&values == &SubstitutionMapOffsets)
      return index_block::SUBSTITUTION_MAP_OFFSETS;
    if (&values == &NormalConformanceOffsets)
      return index_block::NORMAL_CONFORMANCE_OFFSETS;
    if (&values == &SILLayoutOffsets)
      return index_block::SIL_LAYOUT_OFFSETS;
    llvm_unreachable("unknown offset kind");
  }

  /// Writes the BLOCKINFO block for the serialized module file.
  void writeBlockInfoBlock();

  /// Writes the Swift module file header and name, plus metadata determining
  /// if the module can be loaded.
  void writeHeader(const SerializationOptions &options = {});

  /// Writes the dependencies used to build this module: its imported
  /// modules and its source files.
  void writeInputBlock(const SerializationOptions &options);

  void writeParameterList(const ParameterList *PL);

  /// Writes the given pattern, recursively.
  void writePattern(const Pattern *pattern, DeclContext *owningDC);

  /// Writes a generic parameter list.
  bool writeGenericParams(const GenericParamList *genericParams);

  /// Writes the body text of the provided funciton, if the function is
  /// inlinable and has body text.
  void writeInlinableBodyTextIfNeeded(const AbstractFunctionDecl *decl);

  /// Writes a list of protocol conformances.
  void writeConformances(ArrayRef<ProtocolConformanceRef> conformances,
                         const std::array<unsigned, 256> &abbrCodes);

  /// Writes a list of protocol conformances.
  void writeConformances(ArrayRef<ProtocolConformance*> conformances,
                         const std::array<unsigned, 256> &abbrCodes);

  /// Writes an array of members for a decl context.
  ///
  /// \param parentID The DeclID of the context.
  /// \param members The decls within the context.
  /// \param isClass True if the context could be a class context (class,
  ///        class extension, or protocol).
  void writeMembers(DeclID parentID, DeclRange members, bool isClass);

  /// Write a default witness table for a protocol.
  ///
  /// \param proto The protocol.
  void writeDefaultWitnessTable(const ProtocolDecl *proto,
                                const std::array<unsigned, 256> &abbrCodes);

  /// Check if a decl is cross-referenced.
  bool isDeclXRef(const Decl *D) const;

  /// Writes a reference to a decl in another module.
  void writeCrossReference(const DeclContext *DC, uint32_t pathLen = 1);

  /// Writes a reference to a decl in another module.
  void writeCrossReference(const Decl *D);

  /// Writes out a declaration attribute.
  void writeDeclAttribute(const DeclAttribute *DA);

  /// Writes out a foreign error convention.
  void writeForeignErrorConvention(const ForeignErrorConvention &fec);

  /// Writes the given decl.
  void writeDecl(const Decl *D);

  /// Writes the given decl context.
  void writeDeclContext(const DeclContext *DC);

  /// Write a DeclContext as a local DeclContext at the current offset.
  void writeLocalDeclContext(const DeclContext *DC);

  /// Write the components of a PatternBindingInitializer as a local context.
  void writePatternBindingInitializer(PatternBindingDecl *binding,
                                      unsigned bindingIndex);

  /// Write the components of a DefaultArgumentInitializer as a local context.
  void writeDefaultArgumentInitializer(const DeclContext *parentContext, unsigned index);

  /// Write the components of an AbstractClosureExpr as a local context.
  void writeAbstractClosureExpr(const DeclContext *parentContext, Type Ty, bool isImplicit, unsigned discriminator);

  /// Writes the given type.
  void writeType(Type ty);

  /// Writes a generic signature.
  void writeGenericSignature(const GenericSignature *sig);

  /// Writes a generic environment.
  void writeGenericEnvironment(const GenericEnvironment *env);

  /// Writes a substitution map.
  void writeSubstitutionMap(const SubstitutionMap substitutions);

  /// Registers the abbreviation for the given decl or type layout.
  template <typename Layout>
  void registerDeclTypeAbbr() {
    using AbbrArrayTy = decltype(DeclTypeAbbrCodes);
    static_assert(Layout::Code <= std::tuple_size<AbbrArrayTy>::value,
                  "layout has invalid record code");
    DeclTypeAbbrCodes[Layout::Code] = Layout::emitAbbrev(Out);
  }

  /// Writes all decls and types in the DeclsToWrite queue.
  ///
  /// This will continue until the queue is empty, even if the items currently
  /// in the queue trigger the serialization of additional decls and/or types.
  void writeAllDeclsAndTypes();

  /// Writes all identifiers in the IdentifiersToWrite queue.
  ///
  /// This must be called after writeAllDeclsAndTypes(), since that may add
  /// additional identifiers to the pool.
  void writeAllIdentifiers();

  /// Writes the offsets for decls or types.
  void writeOffsets(const index_block::OffsetsLayout &Offsets,
                    const std::vector<BitOffset> &values);

  /// Serializes all transparent SIL functions in the SILModule.
  void writeSIL(const SILModule *M, bool serializeAllSIL);

  /// Top-level entry point for serializing a module.
  void writeAST(ModuleOrSourceFile DC,
                bool enableNestedTypeLookupTable);

  using SerializerBase::SerializerBase;
  using SerializerBase::writeToStream;

public:
  /// Serialize a module to the given stream.
  static void writeToStream(raw_ostream &os, ModuleOrSourceFile DC,
                            const SILModule *M,
                            const SerializationOptions &options);

  /// Records the use of the given Type.
  ///
  /// The Type will be scheduled for serialization if necessary.
  ///
  /// \returns The ID for the given Type in this module.
  TypeID addTypeRef(Type ty);

  /// Records the use of the given DeclBaseName.
  ///
  /// The Identifier will be scheduled for serialization if necessary.
  ///
  /// \returns The ID for the given DeclBaseName in this module.
  IdentifierID addDeclBaseNameRef(DeclBaseName ident);

  /// Records the use of the given string, which will only be stored once in
  /// the resulting module file.
  ///
  /// \returns A pair containing the copy of the string now owned by the
  /// Serializer and the ID for the string in this module.
  /// \sa addUniquedStringRef
  std::pair<StringRef, IdentifierID> addUniquedString(StringRef str);

  /// Records the use of the given string, which will only be stored once in
  /// the resulting module file.
  ///
  /// \returns The ID for the given string in this module.
  /// \sa addUniquedString
  IdentifierID addUniquedStringRef(StringRef str) {
    return addUniquedString(str).second;
  }

  /// Records the use of the given file name.
  ///
  /// The Identifier will be scheduled for serialization if necessary.
  ///
  /// \returns The ID for the given file name in this module.
  IdentifierID addFilename(StringRef filename);

  /// Records the use of the given Decl.
  ///
  /// The Decl will be scheduled for serialization if necessary.
  ///
  /// \returns The ID for the given Decl in this module.
  DeclID addDeclRef(const Decl *D, bool allowTypeAliasXRef = false);

  /// Records the use of the given DeclContext.
  ///
  /// The DeclContext will be scheduled for serialization if necessary.
  DeclContextID addDeclContextRef(const DeclContext *DC);

  /// Records the use of the given local DeclContext.
  ///
  /// The DeclContext will be scheduled for serialization if necessary.
  DeclContextID addLocalDeclContextRef(const DeclContext *DC);

  /// Records the use of the given generic signature.
  ///
  /// The GenericSignature will be scheduled for serialization if necessary.
  GenericSignatureID addGenericSignatureRef(const GenericSignature *sig);

  /// Records the use of the given generic environment.
  ///
  /// The GenericEnvironment will be scheduled for serialization if necessary.
  GenericEnvironmentID addGenericEnvironmentRef(const GenericEnvironment *env);

  /// Records the use of the given substitution map.
  ///
  /// The SubstitutionMap will be scheduled for serialization if necessary.
  SubstitutionMapID addSubstitutionMapRef(SubstitutionMap substitutions);

  /// Records the use of the given normal protocol conformance.
  ///
  /// The normal protocol conformance will be scheduled for
  /// serialization if necessary.
  ///
  /// \returns The ID for the given conformance in this module.
  NormalConformanceID addConformanceRef(
                        const NormalProtocolConformance *conformance);

  /// Records the use of the given SILLayout.
  SILLayoutID addSILLayoutRef(SILLayout *layout);

  /// Records the use of the given module.
  ///
  /// The module's name will be scheduled for serialization if necessary.
  ///
  /// \returns The ID for the identifier for the module's name, or one of the
  /// special module codes defined above.
  IdentifierID addModuleRef(const ModuleDecl *M);

  /// Write a normal protocol conformance.
  void writeNormalConformance(const NormalProtocolConformance *conformance);

  /// Write a SILLayout.
  void writeSILLayout(SILLayout *conformance);

  /// Writes a protocol conformance.
  ///
  /// \param genericEnv When provided, the generic environment that describes
  /// the archetypes within the substitutions. The replacement types within
  /// the substitution will be mapped out of the generic environment before
  /// being written.
  void writeConformance(ProtocolConformanceRef conformance,
                        const std::array<unsigned, 256> &abbrCodes,
                        GenericEnvironment *genericEnv = nullptr);

  /// Writes a protocol conformance.
  void writeConformance(ProtocolConformance *conformance,
                        const std::array<unsigned, 256> &abbrCodes,
                        GenericEnvironment *genericEnv = nullptr);

  /// Writes a set of generic requirements.
  void writeGenericRequirements(ArrayRef<Requirement> requirements,
                                const std::array<unsigned, 256> &abbrCodes);
};

/// Serialize module documentation to the given stream.
void writeDocToStream(raw_ostream &os, ModuleOrSourceFile DC,
                      StringRef GroupInfoPath);
} // end namespace serialization
} // end namespace swift
#endif
