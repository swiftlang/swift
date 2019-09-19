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

#include "ModuleFormat.h"
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
  class DeclSerializer;
  friend class DeclSerializer;
  class TypeSerializer;
  friend class TypeSerializer;

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

  /// All uniqued strings that need to be serialized (identifiers and
  /// non-identifiers).
  std::vector<StringRef> StringsToWrite;

  /// The last assigned IdentifierID for uniqued strings from this module.
  ///
  /// Note that special module IDs and IDs of special names must not be valid
  /// IdentifierIDs, except that 0 will always represent the empty identifier.
  uint32_t /*IdentifierID*/ LastUniquedStringID =
      serialization::NUM_SPECIAL_IDS - 1;

  /// Helper for serializing entities in the AST block object graph.
  ///
  /// Keeps track of assigning IDs to newly-seen entities, and collecting
  /// offsets when those entities are ready to be serialized.
  ///
  /// \tparam T The AST type that's being serialized
  /// \tparam ID The ID type for encoding into the bitstream
  /// \tparam RecordCode_ The code for the offsets record in the Index block for
  /// referring to these entities.
  template <typename T, typename ID, unsigned RecordCode_>
  class ASTBlockRecordKeeper {
    /// Map of entities to assigned ID numbers.
    llvm::DenseMap<T, ID> IDs;

    /// All entities that still need to be written.
    ///
    /// This is a queue and not simply a vector because serializing one
    /// entity might result in another one getting queued up.
    std::queue<T> EntitiesToWrite;

    /// The offsets of entities that have already been written.
    ///
    /// `Offsets[entityID - 1]` gives the offset for a particular entity. (0 is
    /// reserved for "empty" entities.)
    std::vector<BitOffset> Offsets;
  public:
    /// The code for the offsets record in the Index block for referring to
    /// these entities.
    static const unsigned RecordCode = RecordCode_;

    /// Assigns \p entity an ID and schedules it for writing.
    ///
    /// If \p entity has been seen before, returns the existing ID.
    ///
    /// 0 is used for "empty" entities (those that coerce to \c false ).
    ID addRef(T entity) {
      assert(ID() == 0 && "bad default constructor for ID");
      if (!entity)
        return 0;

      auto &entityID = IDs[entity];
      if (entityID == ID()) {
        EntitiesToWrite.push(entity);
        entityID = IDs.size();
      }
      return entityID;
    }

    /// Returns true if \p entity has been seen before (i.e. ever passed to
    /// #addRef).
    bool hasRef(T entity) const {
      return IDs.find(entity) != IDs.end();
    }

    bool hasMoreToSerialize() const {
      return !EntitiesToWrite.empty();
    }

    /// Returns the next entity to be written.
    ///
    /// If there is nothing left to serialize, returns None.
    Optional<T> peekNext() const {
      if (!hasMoreToSerialize())
        return None;
      return EntitiesToWrite.front();
    }

    /// Records that the next entity will be written at \p offset, and returns
    /// it so it can be written.
    ///
    /// If there is nothing left to serialize, returns None.
    Optional<T> popNext(BitOffset offset) {
      if (!hasMoreToSerialize())
        return None;
      T result = EntitiesToWrite.front();
      EntitiesToWrite.pop();
      Offsets.push_back(offset);
      assert(IDs.lookup(result) == Offsets.size());
      return result;
    }

    /// Returns the offsets for all entities that have been written.
    ///
    /// Should only be called after all entities \e have been written.
    ArrayRef<BitOffset> getOffsets() const {
      assert(!hasMoreToSerialize() && "not all entities were serialized");
      return Offsets;
    }
  };

  ASTBlockRecordKeeper<const Decl *, DeclID,
                       index_block::DECL_OFFSETS>
  DeclsToSerialize;

  ASTBlockRecordKeeper<Type, TypeID,
                       index_block::TYPE_OFFSETS>
  TypesToSerialize;

  ASTBlockRecordKeeper<const DeclContext *, LocalDeclContextID,
                       index_block::LOCAL_DECL_CONTEXT_OFFSETS>
  LocalDeclContextsToSerialize;

  ASTBlockRecordKeeper<const GenericSignature *, GenericSignatureID,
                       index_block::GENERIC_SIGNATURE_OFFSETS>
  GenericSignaturesToSerialize;

  ASTBlockRecordKeeper<SubstitutionMap, SubstitutionMapID,
                       index_block::SUBSTITUTION_MAP_OFFSETS>
  SubstitutionMapsToSerialize;

  ASTBlockRecordKeeper<const NormalProtocolConformance *, NormalConformanceID,
                       index_block::NORMAL_CONFORMANCE_OFFSETS>
  NormalConformancesToSerialize;

  ASTBlockRecordKeeper<const SILLayout *, SILLayoutID,
                       index_block::SIL_LAYOUT_OFFSETS>
  SILLayoutsToSerialize;

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

  /// The abbreviation code for each record in the "decls-and-types" block.
  ///
  /// These are registered up front when entering the block, so they can be
  /// reused.
  std::array<unsigned, 256> DeclTypeAbbrCodes;

  /// The decls that adopt compiler-known protocols.
  SmallVector<DeclID, 2> KnownProtocolAdopters[NumKnownProtocols];

  /// Writes the BLOCKINFO block for the serialized module file.
  void writeBlockInfoBlock();

  /// Writes the Swift module file header and name, plus metadata determining
  /// if the module can be loaded.
  void writeHeader(const SerializationOptions &options = {});

  /// Writes the dependencies used to build this module: its imported
  /// modules and its source files.
  void writeInputBlock(const SerializationOptions &options);

  /// Writes a list of protocol conformances.
  void writeConformances(ArrayRef<ProtocolConformanceRef> conformances,
                         const std::array<unsigned, 256> &abbrCodes);

  /// Writes a list of protocol conformances.
  void writeConformances(ArrayRef<ProtocolConformance*> conformances,
                         const std::array<unsigned, 256> &abbrCodes);

  /// Check if a decl is cross-referenced.
  bool isDeclXRef(const Decl *D) const;

  /// Writes a reference to a decl in another module.
  void writeCrossReference(const DeclContext *DC, uint32_t pathLen = 1);

  /// Writes a reference to a decl in another module.
  void writeCrossReference(const Decl *D);

  /// Writes the given decl.
  void writeASTBlockEntity(const Decl *D);

  /// Write a DeclContext as a local DeclContext at the current offset.
  void writeASTBlockEntity(const DeclContext *DC);

  /// Write the components of a PatternBindingInitializer as a local context.
  void writePatternBindingInitializer(PatternBindingDecl *binding,
                                      unsigned bindingIndex);

  /// Write the components of a DefaultArgumentInitializer as a local context.
  void writeDefaultArgumentInitializer(const DeclContext *parentContext, unsigned index);

  /// Write the components of an AbstractClosureExpr as a local context.
  void writeAbstractClosureExpr(const DeclContext *parentContext, Type Ty, bool isImplicit, unsigned discriminator);

  /// Writes the given type.
  void writeASTBlockEntity(Type ty);

  /// Writes a generic signature.
  void writeASTBlockEntity(const GenericSignature *sig);

  /// Writes a substitution map.
  void writeASTBlockEntity(const SubstitutionMap substitutions);

  /// Registers the abbreviation for the given decl or type layout.
  template <typename Layout>
  void registerDeclTypeAbbr() {
    using AbbrArrayTy = decltype(DeclTypeAbbrCodes);
    static_assert(Layout::Code <= std::tuple_size<AbbrArrayTy>::value,
                  "layout has invalid record code");
    DeclTypeAbbrCodes[Layout::Code] = Layout::emitAbbrev(Out);
  }

  /// Writes all queued \p entities until there are no more.
  ///
  /// \returns true if any entities were written
  template <typename SpecificASTBlockRecordKeeper>
  bool writeASTBlockEntitiesIfNeeded(SpecificASTBlockRecordKeeper &entities);

  /// Writes all decls and types in the DeclsToWrite queue.
  ///
  /// This will continue until the queue is empty, even if the items currently
  /// in the queue trigger the serialization of additional decls and/or types.
  void writeAllDeclsAndTypes();

  /// Writes all identifiers in the IdentifiersToWrite queue.
  ///
  /// This must be called after writeAllDeclsAndTypes(), since that may add
  /// additional identifiers to the pool.
  std::vector<CharOffset> writeAllIdentifiers();

  /// Writes the offsets for a serialized entity kind.
  ///
  /// \see ASTBlockRecordKeeper
  template <typename SpecificASTBlockRecordKeeper>
  void writeOffsets(const index_block::OffsetsLayout &Offsets,
                    const SpecificASTBlockRecordKeeper &entities);

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
  LocalDeclContextID addLocalDeclContextRef(const DeclContext *DC);

  /// Records the use of the given generic signature.
  ///
  /// The GenericSignature will be scheduled for serialization if necessary.
  GenericSignatureID addGenericSignatureRef(const GenericSignature *sig);

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
  SILLayoutID addSILLayoutRef(const SILLayout *layout);

  /// Records the module containing \p DC.
  ///
  /// The module's name will be scheduled for serialization if necessary. This
  /// may not be exactly the same as the name of the module containing DC;
  /// instead, it will match the containing file's "exported module name".
  ///
  /// \returns The ID for the identifier for the module's name, or one of the
  /// special module codes defined above.
  /// \see FileUnit::getExportedModuleName
  IdentifierID addContainingModuleRef(const DeclContext *DC);

  /// Write a normal protocol conformance.
  void writeASTBlockEntity(const NormalProtocolConformance *conformance);

  /// Write a SILLayout.
  void writeASTBlockEntity(const SILLayout *layout);

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
