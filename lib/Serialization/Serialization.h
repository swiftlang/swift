//===--- Serialization.h - Read and write Swift modules -------------------===//
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
//
//  This file defines the Serializer interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_SERIALIZATION_H
#define SWIFT_SERIALIZATION_SERIALIZATION_H

#include "ModuleFormat.h"
#include "swift/Subsystems.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/LLVM.h"
#include <array>
#include <queue>

namespace swift {
  class SILModule;
  class TranslationUnit;

namespace serialization {

typedef ArrayRef<std::string> FilenamesTy;

class Serializer {
  SmallVector<char, 0> Buffer;
  llvm::BitstreamWriter Out{Buffer};

  /// A reusable buffer for emitting records.
  SmallVector<uint64_t, 64> ScratchRecord;

  /// The TranslationUnit currently being serialized.
  const TranslationUnit *TU = nullptr;

  /// The SourceFile currently being serialized, if any.
  ///
  /// If this is non-null, only decls actually from this SourceFile will be
  /// serialized. Any other decls will be cross-referenced instead.
  const SourceFile *SF = nullptr;

  /// The SILModule currently being serialized.
  const SILModule *M = nullptr;

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
  llvm::DenseMap<DeclTypeUnion, DeclID> DeclIDs;

  /// A map from Identifiers to their serialized IDs.
  llvm::DenseMap<Identifier, IdentifierID> IdentifierIDs;

  /// A map from generic parameter lists to the decls they come from.
  llvm::DenseMap<const GenericParamList *, const Decl *> GenericContexts;

public:
  using DeclTableData = SmallVector<std::pair<uint8_t, DeclID>, 4>;
  /// The in-memory representation of what will eventually be an on-disk hash
  /// table.
  using DeclTable = llvm::DenseMap<Identifier, DeclTableData>;

private:
  /// A map from identifiers to methods and properties with the given name.
  ///
  /// This is used for id-style lookup.
  DeclTable ClassMembersByName;

  /// The queue of types and decls that need to be serialized.
  ///
  /// This is a queue and not simply a vector because serializing one
  /// decl-or-type might trigger the serialization of another one.
  std::queue<DeclTypeUnion> DeclsAndTypesToWrite;

  /// All identifiers that need to be serialized.
  std::vector<Identifier> IdentifiersToWrite;

  /// The abbreviation code for each record in the "decls-and-types" block.
  ///
  /// These are registered up front when entering the block, so they can be
  /// reused.
  std::array<unsigned, 256> DeclTypeAbbrCodes;

  /// The offset of each Decl in the bitstream, indexed by DeclID.
  std::vector<BitOffset> DeclOffsets;

  /// The offset of each Type in the bitstream, indexed by TypeID.
  std::vector<BitOffset> TypeOffsets;

  /// The offset of each Identifier in the identifier data block, indexed by
  /// IdentifierID.
  std::vector<CharOffset> IdentifierOffsets;

  SmallVector<DeclID, 2> KnownProtocolAdopters[NumKnownProtocols];
  SmallVector<DeclID, 2> EagerDeserializationDecls;

  /// The last assigned DeclID for decls from this module.
  DeclID LastDeclID = 0;

  /// The last assigned DeclID for types from this module.
  TypeID LastTypeID = 0;

  /// The last assigned IdentifierID for types from this module.
  ///
  /// Note that special module IDs must not be valid IdentifierIDs, except that
  /// 0 will always represent the empty identifier.
  IdentifierID LastIdentifierID = serialization::NUM_SPECIAL_MODULES - 1;

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
    llvm_unreachable("unknown offset kind");
  }

  /// Returns the declaration the given generic parameter list is associated
  /// with.
  const Decl *getGenericContext(const GenericParamList *paramList);

  /// Writes the BLOCKINFO block.
  void writeBlockInfoBlock();

  /// Writes the Swift module file header, BLOCKINFO block, and
  /// non-TU-specific metadata.
  void writeHeader();

  /// Writes the dependencies used to build this module: its imported
  /// modules and its source files.
  void writeInputFiles(const TranslationUnit *TU, FilenamesTy inputFiles,
                       StringRef moduleLinkName);

  /// Writes the given pattern, recursively.
  void writePattern(const Pattern *pattern);

  /// Writes a set of generic requirements.
  void writeRequirements(ArrayRef<Requirement> requirements);

  /// Writes a generic parameter list.
  bool writeGenericParams(const GenericParamList *genericParams);

  /// Encode the underlying conformance of a generic or specialized
  /// conformance.
  ///
  /// \param conformance The conformance we're encoding.
  ///
  /// \param typeID Will be set to the "type ID" value to be stored
  /// in the parent record.
  ///
  /// \param moduleID Will be set to the "module ID" value to
  /// be stored in the parent record.
  ///
  /// \returns true if the underlying conformance will need to be written
  /// out as its own record following the parent record.
  bool encodeUnderlyingConformance(const ProtocolConformance *conformance,
                                   DeclID &typeID,
                                   IdentifierID &moduleID);

  /// Writes a protocol conformance.
  void writeConformance(const ProtocolDecl *protocol,
                        const ProtocolConformance *conformance,
                        const Decl *associatedDecl,
                        const std::array<unsigned, 256> &abbrCodes);

  /// Writes a list of protocol conformances.
  void writeConformances(ArrayRef<ProtocolDecl *> protocols,
                         ArrayRef<ProtocolConformance *> conformances,
                         const Decl *associatedDecl,
                         const std::array<unsigned, 256> &abbrCodes);

  /// Writes an array of members for a decl context.
  ///
  /// \param members The decls within the context
  /// \param isClass True if the context could be a class context (class,
  ///        class extension, or protocol).
  void writeMembers(ArrayRef<Decl *> members, bool isClass);

  /// Writes a reference to a decl in another module.
  void writeCrossReference(const Decl *D);

  /// Writes the given decl.
  ///
  /// Returns false if the decl cannot be serialized without losing
  /// information.
  void writeDecl(const Decl *D);

  /// Writes the given type.
  ///
  /// Returns false if the type cannot be serialized without losing
  /// information.
  void writeType(Type ty);

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
  void writeSILFunctions(const SILModule *M);

  /// Top-level entry point for serializing a translation unit module.
  void writeTranslationUnit(TranslationUnitOrSourceFile DC, const SILModule *M);

public:
  Serializer() = default;

  /// Serialize a translation unit to the given stream.
  void writeToStream(raw_ostream &os, TranslationUnitOrSourceFile DC,
                     const SILModule *M, FilenamesTy inputFiles,
                     StringRef moduleLinkName);

  /// Records the use of the given Type.
  ///
  /// The Type will be scheduled for serialization if necessary.
  ///
  /// \returns The ID for the given Type in this module.
  TypeID addTypeRef(Type ty);

  /// Records the use of the given Identifier.
  ///
  /// The Identifier will be scheduled for serialization if necessary.
  ///
  /// \returns The ID for the given Identifier in this module.
  IdentifierID addIdentifierRef(Identifier ident);

  /// Records the use of the given Decl.
  ///
  /// The Decl will be scheduled for serialization if necessary.
  ///
  /// \returns The ID for the given Decl in this module.
  DeclID addDeclRef(const Decl *D);

  /// Records the use of the given module.
  ///
  /// The module's name will be scheduled for serialization if necessary.
  ///
  /// \returns The ID for the identifier for the module's name, or one of the
  /// special module codes defined above.
  IdentifierID addModuleRef(const Module *M);

  /// Writes a list of generic substitutions. abbrCode is needed to support
  /// usage out of decl block.
  void writeSubstitutions(ArrayRef<Substitution> substitutions,
                          const std::array<unsigned, 256> &abbrCodes);
};
} // end namespace serialization
} // end namespace swift
#endif
