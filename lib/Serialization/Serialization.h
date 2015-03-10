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

#include "swift/Serialization/ModuleFormat.h"
#include "swift/Serialization/SerializationOptions.h"
#include "swift/Subsystems.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/LLVM.h"
#include <array>
#include <queue>
#include <tuple>

namespace swift {
  class SILModule;

namespace serialization {

typedef ArrayRef<std::string> FilenamesTy;

class Serializer {
  SmallVector<char, 0> Buffer;
  llvm::BitstreamWriter Out{Buffer};

  /// A reusable buffer for emitting records.
  SmallVector<uint64_t, 64> ScratchRecord;

  /// The module currently being serialized.
  const Module *M = nullptr;

  /// The SourceFile currently being serialized, if any.
  ///
  /// If this is non-null, only decls actually from this SourceFile will be
  /// serialized. Any other decls will be cross-referenced instead.
  const SourceFile *SF = nullptr;

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

  // FIXME: This should be a PointerIntPair, but there's a bug in
  // PointerIntPair when the number of free bits is greater than 32.
  using DeclIDAndForce = std::pair<DeclID, bool>;

private:
  /// A map from Types and Decls to their serialized IDs.
  llvm::DenseMap<DeclTypeUnion, DeclIDAndForce> DeclAndTypeIDs;

  /// A map from Identifiers to their serialized IDs.
  llvm::DenseMap<Identifier, IdentifierID> IdentifierIDs;

  /// A map from DeclContexts to their serialized IDs.
  llvm::DenseMap<const DeclContext*, DeclContextID> DeclContextIDs;

  /// A map from local DeclContexts to their serialized IDs.
  llvm::DenseMap<const DeclContext*, DeclContextID> LocalDeclContextIDs;

  /// A map from generic parameter lists to the decls they come from.
  llvm::DenseMap<const GenericParamList *, const Decl *> GenericContexts;

public:
  using DeclTableData = SmallVector<std::pair<uint8_t, DeclID>, 4>;
  /// The in-memory representation of what will eventually be an on-disk hash
  /// table.
  using DeclTable = llvm::DenseMap<Identifier, DeclTableData>;

  /// Returns the declaration the given generic parameter list is associated
  /// with.
  const Decl *getGenericContext(const GenericParamList *paramList);

  using ObjCMethodTableData = SmallVector<std::tuple<TypeID, bool, DeclID>, 4>;

  // In-memory representation of what will eventually be an on-disk
  // hash table of all defined Objective-C methods.
  using ObjCMethodTable = llvm::DenseMap<ObjCSelector, ObjCMethodTableData>;

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

  /// DeclContexts that need to be serialized.
  std::queue<const DeclContext*> DeclContextsToWrite;

  /// Local DeclContexts that need to be serialized.
  std::queue<const DeclContext*> LocalDeclContextsToWrite;

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

  /// The offset of each DeclContext in the bitstream, indexed by DeclContextID
  std::vector<BitOffset> DeclContextOffsets;

  /// The offset of each localDeclContext in the bitstream,
  /// indexed by DeclContextID
  std::vector<BitOffset> LocalDeclContextOffsets;

  /// The offset of each Identifier in the identifier data block, indexed by
  /// IdentifierID.
  std::vector<CharOffset> IdentifierOffsets;

  /// The decls that adopt compiler-known protocols.
  SmallVector<DeclID, 2> KnownProtocolAdopters[NumKnownProtocols];

  /// The last assigned DeclID for decls from this module.
  DeclID LastDeclID = 0;

  /// The last assigned DeclContextID for decl contexts from this module.
  DeclContextID LastDeclContextID = 0;

  /// The last assigned DeclContextID for local decl contexts from this module.
  DeclContextID LastLocalDeclContextID = 0;

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
    if (&values == &DeclContextOffsets)
      return index_block::DECL_CONTEXT_OFFSETS;
    if (&values == &LocalDeclContextOffsets)
      return index_block::LOCAL_DECL_CONTEXT_OFFSETS;
    llvm_unreachable("unknown offset kind");
  }

  /// Writes the BLOCKINFO block for the serialized module file.
  void writeBlockInfoBlock();

  /// Writes the BLOCKINFO block for the module documentation file.
  void writeDocBlockInfoBlock();

  /// Writes the Swift module file header and name, plus metadata determining
  /// if the module can be loaded.
  void writeHeader(const SerializationOptions &options = {});

  /// Writes the Swift doc module file header and name.
  void writeDocHeader();

  /// Writes the dependencies used to build this module: its imported
  /// modules and its source files.
  void writeInputBlock(const SerializationOptions &options);

  /// Writes the given pattern, recursively.
  void writePattern(const Pattern *pattern);

  /// Writes a set of generic requirements.
  void writeRequirements(ArrayRef<Requirement> requirements);

  /// Writes a list of protocol conformances.
  void writeConformances(ArrayRef<ProtocolConformance *> conformances,
                         const Decl *associatedDecl,
                         const std::array<unsigned, 256> &abbrCodes,
                         bool writeIncomplete = false);

  /// Writes an array of members for a decl context.
  ///
  /// \param members The decls within the context
  /// \param isClass True if the context could be a class context (class,
  ///        class extension, or protocol).
  void writeMembers(DeclRange members, bool isClass);

  /// Check if a decl is cross-referenced.
  bool isDeclXRef(const Decl *D) const;

  /// Writes a reference to a decl in another module.
  void writeCrossReference(const DeclContext *DC, uint32_t pathLen = 1);

  /// Writes a reference to a decl in another module.
  void writeCrossReference(const Decl *D);

  /// Writes out a declaration attribute.
  void writeDeclAttribute(const DeclAttribute *DA);

  /// Writes the given decl.
  void writeDecl(const Decl *D);

  /// Writes the given decl context.
  void writeDeclContext(const DeclContext *DC);

  /// Write a DeclContext as a local DeclContext at the current offset.
  void writeLocalDeclContext(const DeclContext *DC);

  /// Write the components of a PatternBindingInitializer as a local context.
  void writePatternBindingInitializer(PatternBindingDecl *binding);

  /// Write the components of a DefaultArgumentInitializer as a local context.
  void writeDefaultArgumentInitializer(const DeclContext *parentContext, unsigned index);

  /// Write the components of an AbstractClosureExpr as a local context.
  void writeAbstractClosureExpr(const DeclContext *parentContext, Type Ty, bool isImplicit, unsigned discriminator);

  /// Writes the given type.
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
  void writeSIL(const SILModule *M, bool serializeAllSIL);

  /// Top-level entry point for serializing a module.
  void writeAST(ModuleOrSourceFile DC);

  void writeToStream(raw_ostream &os);

  template <size_t N>
  Serializer(const unsigned char (&signature)[N], ModuleOrSourceFile DC);

public:
  /// Serialize a module to the given stream.
  static void writeToStream(raw_ostream &os, ModuleOrSourceFile DC,
                            const SILModule *M,
                            const SerializationOptions &options);

  /// Serialize module documentation to the given stream.
  static void writeDocToStream(raw_ostream &os, ModuleOrSourceFile DC);

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
  DeclID addDeclRef(const Decl *D, bool forceSerialization = false);

  /// Records the use of the given DeclContext.
  ///
  /// The DeclContext will be scheduled for serialization if necessary.
  DeclContextID addDeclContextRef(const DeclContext *DC);

  /// Records the use of the given local DeclContext.
  ///
  /// The DeclContext will be scheduled for serialization if necessary.
  DeclContextID addLocalDeclContextRef(const DeclContext *DC);

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

  /// Writes a protocol conformance.
  void writeConformance(const ProtocolDecl *protocol,
                        const ProtocolConformance *conformance,
                        const Decl *associatedDecl,
                        const std::array<unsigned, 256> &abbrCodes,
                        bool writeIncomplete = false);

  /// Encode a reference to another conformance.
  ///
  /// This is used for the conformances of substitutions and for the
  /// underlying conformances of specialized and inherited conformances.
  ///
  /// \param conformance The conformance we're encoding.
  ///
  /// \param typeID Will be set to the "type ID" value to be stored
  /// in the parent record.
  ///
  /// \param moduleID Will be set to the "module ID" value to
  /// be stored in the parent record.
  ///
  /// \param allowReferencingCurrentModule If false, conformances in the current
  /// module will always be appended rather than referenced.
  ///
  /// \returns true if the underlying conformance will need to be written
  /// out as its own record following the parent record.
  bool encodeReferencedConformance(const ProtocolConformance *conformance,
                                   DeclID &typeID, ModuleID &moduleID,
                                   bool allowReferencingCurrentModule);

  /// Writes a generic parameter list.
  bool writeGenericParams(const GenericParamList *genericParams,
                          const std::array<unsigned, 256> &abbrCodes);

};
} // end namespace serialization
} // end namespace swift
#endif
