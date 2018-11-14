//===--- Serialization.cpp - Read and write Swift modules -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "Serialization.h"
#include "SILFormat.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/Expr.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/RawComment.h"
#include "swift/Basic/Dwarf.h"
#include "swift/Basic/FileSystem.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/Timer.h"
#include "swift/Basic/Version.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Serialization/SerializationOptions.h"
#include "swift/Strings.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Bitcode/RecordLayout.h"
#include "llvm/Config/config.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Chrono.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/DJB.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/OnDiskHashTable.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

#include <vector>

using namespace swift;
using namespace swift::serialization;
using namespace llvm::support;
using swift::version::Version;
using llvm::BCBlockRAII;

/// Used for static_assert.
static constexpr bool declIDFitsIn32Bits() {
  using Int32Info = std::numeric_limits<uint32_t>;
  using PtrIntInfo = std::numeric_limits<uintptr_t>;
  using DeclIDTraits = llvm::PointerLikeTypeTraits<DeclID>;
  return PtrIntInfo::digits - DeclIDTraits::NumLowBitsAvailable <= Int32Info::digits;
}

/// Used for static_assert.
static constexpr bool bitOffsetFitsIn32Bits() {
  // FIXME: Considering BitOffset is a _bit_ offset, and we're storing it in 31
  // bits of a PointerEmbeddedInt, the maximum offset inside a modulefile we can
  // handle happens at 2**28 _bytes_, which is only 268MB. Considering
  // Swift.swiftmodule is itself 25MB, it seems entirely possible users will
  // exceed this limit.
  using Int32Info = std::numeric_limits<uint32_t>;
  using PtrIntInfo = std::numeric_limits<uintptr_t>;
  using BitOffsetTraits = llvm::PointerLikeTypeTraits<BitOffset>;
  return PtrIntInfo::digits - BitOffsetTraits::NumLowBitsAvailable <= Int32Info::digits;
}

namespace {
  /// Used to serialize the on-disk decl hash table.
  class DeclTableInfo {
  public:
    using key_type = DeclBaseName;
    using key_type_ref = key_type;
    using data_type = Serializer::DeclTableData;
    using data_type_ref = const data_type &;
    using hash_value_type = uint32_t;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      switch (key.getKind()) {
        case DeclBaseName::Kind::Normal:
          assert(!key.empty());
          // FIXME: DJB seed=0, audit whether the default seed could be used.
          return llvm::djbHash(key.getIdentifier().str(), 0);
        case DeclBaseName::Kind::Subscript:
          return static_cast<uint8_t>(DeclNameKind::Subscript);
        case DeclBaseName::Kind::Constructor:
          return static_cast<uint8_t>(DeclNameKind::Constructor);
        case DeclBaseName::Kind::Destructor:
          return static_cast<uint8_t>(DeclNameKind::Destructor);
      }
      llvm_unreachable("unhandled kind");
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      uint32_t keyLength = sizeof(uint8_t); // For the flag of the name's kind
      if (key.getKind() == DeclBaseName::Kind::Normal) {
        keyLength += key.getIdentifier().str().size(); // The name's length
      }
      assert(keyLength == static_cast<uint16_t>(keyLength));

      uint32_t dataLength = (sizeof(uint32_t) + 1) * data.size();
      assert(dataLength == static_cast<uint16_t>(dataLength));

      endian::Writer writer(out, little);
      writer.write<uint16_t>(keyLength);
      writer.write<uint16_t>(dataLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      endian::Writer writer(out, little);
      switch (key.getKind()) {
      case DeclBaseName::Kind::Normal:
        writer.write<uint8_t>(static_cast<uint8_t>(DeclNameKind::Normal));
        writer.OS << key.getIdentifier().str();
        break;
      case DeclBaseName::Kind::Subscript:
        writer.write<uint8_t>(static_cast<uint8_t>(DeclNameKind::Subscript));
        break;
      case DeclBaseName::Kind::Constructor:
        writer.write<uint8_t>(static_cast<uint8_t>(DeclNameKind::Constructor));
        break;
      case DeclBaseName::Kind::Destructor:
        writer.write<uint8_t>(static_cast<uint8_t>(DeclNameKind::Destructor));
        break;
      }
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      static_assert(declIDFitsIn32Bits(), "DeclID too large");
      endian::Writer writer(out, little);
      for (auto entry : data) {
        writer.write<uint8_t>(entry.first);
        writer.write<uint32_t>(entry.second);
      }
    }
  };

  class ExtensionTableInfo {
    serialization::Serializer &Serializer;
    llvm::SmallDenseMap<const NominalTypeDecl *,std::string,4> MangledNameCache;

  public:
    explicit ExtensionTableInfo(serialization::Serializer &serializer)
        : Serializer(serializer) {}

    using key_type = Identifier;
    using key_type_ref = key_type;
    using data_type = Serializer::ExtensionTableData;
    using data_type_ref = const data_type &;
    using hash_value_type = uint32_t;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      assert(!key.empty());
      // FIXME: DJB seed=0, audit whether the default seed could be used.
      return llvm::djbHash(key.str(), 0);
    }

    int32_t getNameDataForBase(const NominalTypeDecl *nominal,
                               StringRef *dataToWrite = nullptr) {
      if (nominal->getDeclContext()->isModuleScopeContext())
        return -Serializer.addModuleRef(nominal->getParentModule());

      auto &mangledName = MangledNameCache[nominal];
      if (mangledName.empty())
        mangledName = Mangle::ASTMangler().mangleNominalType(nominal);

      assert(llvm::isUInt<31>(mangledName.size()));
      if (dataToWrite)
        *dataToWrite = mangledName;
      return mangledName.size();
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      uint32_t keyLength = key.str().size();
      assert(keyLength == static_cast<uint16_t>(keyLength));
      uint32_t dataLength = (sizeof(uint32_t) * 2) * data.size();
      for (auto dataPair : data) {
        int32_t nameData = getNameDataForBase(dataPair.first);
        if (nameData > 0)
          dataLength += nameData;
      }
      assert(dataLength == static_cast<uint16_t>(dataLength));
      endian::Writer writer(out, little);
      writer.write<uint16_t>(keyLength);
      writer.write<uint16_t>(dataLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      out << key.str();
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      static_assert(declIDFitsIn32Bits(), "DeclID too large");
      endian::Writer writer(out, little);
      for (auto entry : data) {
        StringRef dataToWrite;
        writer.write<uint32_t>(entry.second);
        writer.write<int32_t>(getNameDataForBase(entry.first, &dataToWrite));
        out << dataToWrite;
      }
    }
  };

  class LocalDeclTableInfo {
  public:
    using key_type = std::string;
    using key_type_ref = StringRef;
    using data_type = DeclID;
    using data_type_ref = const data_type &;
    using hash_value_type = uint32_t;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      assert(!key.empty());
      // FIXME: DJB seed=0, audit whether the default seed could be used.
      return llvm::djbHash(key, 0);
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      uint32_t keyLength = key.size();
      assert(keyLength == static_cast<uint16_t>(keyLength));
      uint32_t dataLength = sizeof(uint32_t);
      endian::Writer writer(out, little);
      writer.write<uint16_t>(keyLength);
      // No need to write the data length; it's constant.
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      out << key;
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      static_assert(declIDFitsIn32Bits(), "DeclID too large");
      endian::Writer writer(out, little);
      writer.write<uint32_t>(data);
    }
  };

  using LocalTypeHashTableGenerator =
    llvm::OnDiskChainedHashTableGenerator<LocalDeclTableInfo>;

  class NestedTypeDeclsTableInfo {
  public:
    using key_type = Identifier;
    using key_type_ref = const key_type &;
    using data_type = Serializer::NestedTypeDeclsData; // (parent, child) pairs
    using data_type_ref = const data_type &;
    using hash_value_type = uint32_t;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      assert(!key.empty());
      // FIXME: DJB seed=0, audit whether the default seed could be used.
      return llvm::djbHash(key.str(), 0);
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      uint32_t keyLength = key.str().size();
      assert(keyLength == static_cast<uint16_t>(keyLength));
      uint32_t dataLength = (sizeof(uint32_t) * 2) * data.size();
      assert(dataLength == static_cast<uint16_t>(dataLength));
      endian::Writer writer(out, little);
      writer.write<uint16_t>(keyLength);
      writer.write<uint16_t>(dataLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      // FIXME: Avoid writing string data for identifiers here.
      out << key.str();
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      static_assert(declIDFitsIn32Bits(), "DeclID too large");
      endian::Writer writer(out, little);
      for (auto entry : data) {
        writer.write<uint32_t>(entry.first);
        writer.write<uint32_t>(entry.second);
      }
    }
  };

  class DeclMemberNamesTableInfo {
  public:
    using key_type = DeclBaseName;
    using key_type_ref = const key_type &;
    using data_type = BitOffset; // Offsets to sub-tables
    using data_type_ref = const data_type &;
    using hash_value_type = uint32_t;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      switch (key.getKind()) {
      case DeclBaseName::Kind::Normal:
        assert(!key.empty());
        // FIXME: DJB seed=0, audit whether the default seed could be used.
        return llvm::djbHash(key.getIdentifier().str(), 0);
      case DeclBaseName::Kind::Subscript:
        return static_cast<uint8_t>(DeclNameKind::Subscript);
      case DeclBaseName::Kind::Constructor:
        return static_cast<uint8_t>(DeclNameKind::Constructor);
      case DeclBaseName::Kind::Destructor:
        return static_cast<uint8_t>(DeclNameKind::Destructor);
      }
      llvm_unreachable("unhandled kind");
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      uint32_t keyLength = sizeof(uint8_t); // For the flag of the name's kind
      if (key.getKind() == DeclBaseName::Kind::Normal) {
        keyLength += key.getIdentifier().str().size(); // The name's length
      }
      assert(keyLength == static_cast<uint16_t>(keyLength));
      uint32_t dataLength = sizeof(uint32_t);
      endian::Writer writer(out, little);
      writer.write<uint16_t>(keyLength);
      // No need to write dataLength, it's constant.
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      endian::Writer writer(out, little);
      switch (key.getKind()) {
      case DeclBaseName::Kind::Normal:
        writer.write<uint8_t>(static_cast<uint8_t>(DeclNameKind::Normal));
        writer.OS << key.getIdentifier().str();
        break;
      case DeclBaseName::Kind::Subscript:
        writer.write<uint8_t>(static_cast<uint8_t>(DeclNameKind::Subscript));
        break;
      case DeclBaseName::Kind::Constructor:
        writer.write<uint8_t>(static_cast<uint8_t>(DeclNameKind::Constructor));
        break;
      case DeclBaseName::Kind::Destructor:
        writer.write<uint8_t>(static_cast<uint8_t>(DeclNameKind::Destructor));
        break;
      }
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      static_assert(bitOffsetFitsIn32Bits(), "BitOffset too large");
      endian::Writer writer(out, little);
      writer.write<uint32_t>(static_cast<uint32_t>(data));
    }
  };

  class DeclMembersTableInfo {
  public:
    using key_type = DeclID;
    using key_type_ref = const key_type &;
    using data_type = Serializer::DeclMembersData; // Vector of DeclIDs
    using data_type_ref = const data_type &;
    using hash_value_type = uint32_t;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      return llvm::hash_value(static_cast<uint32_t>(key));
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      // This will trap if a single ValueDecl has more than 16383 members
      // with the same DeclBaseName. Seems highly unlikely.
      assert((data.size() < (1 << 14)) && "Too many members");
      uint32_t dataLength = sizeof(uint32_t) * data.size(); // value DeclIDs
      endian::Writer writer(out, little);
      // No need to write the key length; it's constant.
      writer.write<uint16_t>(dataLength);
      return { sizeof(uint32_t), dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      static_assert(declIDFitsIn32Bits(), "DeclID too large");
      assert(len == sizeof(uint32_t));
      endian::Writer writer(out, little);
      writer.write<uint32_t>(key);
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      static_assert(declIDFitsIn32Bits(), "DeclID too large");
      endian::Writer writer(out, little);
      for (auto entry : data) {
        writer.write<uint32_t>(entry);
      }
    }
  };
} // end anonymous namespace

namespace llvm {
  template<> struct DenseMapInfo<Serializer::DeclTypeUnion> {
    using DeclTypeUnion = Serializer::DeclTypeUnion;
    static inline DeclTypeUnion getEmptyKey() { return nullptr; }
    static inline DeclTypeUnion getTombstoneKey() { return swift::Type(); }
    static unsigned getHashValue(const DeclTypeUnion &val) {
      return DenseMapInfo<const void *>::getHashValue(val.getOpaqueValue());
    }
    static bool isEqual(const DeclTypeUnion &lhs, const DeclTypeUnion &rhs) {
      return lhs == rhs;
    }
  };
} // namespace llvm

static ModuleDecl *getModule(ModuleOrSourceFile DC) {
  if (auto M = DC.dyn_cast<ModuleDecl *>())
    return M;
  return DC.get<SourceFile *>()->getParentModule();
}

static ASTContext &getContext(ModuleOrSourceFile DC) {
  return getModule(DC)->getASTContext();
}

static bool shouldSerializeAsLocalContext(const DeclContext *DC) {
  return DC->isLocalContext() && !isa<AbstractFunctionDecl>(DC) &&
        !isa<SubscriptDecl>(DC);
}

static const Decl *getDeclForContext(const DeclContext *DC) {
  switch (DC->getContextKind()) {
  case DeclContextKind::Module:
    // Use a null decl to represent the module.
    return nullptr;
  case DeclContextKind::FileUnit:
    return getDeclForContext(DC->getParent());
  case DeclContextKind::SerializedLocal:
    llvm_unreachable("Serialized local contexts should only come from deserialization");
  case DeclContextKind::Initializer:
  case DeclContextKind::AbstractClosureExpr:
    // FIXME: What about default functions?
    llvm_unreachable("shouldn't serialize decls from anonymous closures");
  case DeclContextKind::GenericTypeDecl:
    return cast<GenericTypeDecl>(DC);
  case DeclContextKind::ExtensionDecl:
    return cast<ExtensionDecl>(DC);
  case DeclContextKind::TopLevelCodeDecl:
    llvm_unreachable("shouldn't serialize the main module");
  case DeclContextKind::AbstractFunctionDecl:
    return cast<AbstractFunctionDecl>(DC);
  case DeclContextKind::SubscriptDecl:
    return cast<SubscriptDecl>(DC);
  }

  llvm_unreachable("Unhandled DeclContextKind in switch.");
}

namespace {
  struct Accessors {
    uint8_t OpaqueReadOwnership;
    uint8_t ReadImpl, WriteImpl, ReadWriteImpl;
    SmallVector<AccessorDecl *, 8> Decls;
  };
} // end anonymous namespace

static uint8_t getRawOpaqueReadOwnership(swift::OpaqueReadOwnership ownership) {
  switch (ownership) {
#define CASE(KIND)                                            \
  case swift::OpaqueReadOwnership::KIND:                      \
    return uint8_t(serialization::OpaqueReadOwnership::KIND);
  CASE(Owned)
  CASE(Borrowed)
  CASE(OwnedOrBorrowed)
#undef CASE
  }
  llvm_unreachable("bad kind");  
}

static uint8_t getRawReadImplKind(swift::ReadImplKind kind) {
  switch (kind) {
#define CASE(KIND)                                     \
  case swift::ReadImplKind::KIND:                      \
    return uint8_t(serialization::ReadImplKind::KIND);
  CASE(Stored)
  CASE(Get)
  CASE(Inherited)
  CASE(Address)
  CASE(Read)
#undef CASE
  }
  llvm_unreachable("bad kind");
}

static unsigned getRawWriteImplKind(swift::WriteImplKind kind) {
  switch (kind) {
#define CASE(KIND)                                      \
  case swift::WriteImplKind::KIND:                      \
    return uint8_t(serialization::WriteImplKind::KIND);
  CASE(Immutable)
  CASE(Stored)
  CASE(Set)
  CASE(StoredWithObservers)
  CASE(InheritedWithObservers)
  CASE(MutableAddress)
  CASE(Modify)
#undef CASE
  }
  llvm_unreachable("bad kind");
}

static unsigned getRawReadWriteImplKind(swift::ReadWriteImplKind kind) {
  switch (kind) {
#define CASE(KIND)                                          \
  case swift::ReadWriteImplKind::KIND:                      \
    return uint8_t(serialization::ReadWriteImplKind::KIND);
  CASE(Immutable)
  CASE(Stored)
  CASE(MutableAddress)
  CASE(MaterializeToTemporary)
  CASE(Modify)
#undef CASE
  }
  llvm_unreachable("bad kind");
}

static Accessors getAccessors(const AbstractStorageDecl *storage) {
  Accessors accessors;
  accessors.OpaqueReadOwnership =
    getRawOpaqueReadOwnership(storage->getOpaqueReadOwnership());
  auto impl = storage->getImplInfo();
  accessors.ReadImpl = getRawReadImplKind(impl.getReadImpl());
  accessors.WriteImpl = getRawWriteImplKind(impl.getWriteImpl());
  accessors.ReadWriteImpl = getRawReadWriteImplKind(impl.getReadWriteImpl());
  auto decls = storage->getAllAccessors();
  accessors.Decls.append(decls.begin(), decls.end());
  return accessors;
}

DeclID Serializer::addLocalDeclContextRef(const DeclContext *DC) {
  assert(DC->isLocalContext() && "Expected a local DeclContext");
  auto &id = LocalDeclContextIDs[DC];
  if (id != 0)
    return id;

  id = ++LastLocalDeclContextID;
  LocalDeclContextsToWrite.push(DC);
  return id;
}

GenericSignatureID Serializer::addGenericSignatureRef(
                                                const GenericSignature *env) {
  if (!env) return 0;

  auto &id = GenericSignatureIDs[env];
  if (id != 0)
    return id;

  id = ++LastGenericSignatureID;
  GenericSignaturesToWrite.push(env);
  return id;
}

GenericEnvironmentID Serializer::addGenericEnvironmentRef(
                                                const GenericEnvironment *env) {
  if (!env) return 0;

  auto &id = GenericEnvironmentIDs[env];
  if (id != 0)
    return id;

  id = ++LastGenericEnvironmentID;
  GenericEnvironmentsToWrite.push(env);
  return id;
}

SubstitutionMapID Serializer::addSubstitutionMapRef(
                                              SubstitutionMap substitutions) {
  if (!substitutions) return 0;

  auto &id = SubstitutionMapIDs[substitutions];
  if (id != 0)
    return id;

  id = ++LastSubstitutionMapID;
  SubstitutionMapsToWrite.push(substitutions);
  return id;
}

DeclContextID Serializer::addDeclContextRef(const DeclContext *DC) {
  switch (DC->getContextKind()) {
  case DeclContextKind::Module:
  case DeclContextKind::FileUnit: // Skip up to the module
    return 0;
  default:
    break;
  }

  // If this decl context is a plain old serializable decl, queue it up for
  // normal serialization.
  if (shouldSerializeAsLocalContext(DC))
    addLocalDeclContextRef(DC);
  else
    addDeclRef(getDeclForContext(DC));

  auto &id = DeclContextIDs[DC];
  if (id)
    return id;

  id = ++LastDeclContextID;
  DeclContextsToWrite.push(DC);

  return id;
}

DeclID Serializer::addDeclRef(const Decl *D, bool allowTypeAliasXRef) {
  if (!D)
    return 0;

  DeclID &id = DeclAndTypeIDs[D];
  if (id != 0)
    return id;

  assert((!isDeclXRef(D) || isa<ValueDecl>(D) || isa<OperatorDecl>(D) ||
          isa<PrecedenceGroupDecl>(D)) &&
         "cannot cross-reference this decl");

  assert((!isDeclXRef(D) ||
          !D->getAttrs().hasAttribute<ForbidSerializingReferenceAttr>()) &&
         "cannot cross-reference this decl");

  assert((allowTypeAliasXRef || !isa<TypeAliasDecl>(D) ||
          D->getModuleContext() == M) &&
         "cannot cross-reference typealiases directly (use the NameAliasType)");

  id = ++LastDeclID;
  DeclsAndTypesToWrite.push(D);
  return id;
}

serialization::TypeID Serializer::addTypeRef(Type ty) {
  if (!ty)
    return 0;

#ifndef NDEBUG
  PrettyStackTraceType trace(M->getASTContext(), "serializing", ty);
  assert(!ty->hasError() && "Serializing error type");
#endif

  auto &id = DeclAndTypeIDs[ty];
  if (id != 0)
    return id;

  id = ++LastTypeID;
  DeclsAndTypesToWrite.push(ty);
  return id;
}

IdentifierID Serializer::addDeclBaseNameRef(DeclBaseName ident) {
  switch (ident.getKind()) {
  case DeclBaseName::Kind::Normal: {
    if (ident.empty())
      return 0;

    IdentifierID &id = IdentifierIDs[ident.getIdentifier()];
    if (id != 0)
      return id;

    id = ++LastUniquedStringID;
    StringsToWrite.push_back(ident.getIdentifier().str());
    return id;
  }
  case DeclBaseName::Kind::Subscript:
    return SUBSCRIPT_ID;
  case DeclBaseName::Kind::Constructor:
    return CONSTRUCTOR_ID;
  case DeclBaseName::Kind::Destructor:
    return DESTRUCTOR_ID;
  }
  llvm_unreachable("unhandled kind");
}

std::pair<StringRef, IdentifierID> Serializer::addUniquedString(StringRef str) {
  if (str.empty())
    return {str, 0};

  decltype(UniquedStringIDs)::iterator iter;
  bool isNew;
  std::tie(iter, isNew) =
      UniquedStringIDs.insert({str, LastUniquedStringID + 1});

  if (!isNew)
    return {iter->getKey(), iter->getValue()};

  ++LastUniquedStringID;
  // Note that we use the string data stored in the StringMap.
  StringsToWrite.push_back(iter->getKey());
  return {iter->getKey(), LastUniquedStringID};
}

IdentifierID Serializer::addFilename(StringRef filename) {
  assert(!filename.empty() && "Attemping to add an empty filename");

  return addUniquedString(filename).second;
}

IdentifierID Serializer::addModuleRef(const ModuleDecl *M) {
  if (M == this->M)
    return CURRENT_MODULE_ID;
  if (M == this->M->getASTContext().TheBuiltinModule)
    return BUILTIN_MODULE_ID;

  auto clangImporter =
    static_cast<ClangImporter *>(
      this->M->getASTContext().getClangModuleLoader());
  if (M == clangImporter->getImportedHeaderModule())
    return OBJC_HEADER_MODULE_ID;

  // If we're referring to a member of a private module that will be
  // re-exported via a public module, record the public module's name.
  if (auto clangModuleUnit =
        dyn_cast<ClangModuleUnit>(M->getFiles().front())) {
    auto exportedModuleName =
        M->getASTContext().getIdentifier(
                                 clangModuleUnit->getExportedModuleName());
    return addDeclBaseNameRef(exportedModuleName);
  }

  assert(!M->getName().empty());
  return addDeclBaseNameRef(M->getName());
}

SILLayoutID Serializer::addSILLayoutRef(SILLayout *layout) {
  auto &id = SILLayouts[layout];
  if (id != 0)
    return id;
  
  id = ++LastSILLayoutID;
  SILLayoutsToWrite.push(layout);
  return id;
}

NormalConformanceID Serializer::addConformanceRef(
                      const NormalProtocolConformance *conformance) {
  assert(conformance->getDeclContext()->getParentModule() == M &&
         "cannot reference conformance from another module");
  auto &conformanceID = NormalConformances[conformance];
  if (conformanceID)
    return conformanceID;

  conformanceID = ++LastNormalConformanceID;
  NormalConformancesToWrite.push(conformance);

  return conformanceID;
}

/// Record the name of a block.
void SerializerBase::emitBlockID(unsigned ID, StringRef name,
                                 SmallVectorImpl<unsigned char> &nameBuffer) {
  SmallVector<unsigned, 1> idBuffer;
  idBuffer.push_back(ID);
  Out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_SETBID, idBuffer);

  // Emit the block name if present.
  if (name.empty())
    return;
  nameBuffer.resize(name.size());
  memcpy(nameBuffer.data(), name.data(), name.size());
  Out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_BLOCKNAME, nameBuffer);
}

void SerializerBase::emitRecordID(unsigned ID, StringRef name,
                                  SmallVectorImpl<unsigned char> &nameBuffer) {
  assert(ID < 256 && "can't fit record ID in next to name");
  nameBuffer.resize(name.size()+1);
  nameBuffer[0] = ID;
  memcpy(nameBuffer.data()+1, name.data(), name.size());
  Out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_SETRECORDNAME, nameBuffer);
}

void Serializer::writeBlockInfoBlock() {
  BCBlockRAII restoreBlock(Out, llvm::bitc::BLOCKINFO_BLOCK_ID, 2);

  SmallVector<unsigned char, 64> nameBuffer;
#define BLOCK(X) emitBlockID(X ## _ID, #X, nameBuffer)
#define BLOCK_RECORD(K, X) emitRecordID(K::X, #X, nameBuffer)

  BLOCK(MODULE_BLOCK);

  BLOCK(CONTROL_BLOCK);
  BLOCK_RECORD(control_block, METADATA);
  BLOCK_RECORD(control_block, MODULE_NAME);
  BLOCK_RECORD(control_block, TARGET);

  BLOCK(OPTIONS_BLOCK);
  BLOCK_RECORD(options_block, SDK_PATH);
  BLOCK_RECORD(options_block, XCC);
  BLOCK_RECORD(options_block, IS_SIB);
  BLOCK_RECORD(options_block, IS_TESTABLE);
  BLOCK_RECORD(options_block, ARE_PRIVATE_IMPORTS_ENABLED);
  BLOCK_RECORD(options_block, RESILIENCE_STRATEGY);

  BLOCK(INPUT_BLOCK);
  BLOCK_RECORD(input_block, IMPORTED_MODULE);
  BLOCK_RECORD(input_block, LINK_LIBRARY);
  BLOCK_RECORD(input_block, IMPORTED_HEADER);
  BLOCK_RECORD(input_block, IMPORTED_HEADER_CONTENTS);
  BLOCK_RECORD(input_block, MODULE_FLAGS);
  BLOCK_RECORD(input_block, SEARCH_PATH);
  BLOCK_RECORD(input_block, FILE_DEPENDENCY);

  BLOCK(DECLS_AND_TYPES_BLOCK);
#define RECORD(X) BLOCK_RECORD(decls_block, X);
#include "swift/Serialization/DeclTypeRecordNodes.def"

  BLOCK(IDENTIFIER_DATA_BLOCK);
  BLOCK_RECORD(identifier_block, IDENTIFIER_DATA);

  BLOCK(INDEX_BLOCK);
  BLOCK_RECORD(index_block, TYPE_OFFSETS);
  BLOCK_RECORD(index_block, DECL_OFFSETS);
  BLOCK_RECORD(index_block, IDENTIFIER_OFFSETS);
  BLOCK_RECORD(index_block, TOP_LEVEL_DECLS);
  BLOCK_RECORD(index_block, OPERATORS);
  BLOCK_RECORD(index_block, EXTENSIONS);
  BLOCK_RECORD(index_block, CLASS_MEMBERS_FOR_DYNAMIC_LOOKUP);
  BLOCK_RECORD(index_block, OPERATOR_METHODS);
  BLOCK_RECORD(index_block, OBJC_METHODS);
  BLOCK_RECORD(index_block, ENTRY_POINT);
  BLOCK_RECORD(index_block, LOCAL_DECL_CONTEXT_OFFSETS);
  BLOCK_RECORD(index_block, GENERIC_SIGNATURE_OFFSETS);
  BLOCK_RECORD(index_block, GENERIC_ENVIRONMENT_OFFSETS);
  BLOCK_RECORD(index_block, SUBSTITUTION_MAP_OFFSETS);
  BLOCK_RECORD(index_block, DECL_CONTEXT_OFFSETS);
  BLOCK_RECORD(index_block, LOCAL_TYPE_DECLS);
  BLOCK_RECORD(index_block, NORMAL_CONFORMANCE_OFFSETS);
  BLOCK_RECORD(index_block, SIL_LAYOUT_OFFSETS);
  BLOCK_RECORD(index_block, PRECEDENCE_GROUPS);
  BLOCK_RECORD(index_block, NESTED_TYPE_DECLS);
  BLOCK_RECORD(index_block, DECL_MEMBER_NAMES);
  BLOCK_RECORD(index_block, ORDERED_TOP_LEVEL_DECLS);

  BLOCK(DECL_MEMBER_TABLES_BLOCK);
  BLOCK_RECORD(decl_member_tables_block, DECL_MEMBERS);

  BLOCK(SIL_BLOCK);
  BLOCK_RECORD(sil_block, SIL_FUNCTION);
  BLOCK_RECORD(sil_block, SIL_BASIC_BLOCK);
  BLOCK_RECORD(sil_block, SIL_ONE_VALUE_ONE_OPERAND);
  BLOCK_RECORD(sil_block, SIL_ONE_TYPE);
  BLOCK_RECORD(sil_block, SIL_ONE_OPERAND);
  BLOCK_RECORD(sil_block, SIL_ONE_TYPE_ONE_OPERAND);
  BLOCK_RECORD(sil_block, SIL_ONE_TYPE_VALUES);
  BLOCK_RECORD(sil_block, SIL_TWO_OPERANDS);
  BLOCK_RECORD(sil_block, SIL_TAIL_ADDR);
  BLOCK_RECORD(sil_block, SIL_INST_APPLY);
  BLOCK_RECORD(sil_block, SIL_INST_NO_OPERAND);
  BLOCK_RECORD(sil_block, SIL_VTABLE);
  BLOCK_RECORD(sil_block, SIL_VTABLE_ENTRY);
  BLOCK_RECORD(sil_block, SIL_GLOBALVAR);
  BLOCK_RECORD(sil_block, SIL_INST_CAST);
  BLOCK_RECORD(sil_block, SIL_INIT_EXISTENTIAL);
  BLOCK_RECORD(sil_block, SIL_WITNESS_TABLE);
  BLOCK_RECORD(sil_block, SIL_WITNESS_METHOD_ENTRY);
  BLOCK_RECORD(sil_block, SIL_WITNESS_BASE_ENTRY);
  BLOCK_RECORD(sil_block, SIL_WITNESS_ASSOC_PROTOCOL);
  BLOCK_RECORD(sil_block, SIL_WITNESS_ASSOC_ENTRY);
  BLOCK_RECORD(sil_block, SIL_WITNESS_CONDITIONAL_CONFORMANCE);
  BLOCK_RECORD(sil_block, SIL_DEFAULT_WITNESS_TABLE);
  BLOCK_RECORD(sil_block, SIL_DEFAULT_WITNESS_TABLE_NO_ENTRY);
  BLOCK_RECORD(sil_block, SIL_INST_WITNESS_METHOD);
  BLOCK_RECORD(sil_block, SIL_SPECIALIZE_ATTR);
  BLOCK_RECORD(sil_block, SIL_ONE_OPERAND_EXTRA_ATTR);
  BLOCK_RECORD(sil_block, SIL_TWO_OPERANDS_EXTRA_ATTR);

  // These layouts can exist in both decl blocks and sil blocks.
#define BLOCK_RECORD_WITH_NAMESPACE(K, X) emitRecordID(X, #X, nameBuffer)
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::INVALID_PROTOCOL_CONFORMANCE);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::ABSTRACT_PROTOCOL_CONFORMANCE);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::NORMAL_PROTOCOL_CONFORMANCE);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::SPECIALIZED_PROTOCOL_CONFORMANCE);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::INHERITED_PROTOCOL_CONFORMANCE);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::NORMAL_PROTOCOL_CONFORMANCE_ID);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::PROTOCOL_CONFORMANCE_XREF);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::GENERIC_PARAM_LIST);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::GENERIC_REQUIREMENT);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::LAYOUT_REQUIREMENT);

  BLOCK(SIL_INDEX_BLOCK);
  BLOCK_RECORD(sil_index_block, SIL_FUNC_NAMES);
  BLOCK_RECORD(sil_index_block, SIL_FUNC_OFFSETS);
  BLOCK_RECORD(sil_index_block, SIL_VTABLE_NAMES);
  BLOCK_RECORD(sil_index_block, SIL_VTABLE_OFFSETS);
  BLOCK_RECORD(sil_index_block, SIL_GLOBALVAR_NAMES);
  BLOCK_RECORD(sil_index_block, SIL_GLOBALVAR_OFFSETS);
  BLOCK_RECORD(sil_index_block, SIL_WITNESS_TABLE_NAMES);
  BLOCK_RECORD(sil_index_block, SIL_WITNESS_TABLE_OFFSETS);
  BLOCK_RECORD(sil_index_block, SIL_DEFAULT_WITNESS_TABLE_NAMES);
  BLOCK_RECORD(sil_index_block, SIL_DEFAULT_WITNESS_TABLE_OFFSETS);
  BLOCK_RECORD(sil_index_block, SIL_PROPERTY_OFFSETS);

#undef BLOCK
#undef BLOCK_RECORD
}

void Serializer::writeHeader(const SerializationOptions &options) {
  {
    BCBlockRAII restoreBlock(Out, CONTROL_BLOCK_ID, 3);
    control_block::ModuleNameLayout ModuleName(Out);
    control_block::MetadataLayout Metadata(Out);
    control_block::TargetLayout Target(Out);

    ModuleName.emit(ScratchRecord, M->getName().str());

    SmallString<32> versionStringBuf;
    llvm::raw_svector_ostream versionString(versionStringBuf);
    versionString << Version::getCurrentLanguageVersion();
    size_t shortVersionStringLength = versionString.tell();
    versionString << '('
                  << M->getASTContext().LangOpts.EffectiveLanguageVersion;
    size_t compatibilityVersionStringLength =
        versionString.tell() - shortVersionStringLength - 1;
    versionString << ")/" << version::getSwiftFullVersion();
    Metadata.emit(ScratchRecord,
                  SWIFTMODULE_VERSION_MAJOR, SWIFTMODULE_VERSION_MINOR,
                  shortVersionStringLength,
                  compatibilityVersionStringLength,
                  versionString.str());

    Target.emit(ScratchRecord, M->getASTContext().LangOpts.Target.str());

    {
      llvm::BCBlockRAII restoreBlock(Out, OPTIONS_BLOCK_ID, 4);

      options_block::IsSIBLayout IsSIB(Out);
      IsSIB.emit(ScratchRecord, options.IsSIB);

      if (M->isTestingEnabled()) {
        options_block::IsTestableLayout IsTestable(Out);
        IsTestable.emit(ScratchRecord);
      }

      if (M->arePrivateImportsEnabled()) {
        options_block::ArePrivateImportsEnabledLayout PrivateImports(Out);
        PrivateImports.emit(ScratchRecord);
      }

      if (M->getResilienceStrategy() != ResilienceStrategy::Default) {
        options_block::ResilienceStrategyLayout Strategy(Out);
        Strategy.emit(ScratchRecord, unsigned(M->getResilienceStrategy()));
      }

      if (options.SerializeOptionsForDebugging) {
        options_block::SDKPathLayout SDKPath(Out);
        options_block::XCCLayout XCC(Out);

        SDKPath.emit(ScratchRecord, M->getASTContext().SearchPathOpts.SDKPath);
        auto &Opts = options.ExtraClangOptions;
        for (auto Arg = Opts.begin(), E = Opts.end(); Arg != E; ++Arg) { 
          // FIXME: This is a hack and calls for a better design.
          //
          // Filter out any -ivfsoverlay options that include an
          // unextended-module-overlay.yaml overlay. By convention the Xcode
          // buildsystem uses these while *building* mixed Objective-C and Swift
          // frameworks; but they should never be used to *import* the module
          // defined in the framework.
          if (StringRef(*Arg).startswith("-ivfsoverlay")) {
            auto Next = std::next(Arg);
            if (Next != E &&
                StringRef(*Next).endswith("unextended-module-overlay.yaml")) {
              ++Arg;
              continue;
            }
          }
          XCC.emit(ScratchRecord, *Arg);
        }
      }
    }
  }
}

using ImportPathBlob = llvm::SmallString<64>;
static void flattenImportPath(const ModuleDecl::ImportedModule &import,
                              ImportPathBlob &out) {
  SmallVector<StringRef, 4> reverseSubmoduleNames(
      import.second->getReverseFullModuleName(), {});

  interleave(reverseSubmoduleNames.rbegin(), reverseSubmoduleNames.rend(),
             [&out](StringRef next) { out.append(next); },
             [&out] { out.push_back('\0'); });

  if (import.first.empty())
    return;

  out.push_back('\0');
  assert(import.first.size() == 1 && "can only handle top-level decl imports");
  auto accessPathElem = import.first.front();
  out.append(accessPathElem.first.str());
}

void Serializer::writeInputBlock(const SerializationOptions &options) {
  BCBlockRAII restoreBlock(Out, INPUT_BLOCK_ID, 4);
  input_block::ImportedModuleLayout ImportedModule(Out);
  input_block::LinkLibraryLayout LinkLibrary(Out);
  input_block::ImportedHeaderLayout ImportedHeader(Out);
  input_block::ImportedHeaderContentsLayout ImportedHeaderContents(Out);
  input_block::SearchPathLayout SearchPath(Out);
  input_block::FileDependencyLayout FileDependency(Out);

  if (options.SerializeOptionsForDebugging) {
    const SearchPathOptions &searchPathOpts = M->getASTContext().SearchPathOpts;
    // Put the framework search paths first so that they'll be preferred upon
    // deserialization.
    for (auto &framepath : searchPathOpts.FrameworkSearchPaths)
      SearchPath.emit(ScratchRecord, /*framework=*/true, framepath.IsSystem,
                      framepath.Path);
    for (auto &path : searchPathOpts.ImportSearchPaths)
      SearchPath.emit(ScratchRecord, /*framework=*/false, /*system=*/false, path);
  }

  for (auto const &dep : options.Dependencies) {
    FileDependency.emit(ScratchRecord, dep.Size, dep.Hash, dep.Path);
  }

  SmallVector<ModuleDecl::ImportedModule, 8> allImports;
  M->getImportedModules(allImports, ModuleDecl::ImportFilter::All);
  ModuleDecl::removeDuplicateImports(allImports);

  // Collect the public imports as a subset so that we can mark them with an
  // extra flag.
  SmallVector<ModuleDecl::ImportedModule, 8> publicImports;
  M->getImportedModules(publicImports, ModuleDecl::ImportFilter::Public);
  llvm::SmallSet<ModuleDecl::ImportedModule, 8,
                 ModuleDecl::OrderImportedModules> publicImportSet;
  publicImportSet.insert(publicImports.begin(), publicImports.end());

  auto clangImporter =
    static_cast<ClangImporter *>(M->getASTContext().getClangModuleLoader());
  ModuleDecl *bridgingHeaderModule = clangImporter->getImportedHeaderModule();
  ModuleDecl::ImportedModule bridgingHeaderImport{{}, bridgingHeaderModule};

  // Make sure the bridging header module is always at the top of the import
  // list, mimicking how it is processed before any module imports when
  // compiling source files.
  if (llvm::is_contained(allImports, bridgingHeaderImport)) {
    off_t importedHeaderSize = 0;
    time_t importedHeaderModTime = 0;
    std::string contents;
    if (!options.ImportedHeader.empty()) {
      contents = clangImporter->getBridgingHeaderContents(
          options.ImportedHeader, importedHeaderSize, importedHeaderModTime);
    }
    assert(publicImportSet.count(bridgingHeaderImport));
    ImportedHeader.emit(ScratchRecord,
                        publicImportSet.count(bridgingHeaderImport),
                        importedHeaderSize, importedHeaderModTime,
                        options.ImportedHeader);
    if (!contents.empty()) {
      contents.push_back('\0');
      ImportedHeaderContents.emit(ScratchRecord, contents);
    }
  }

  ModuleDecl *theBuiltinModule = M->getASTContext().TheBuiltinModule;
  for (auto import : allImports) {
    if (import.second == theBuiltinModule ||
        import.second == bridgingHeaderModule) {
      continue;
    }

    ImportPathBlob importPath;
    flattenImportPath(import, importPath);
    ImportedModule.emit(ScratchRecord, publicImportSet.count(import),
                        !import.first.empty(), importPath);
  }

  if (!options.ModuleLinkName.empty()) {
    LinkLibrary.emit(ScratchRecord, serialization::LibraryKind::Library,
                     options.AutolinkForceLoad, options.ModuleLinkName);
  }
}

/// Translate AST default argument kind to the Serialization enum values, which
/// are guaranteed to be stable.
static uint8_t getRawStableDefaultArgumentKind(swift::DefaultArgumentKind kind) {
  switch (kind) {
#define CASE(X) \
  case swift::DefaultArgumentKind::X: \
    return static_cast<uint8_t>(serialization::DefaultArgumentKind::X);
  CASE(None)
  CASE(Normal)
  CASE(Inherited)
  CASE(Column)
  CASE(File)
  CASE(Line)
  CASE(Function)
  CASE(DSOHandle)
  CASE(NilLiteral)
  CASE(EmptyArray)
  CASE(EmptyDictionary)
#undef CASE
  }

  llvm_unreachable("Unhandled DefaultArgumentKind in switch.");
}

static uint8_t getRawStableMetatypeRepresentation(AnyMetatypeType *metatype) {
  if (!metatype->hasRepresentation()) {
    return serialization::MetatypeRepresentation::MR_None;
  }

  switch (metatype->getRepresentation()) {
  case swift::MetatypeRepresentation::Thin:
    return serialization::MetatypeRepresentation::MR_Thin;
  case swift::MetatypeRepresentation::Thick:
    return serialization::MetatypeRepresentation::MR_Thick;
  case swift::MetatypeRepresentation::ObjC:
    return serialization::MetatypeRepresentation::MR_ObjC;
  }
  llvm_unreachable("bad representation");
}

static uint8_t getRawStableResilienceExpansion(swift::ResilienceExpansion e) {
  switch (e) {
  case swift::ResilienceExpansion::Minimal:
    return uint8_t(serialization::ResilienceExpansion::Minimal);
  case swift::ResilienceExpansion::Maximal:
    return uint8_t(serialization::ResilienceExpansion::Maximal);
  }
  llvm_unreachable("unhandled expansion");
}

void Serializer::writeParameterList(const ParameterList *PL) {
  using namespace decls_block;

  SmallVector<DeclID, 8> paramIDs;
  for (const ParamDecl *param : *PL)
    paramIDs.push_back(addDeclRef(param));

  unsigned abbrCode = DeclTypeAbbrCodes[ParameterListLayout::Code];
  ParameterListLayout::emitRecord(Out, ScratchRecord, abbrCode, paramIDs);
}


void Serializer::writePattern(const Pattern *pattern, DeclContext *owningDC) {
  using namespace decls_block;

  // Retrieve the type of the pattern.
  auto getPatternType = [&] {
    Type type = pattern->getType();

    // If we have an owning context and a contextual type, map out to an
    // interface type.
    if (owningDC && type->hasArchetype())
      type = type->mapTypeOutOfContext();

    return type;
  };

  assert(pattern && "null pattern");
  switch (pattern->getKind()) {
  case PatternKind::Paren: {
    unsigned abbrCode = DeclTypeAbbrCodes[ParenPatternLayout::Code];
    ParenPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   pattern->isImplicit());
    writePattern(cast<ParenPattern>(pattern)->getSubPattern(), owningDC);
    break;
  }
  case PatternKind::Tuple: {
    auto tuple = cast<TuplePattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[TuplePatternLayout::Code];
    TuplePatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addTypeRef(getPatternType()),
                                   tuple->getNumElements(),
                                   tuple->isImplicit());

    abbrCode = DeclTypeAbbrCodes[TuplePatternEltLayout::Code];
    for (auto &elt : tuple->getElements()) {
      // FIXME: Default argument expressions?
      TuplePatternEltLayout::emitRecord(
        Out, ScratchRecord, abbrCode, addDeclBaseNameRef(elt.getLabel()));
      writePattern(elt.getPattern(), owningDC);
    }
    break;
  }
  case PatternKind::Named: {
    auto named = cast<NamedPattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[NamedPatternLayout::Code];
    NamedPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addDeclRef(named->getDecl()),
                                   addTypeRef(getPatternType()),
                                   named->isImplicit());
    break;
  }
  case PatternKind::Any: {
    unsigned abbrCode = DeclTypeAbbrCodes[AnyPatternLayout::Code];
    AnyPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 addTypeRef(getPatternType()),
                                 pattern->isImplicit());
    break;
  }
  case PatternKind::Typed: {
    auto typed = cast<TypedPattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[TypedPatternLayout::Code];
    TypedPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addTypeRef(getPatternType()),
                                   typed->isImplicit());
    writePattern(typed->getSubPattern(), owningDC);
    break;
  }
  case PatternKind::Is:
  case PatternKind::EnumElement:
  case PatternKind::OptionalSome:
  case PatternKind::Bool:
  case PatternKind::Expr:
    llvm_unreachable("Refutable patterns cannot be serialized");

  case PatternKind::Var: {
    auto var = cast<VarPattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[VarPatternLayout::Code];
    VarPatternLayout::emitRecord(Out, ScratchRecord, abbrCode, var->isLet(),
                                 var->isImplicit());
    writePattern(var->getSubPattern(), owningDC);
    break;
  }
  }
}

/// Translate from the requirement kind to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t getRawStableRequirementKind(RequirementKind kind) {
#define CASE(KIND)            \
  case RequirementKind::KIND: \
    return GenericRequirementKind::KIND;

  switch (kind) {
  CASE(Conformance)
  CASE(Superclass)
  CASE(SameType)
  CASE(Layout)
  }
#undef CASE

  llvm_unreachable("Unhandled RequirementKind in switch.");
}

void Serializer::writeGenericRequirements(ArrayRef<Requirement> requirements,
                                          const std::array<unsigned, 256> &abbrCodes) {
  using namespace decls_block;

  if (requirements.empty())
    return;

  auto reqAbbrCode = abbrCodes[GenericRequirementLayout::Code];
  auto layoutReqAbbrCode = abbrCodes[LayoutRequirementLayout::Code];
  for (const auto &req : requirements) {
    if (req.getKind() != RequirementKind::Layout)
      GenericRequirementLayout::emitRecord(
          Out, ScratchRecord, reqAbbrCode,
          getRawStableRequirementKind(req.getKind()),
          addTypeRef(req.getFirstType()), addTypeRef(req.getSecondType()));
    else {
      // Write layout requirement.
      auto layout = req.getLayoutConstraint();
      unsigned size = 0;
      unsigned alignment = 0;
      if (layout->isKnownSizeTrivial()) {
        size = layout->getTrivialSizeInBits();
        alignment = layout->getAlignmentInBits();
      }
      LayoutRequirementKind rawKind = LayoutRequirementKind::UnknownLayout;
      switch (layout->getKind()) {
      case LayoutConstraintKind::NativeRefCountedObject:
        rawKind = LayoutRequirementKind::NativeRefCountedObject;
        break;
      case LayoutConstraintKind::RefCountedObject:
        rawKind = LayoutRequirementKind::RefCountedObject;
        break;
      case LayoutConstraintKind::Trivial:
        rawKind = LayoutRequirementKind::Trivial;
        break;
      case LayoutConstraintKind::TrivialOfExactSize:
        rawKind = LayoutRequirementKind::TrivialOfExactSize;
        break;
      case LayoutConstraintKind::TrivialOfAtMostSize:
        rawKind = LayoutRequirementKind::TrivialOfAtMostSize;
        break;
      case LayoutConstraintKind::Class:
        rawKind = LayoutRequirementKind::Class;
        break;
      case LayoutConstraintKind::NativeClass:
        rawKind = LayoutRequirementKind::NativeClass;
        break;
      case LayoutConstraintKind::UnknownLayout:
        rawKind = LayoutRequirementKind::UnknownLayout;
        break;
      }
      LayoutRequirementLayout::emitRecord(
          Out, ScratchRecord, layoutReqAbbrCode, rawKind,
          addTypeRef(req.getFirstType()), size, alignment);
    }
  }
}

void Serializer::writeInlinableBodyTextIfNeeded(
  const AbstractFunctionDecl *AFD) {
  using namespace decls_block;

  if (AFD->getResilienceExpansion() != swift::ResilienceExpansion::Minimal)
    return;
  if (!AFD->hasInlinableBodyText()) return;
  SmallString<128> scratch;
  auto body = AFD->getInlinableBodyText(scratch);

  unsigned abbrCode = DeclTypeAbbrCodes[InlinableBodyTextLayout::Code];
  InlinableBodyTextLayout::emitRecord(Out, ScratchRecord, abbrCode, body);
}

void Serializer::writeGenericParams(const GenericParamList *genericParams) {
  using namespace decls_block;

  // Don't write anything if there are no generic params.
  if (!genericParams)
    return;

  SmallVector<DeclID, 4> paramIDs;
  for (auto next : genericParams->getParams())
    paramIDs.push_back(addDeclRef(next));

  unsigned abbrCode = DeclTypeAbbrCodes[GenericParamListLayout::Code];
  GenericParamListLayout::emitRecord(Out, ScratchRecord, abbrCode, paramIDs);
}

void Serializer::writeGenericSignature(const GenericSignature *sig) {
  using namespace decls_block;

  // Record the offset of this generic environment.
  auto id = GenericSignatureIDs[sig];
  assert(id != 0 && "generic signature not referenced properly");
  (void)id;

  assert((id - 1) == GenericSignatureOffsets.size());
  GenericSignatureOffsets.push_back(Out.GetCurrentBitNo());

  assert(sig != nullptr);

  // Record the generic parameters.
  SmallVector<uint64_t, 4> rawParamIDs;
  for (auto *paramTy : sig->getGenericParams()) {
    rawParamIDs.push_back(addTypeRef(paramTy));
  }

  auto abbrCode = DeclTypeAbbrCodes[GenericSignatureLayout::Code];
  GenericSignatureLayout::emitRecord(Out, ScratchRecord, abbrCode, rawParamIDs);

  writeGenericRequirements(sig->getRequirements(), DeclTypeAbbrCodes);
}

void Serializer::writeGenericEnvironment(const GenericEnvironment *env) {
  using namespace decls_block;

  // Record the offset of this generic environment.
  auto id = GenericEnvironmentIDs[env];
  assert(id != 0 && "generic environment not referenced properly");
  (void)id;

  assert((id - 1) == GenericEnvironmentOffsets.size());
  assert(env != nullptr);

  // Determine whether we must use SIL mode, because one of the generic
  // parameters has a declaration with module context.
  bool SILMode = false;
  for (auto *paramTy : env->getGenericParams()) {
    if (auto *decl = paramTy->getDecl()) {
      if (decl->getDeclContext()->isModuleScopeContext()) {
        SILMode = true;
        break;
      }
    }
  }

  // If not in SIL mode, generic environments just contain a reference to
  // the generic signature.
  if (!SILMode) {
    // Record the generic signature directly.
    auto genericSigID = addGenericSignatureRef(env->getGenericSignature());
    GenericEnvironmentOffsets.push_back((genericSigID << 1) | 0x01);
    return;
  }

  // Record the current bit.
  GenericEnvironmentOffsets.push_back((Out.GetCurrentBitNo() << 1));

  // Record the generic parameters.
  SmallVector<uint64_t, 4> rawParamIDs;
  for (auto *paramTy : env->getGenericParams()) {
    auto *decl = paramTy->getDecl();

    // In SIL mode, add the name and canonicalize the parameter type.
    if (decl)
      rawParamIDs.push_back(addDeclBaseNameRef(decl->getName()));
    else
      rawParamIDs.push_back(addDeclBaseNameRef(Identifier()));

    paramTy = paramTy->getCanonicalType()->castTo<GenericTypeParamType>();
    rawParamIDs.push_back(addTypeRef(paramTy));
  }

  auto envAbbrCode = DeclTypeAbbrCodes[SILGenericEnvironmentLayout::Code];
  SILGenericEnvironmentLayout::emitRecord(Out, ScratchRecord, envAbbrCode,
                                          rawParamIDs);

  writeGenericRequirements(env->getGenericSignature()->getRequirements(),
                           DeclTypeAbbrCodes);
}

void Serializer::writeSubstitutionMap(const SubstitutionMap substitutions) {
  using namespace decls_block;
  assert(substitutions);

  // Record the offset of this substitution map.
  auto id = SubstitutionMapIDs[substitutions];
  assert(id != 0 && "generic environment not referenced properly");
  (void)id;

  // Record the current bit.
  assert((id - 1) == SubstitutionMapOffsets.size());
  SubstitutionMapOffsets.push_back(Out.GetCurrentBitNo());

  // Collect the replacement types.
  SmallVector<uint64_t, 4> rawReplacementTypes;
  for (auto type : substitutions.getReplacementTypes())
    rawReplacementTypes.push_back(addTypeRef(type));

  auto substitutionsAbbrCode = DeclTypeAbbrCodes[SubstitutionMapLayout::Code];
  SubstitutionMapLayout::emitRecord(Out, ScratchRecord, substitutionsAbbrCode,
                                    addGenericSignatureRef(
                                      substitutions.getGenericSignature()),
                                    substitutions.getConformances().size(),
                                    rawReplacementTypes);

  writeConformances(substitutions.getConformances(), DeclTypeAbbrCodes);
}

void Serializer::writeSILLayout(SILLayout *layout) {
  using namespace decls_block;
  auto foundLayoutID = SILLayouts.find(layout);
  assert(foundLayoutID != SILLayouts.end() && "layout not referenced properly");
  assert(foundLayoutID->second - 1 == SILLayoutOffsets.size());
  (void) foundLayoutID;
  SILLayoutOffsets.push_back(Out.GetCurrentBitNo());
  
  SmallVector<unsigned, 16> data;
  // Save field types.
  for (auto &field : layout->getFields()) {
    unsigned typeRef = addTypeRef(field.getLoweredType());
    // Set the high bit if mutable.
    if (field.isMutable())
      typeRef |= 0x80000000U;
    data.push_back(typeRef);
  }
  
  unsigned abbrCode
    = DeclTypeAbbrCodes[SILLayoutLayout::Code];

  SILLayoutLayout::emitRecord(
                        Out, ScratchRecord, abbrCode,
                        addGenericSignatureRef(layout->getGenericSignature()),
                        layout->getFields().size(),
                        data);
}

void Serializer::writeNormalConformance(
       const NormalProtocolConformance *conformance) {
  using namespace decls_block;

  // The conformance must be complete, or we can't serialize it.
  assert(conformance->isComplete());

  auto conformanceID = NormalConformances[conformance];
  assert(conformanceID != 0 && "normal conformance not referenced properly");
  (void)conformanceID;

  assert((conformanceID - 1) == NormalConformanceOffsets.size());
  NormalConformanceOffsets.push_back(Out.GetCurrentBitNo());

  auto protocol = conformance->getProtocol();

  SmallVector<DeclID, 32> data;
  unsigned numValueWitnesses = 0;
  unsigned numTypeWitnesses = 0;

  conformance->forEachTypeWitness(/*resolver=*/nullptr,
                                  [&](AssociatedTypeDecl *assocType,
                                      Type type, TypeDecl *typeDecl) {
    data.push_back(addDeclRef(assocType));
    data.push_back(addTypeRef(type));
    data.push_back(addDeclRef(typeDecl, /*allowTypeAliasXRef*/true));
    ++numTypeWitnesses;
    return false;
  });

  conformance->forEachValueWitness(nullptr,
    [&](ValueDecl *req, Witness witness) {
      ++numValueWitnesses;
      data.push_back(addDeclRef(req));
      data.push_back(addDeclRef(witness.getDecl()));
      assert(witness.getDecl() || req->getAttrs().hasAttribute<OptionalAttr>()
             || req->getAttrs().isUnavailable(req->getASTContext()));

      // If there is no witness, we're done.
      if (!witness.getDecl()) return;

      auto subs = witness.getSubstitutions();

      // Canonicalize away typealiases, since these substitutions aren't used
      // for diagnostics and we reference fewer declarations that way.
      subs = subs.getCanonical();

      // Map archetypes to type parameters, since we always substitute them
      // away. Note that in a merge-modules pass, we're serializing conformances
      // that we deserialized, so they will already have their replacement types
      // in terms of interface types; hence the hasArchetypes() check is
      // necessary for correctness, not just as a fast path.
      if (subs.hasArchetypes())
        subs = subs.mapReplacementTypesOutOfContext();

      data.push_back(addSubstitutionMapRef(subs));
  });

  unsigned numSignatureConformances =
      conformance->getSignatureConformances().size();

  unsigned abbrCode
    = DeclTypeAbbrCodes[NormalProtocolConformanceLayout::Code];
  auto ownerID = addDeclContextRef(conformance->getDeclContext());
  NormalProtocolConformanceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                              addDeclRef(protocol), ownerID,
                                              numTypeWitnesses,
                                              numValueWitnesses,
                                              numSignatureConformances,
                                              data);

  // Write requirement signature conformances.
  for (auto reqConformance : conformance->getSignatureConformances())
    writeConformance(reqConformance, DeclTypeAbbrCodes);
}

void
Serializer::writeConformance(ProtocolConformance *conformance,
                             const std::array<unsigned, 256> &abbrCodes,
                             GenericEnvironment *genericEnv) {
  writeConformance(ProtocolConformanceRef(conformance), abbrCodes, genericEnv);
}

void
Serializer::writeConformance(ProtocolConformanceRef conformanceRef,
                             const std::array<unsigned, 256> &abbrCodes,
                             GenericEnvironment *genericEnv) {
  using namespace decls_block;

  if (conformanceRef.isInvalid()) {
    unsigned abbrCode = abbrCodes[InvalidProtocolConformanceLayout::Code];
    InvalidProtocolConformanceLayout::emitRecord(Out, ScratchRecord, abbrCode);
    return;
  }

  if (conformanceRef.isAbstract()) {
    unsigned abbrCode = abbrCodes[AbstractProtocolConformanceLayout::Code];
    AbstractProtocolConformanceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      addDeclRef(conformanceRef.getAbstract()));
    return;
  }

  auto conformance = conformanceRef.getConcrete();
  switch (conformance->getKind()) {
  case ProtocolConformanceKind::Normal: {
    auto normal = cast<NormalProtocolConformance>(conformance);
    if (!isDeclXRef(getDeclForContext(normal->getDeclContext()))
        && !isa<ClangModuleUnit>(normal->getDeclContext()
                                       ->getModuleScopeContext())) {
      // A normal conformance in this module file.
      unsigned abbrCode = abbrCodes[NormalProtocolConformanceIdLayout::Code];
      NormalProtocolConformanceIdLayout::emitRecord(Out, ScratchRecord,
                                                    abbrCode,
                                                    addConformanceRef(normal));
    } else {
      // A conformance in a different module file.
      unsigned abbrCode = abbrCodes[ProtocolConformanceXrefLayout::Code];
      ProtocolConformanceXrefLayout::emitRecord(
        Out, ScratchRecord,
        abbrCode,
        addDeclRef(normal->getProtocol()),
        addDeclRef(normal->getType()->getAnyNominal()),
        addModuleRef(normal->getDeclContext()->getParentModule()));
    }
    break;
  }

  case ProtocolConformanceKind::Specialized: {
    auto conf = cast<SpecializedProtocolConformance>(conformance);
    unsigned abbrCode = abbrCodes[SpecializedProtocolConformanceLayout::Code];
    auto type = conf->getType();
    if (genericEnv && type->hasArchetype())
      type = type->mapTypeOutOfContext();
    SpecializedProtocolConformanceLayout::emitRecord(
                           Out, ScratchRecord,
                           abbrCode,
                           addTypeRef(type),
                           addSubstitutionMapRef(conf->getSubstitutionMap()));

    writeConformance(conf->getGenericConformance(), abbrCodes, genericEnv);
    break;
  }

  case ProtocolConformanceKind::Inherited: {
    auto conf = cast<InheritedProtocolConformance>(conformance);
    unsigned abbrCode
      = abbrCodes[InheritedProtocolConformanceLayout::Code];

    auto type = conf->getType();
    if (genericEnv && type->hasArchetype())
      type = type->mapTypeOutOfContext();

    InheritedProtocolConformanceLayout::emitRecord(
      Out, ScratchRecord, abbrCode, addTypeRef(type));

    writeConformance(conf->getInheritedConformance(), abbrCodes, genericEnv);
    break;
  }
  }
}

void
Serializer::writeConformances(ArrayRef<ProtocolConformanceRef> conformances,
                              const std::array<unsigned, 256> &abbrCodes) {
  using namespace decls_block;

  for (auto conformance : conformances)
    writeConformance(conformance, abbrCodes);
}

void
Serializer::writeConformances(ArrayRef<ProtocolConformance*> conformances,
                              const std::array<unsigned, 256> &abbrCodes) {
  using namespace decls_block;

  for (auto conformance : conformances)
    writeConformance(conformance, abbrCodes);
}

static uint8_t getRawStableOptionalTypeKind(swift::OptionalTypeKind kind) {
  switch (kind) {
  case swift::OTK_None:
    return static_cast<uint8_t>(serialization::OptionalTypeKind::None);
  case swift::OTK_Optional:
    return static_cast<uint8_t>(serialization::OptionalTypeKind::Optional);
  case swift::OTK_ImplicitlyUnwrappedOptional:
    return static_cast<uint8_t>(
             serialization::OptionalTypeKind::ImplicitlyUnwrappedOptional);
  }

  llvm_unreachable("Unhandled OptionalTypeKind in switch.");
}

static bool shouldSerializeMember(Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Import:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::TopLevelCode:
  case DeclKind::Extension:
  case DeclKind::Module:
  case DeclKind::PrecedenceGroup:
    llvm_unreachable("decl should never be a member");

  case DeclKind::MissingMember:
    llvm_unreachable("should never need to reserialize a member placeholder");

  case DeclKind::IfConfig:
  case DeclKind::PoundDiagnostic:
    return false;

  case DeclKind::EnumCase:
    return false;

  case DeclKind::EnumElement:
  case DeclKind::Protocol:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
  case DeclKind::PatternBinding:
  case DeclKind::Subscript:
  case DeclKind::TypeAlias:
  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType:
  case DeclKind::Enum:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Var:
  case DeclKind::Param:
  case DeclKind::Func:
  case DeclKind::Accessor:
    return true;
  }

  llvm_unreachable("Unhandled DeclKind in switch.");
}

void Serializer::writeMembers(DeclID parentID,
                              DeclRange members, bool isClass) {
  using namespace decls_block;

  unsigned abbrCode = DeclTypeAbbrCodes[MembersLayout::Code];
  SmallVector<DeclID, 16> memberIDs;
  for (auto member : members) {
    if (!shouldSerializeMember(member))
      continue;

    DeclID memberID = addDeclRef(member);
    memberIDs.push_back(memberID);

    if (auto VD = dyn_cast<ValueDecl>(member)) {

      // Record parent->members in subtable of DeclMemberNames
      if (VD->hasName() &&
          !VD->getBaseName().empty()) {
        std::unique_ptr<DeclMembersTable> &memberTable =
          DeclMemberNames[VD->getBaseName()].second;
        if (!memberTable) {
          memberTable = llvm::make_unique<DeclMembersTable>();
        }
        (*memberTable)[parentID].push_back(memberID);
      }

      // Same as above, but for @_implements attributes
      if (auto A = VD->getAttrs().getAttribute<ImplementsAttr>()) {
        std::unique_ptr<DeclMembersTable> &memberTable =
          DeclMemberNames[A->getMemberName().getBaseName()].second;
        if (!memberTable) {
          memberTable = llvm::make_unique<DeclMembersTable>();
        }
        (*memberTable)[parentID].push_back(memberID);
      }

      // Possibly add a record to ClassMembersForDynamicLookup too.
      if (isClass) {
        if (VD->canBeAccessedByDynamicLookup()) {
          auto &list = ClassMembersForDynamicLookup[VD->getBaseName()];
          list.push_back({getKindForTable(VD), memberID});
        }
      }
    }
  }
  MembersLayout::emitRecord(Out, ScratchRecord, abbrCode, memberIDs);
}

void Serializer::writeDefaultWitnessTable(const ProtocolDecl *proto,
                                   const std::array<unsigned, 256> &abbrCodes) {
  using namespace decls_block;

  SmallVector<DeclID, 16> witnessIDs;

  unsigned abbrCode = abbrCodes[DefaultWitnessTableLayout::Code];
  for (auto member : proto->getMembers()) {
    if (auto *value = dyn_cast<ValueDecl>(member)) {
      auto witness = proto->getDefaultWitness(value);
      if (!witness)
        continue;

      DeclID requirementID = addDeclRef(value);
      DeclID witnessID = addDeclRef(witness.getDecl());
      witnessIDs.push_back(requirementID);
      witnessIDs.push_back(witnessID);

      // FIXME: Substitutions
    }
  }
  DefaultWitnessTableLayout::emitRecord(Out, ScratchRecord,
                                        abbrCode, witnessIDs);
}

static serialization::AccessorKind getStableAccessorKind(swift::AccessorKind K){
  switch (K) {
#define ACCESSOR(ID) \
  case swift::AccessorKind::ID: return serialization::ID;
#include "swift/AST/AccessorKinds.def"
  }

  llvm_unreachable("Unhandled AccessorKind in switch.");
}

static serialization::CtorInitializerKind
getStableCtorInitializerKind(swift::CtorInitializerKind K){
  switch (K) {
#define CASE(NAME) \
  case swift::CtorInitializerKind::NAME: return serialization::NAME;
      CASE(Designated)
      CASE(Convenience)
      CASE(Factory)
      CASE(ConvenienceFactory)
#undef CASE
  }

  llvm_unreachable("Unhandled CtorInitializerKind in switch.");
}

void Serializer::writeCrossReference(const DeclContext *DC, uint32_t pathLen) {
  using namespace decls_block;

  unsigned abbrCode;

  switch (DC->getContextKind()) {
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::Initializer:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::SerializedLocal:
    llvm_unreachable("cannot cross-reference this context");

  case DeclContextKind::FileUnit:
    DC = cast<FileUnit>(DC)->getParentModule();
    LLVM_FALLTHROUGH;

  case DeclContextKind::Module:
    abbrCode = DeclTypeAbbrCodes[XRefLayout::Code];
    XRefLayout::emitRecord(Out, ScratchRecord, abbrCode,
                           addModuleRef(cast<ModuleDecl>(DC)), pathLen);
    break;

  case DeclContextKind::GenericTypeDecl: {
    writeCrossReference(DC->getParent(), pathLen + 1);

    auto generic = cast<GenericTypeDecl>(DC);
    abbrCode = DeclTypeAbbrCodes[XRefTypePathPieceLayout::Code];

    Identifier discriminator;
    if (generic->isOutermostPrivateOrFilePrivateScope()) {
      auto *containingFile = cast<FileUnit>(generic->getModuleScopeContext());
      discriminator = containingFile->getDiscriminatorForPrivateValue(generic);
    }

    XRefTypePathPieceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                        addDeclBaseNameRef(generic->getName()),
                                        addDeclBaseNameRef(discriminator),
                                        /*inProtocolExtension*/false,
                                        generic->hasClangNode());
    break;
  }

  case DeclContextKind::ExtensionDecl: {
    auto ext = cast<ExtensionDecl>(DC);
    auto nominal = ext->getExtendedNominal();
    assert(nominal);
    writeCrossReference(nominal, pathLen + 1);

    abbrCode = DeclTypeAbbrCodes[XRefExtensionPathPieceLayout::Code];
    CanGenericSignature genericSig(nullptr);
    if (ext->isConstrainedExtension()) {
      genericSig = ext->getGenericSignature()->getCanonicalSignature();
    }
    XRefExtensionPathPieceLayout::emitRecord(
        Out, ScratchRecord, abbrCode, addModuleRef(DC->getParentModule()),
        addGenericSignatureRef(genericSig));
    break;
  }

  case DeclContextKind::SubscriptDecl: {
    auto SD = cast<SubscriptDecl>(DC);
    writeCrossReference(DC->getParent(), pathLen + 1);
    
    Type ty = SD->getInterfaceType()->getCanonicalType();

    abbrCode = DeclTypeAbbrCodes[XRefValuePathPieceLayout::Code];
    bool isProtocolExt = SD->getDeclContext()->getExtendedProtocolDecl();
    XRefValuePathPieceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                         addTypeRef(ty), SUBSCRIPT_ID,
                                         isProtocolExt, SD->hasClangNode(),
                                         SD->isStatic());
    break;
  }
      
  case DeclContextKind::AbstractFunctionDecl: {
    if (auto fn = dyn_cast<AccessorDecl>(DC)) {
      auto storage = fn->getStorage();
      writeCrossReference(storage->getDeclContext(), pathLen + 2);

      Type ty = storage->getInterfaceType()->getCanonicalType();
      IdentifierID nameID = addDeclBaseNameRef(storage->getBaseName());
      bool isProtocolExt = fn->getDeclContext()->getExtendedProtocolDecl();
      abbrCode = DeclTypeAbbrCodes[XRefValuePathPieceLayout::Code];
      XRefValuePathPieceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                           addTypeRef(ty), nameID,
                                           isProtocolExt,
                                           storage->hasClangNode(),
                                           storage->isStatic());

      abbrCode =
        DeclTypeAbbrCodes[XRefOperatorOrAccessorPathPieceLayout::Code];
      auto emptyID = addDeclBaseNameRef(Identifier());
      auto accessorKind = getStableAccessorKind(fn->getAccessorKind());
      assert(!fn->isObservingAccessor() &&
             "cannot form cross-reference to observing accessors");
      XRefOperatorOrAccessorPathPieceLayout::emitRecord(Out, ScratchRecord,
                                                        abbrCode, emptyID,
                                                        accessorKind);
      break;
    }

    auto fn = cast<AbstractFunctionDecl>(DC);
    writeCrossReference(DC->getParent(), pathLen + 1 + fn->isOperator());

    Type ty = fn->getInterfaceType()->getCanonicalType();

    if (auto ctor = dyn_cast<ConstructorDecl>(DC)) {
      abbrCode = DeclTypeAbbrCodes[XRefInitializerPathPieceLayout::Code];
      XRefInitializerPathPieceLayout::emitRecord(
        Out, ScratchRecord, abbrCode, addTypeRef(ty),
        (bool)ctor->getDeclContext()->getExtendedProtocolDecl(),
        ctor->hasClangNode(),
        getStableCtorInitializerKind(ctor->getInitKind()));
      break;
    }

    abbrCode = DeclTypeAbbrCodes[XRefValuePathPieceLayout::Code];
    bool isProtocolExt = fn->getDeclContext()->getExtendedProtocolDecl();
    XRefValuePathPieceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                         addTypeRef(ty),
                                         addDeclBaseNameRef(fn->getBaseName()),
                                         isProtocolExt, fn->hasClangNode(),
                                         fn->isStatic());

    if (fn->isOperator()) {
      // Encode the fixity as a filter on the func decls, to distinguish prefix
      // and postfix operators.
      auto op = cast<FuncDecl>(fn)->getOperatorDecl();
      assert(op);
      abbrCode = DeclTypeAbbrCodes[XRefOperatorOrAccessorPathPieceLayout::Code];
      auto emptyID = addDeclBaseNameRef(Identifier());
      auto fixity = getStableFixity(op->getKind());
      XRefOperatorOrAccessorPathPieceLayout::emitRecord(Out, ScratchRecord,
                                                        abbrCode, emptyID,
                                                        fixity);
    }
    break;
  }
  }
}

void Serializer::writeCrossReference(const Decl *D) {
  using namespace decls_block;

  unsigned abbrCode;

  if (auto op = dyn_cast<OperatorDecl>(D)) {
    writeCrossReference(op->getModuleContext(), 1);

    abbrCode = DeclTypeAbbrCodes[XRefOperatorOrAccessorPathPieceLayout::Code];
    auto nameID = addDeclBaseNameRef(op->getName());
    auto fixity = getStableFixity(op->getKind());
    XRefOperatorOrAccessorPathPieceLayout::emitRecord(Out, ScratchRecord,
                                                      abbrCode, nameID,
                                                      fixity);
    return;
  }

  if (auto prec = dyn_cast<PrecedenceGroupDecl>(D)) {
    writeCrossReference(prec->getModuleContext(), 1);

    abbrCode = DeclTypeAbbrCodes[XRefOperatorOrAccessorPathPieceLayout::Code];
    auto nameID = addDeclBaseNameRef(prec->getName());
    uint8_t fixity = OperatorKind::PrecedenceGroup;
    XRefOperatorOrAccessorPathPieceLayout::emitRecord(Out, ScratchRecord,
                                                      abbrCode, nameID,
                                                      fixity);
    return;
  }

  if (auto fn = dyn_cast<AbstractFunctionDecl>(D)) {
    // Functions are special because they might be operators.
    writeCrossReference(fn, 0);
    return;
  }

  writeCrossReference(D->getDeclContext());

  if (auto genericParam = dyn_cast<GenericTypeParamDecl>(D)) {
    assert(!D->getDeclContext()->isModuleScopeContext() &&
           "Cannot cross reference a generic type decl at module scope.");
    abbrCode = DeclTypeAbbrCodes[XRefGenericParamPathPieceLayout::Code];
    XRefGenericParamPathPieceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                                genericParam->getDepth(),
                                                genericParam->getIndex());
    return;
  }

  bool isProtocolExt = D->getDeclContext()->getExtendedProtocolDecl();
  if (auto type = dyn_cast<TypeDecl>(D)) {
    abbrCode = DeclTypeAbbrCodes[XRefTypePathPieceLayout::Code];

    Identifier discriminator;
    if (type->isOutermostPrivateOrFilePrivateScope()) {
      auto *containingFile =
         cast<FileUnit>(type->getDeclContext()->getModuleScopeContext());
      discriminator = containingFile->getDiscriminatorForPrivateValue(type);
    }

    XRefTypePathPieceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                        addDeclBaseNameRef(type->getName()),
                                        addDeclBaseNameRef(discriminator),
                                        isProtocolExt, D->hasClangNode());
    return;
  }

  auto val = cast<ValueDecl>(D);
  auto ty = val->getInterfaceType()->getCanonicalType();
  abbrCode = DeclTypeAbbrCodes[XRefValuePathPieceLayout::Code];
  IdentifierID iid = addDeclBaseNameRef(val->getBaseName());
  XRefValuePathPieceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                       addTypeRef(ty), iid, isProtocolExt,
                                       D->hasClangNode(), val->isStatic());
}

/// Translate from the AST associativity enum to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t getRawStableAssociativity(swift::Associativity assoc) {
  switch (assoc) {
  case swift::Associativity::Left:
    return serialization::Associativity::LeftAssociative;
  case swift::Associativity::Right:
    return serialization::Associativity::RightAssociative;
  case swift::Associativity::None:
    return serialization::Associativity::NonAssociative;
  }

  llvm_unreachable("Unhandled Associativity in switch.");
}

static serialization::StaticSpellingKind
getStableStaticSpelling(swift::StaticSpellingKind SS) {
  switch (SS) {
  case swift::StaticSpellingKind::None:
    return serialization::StaticSpellingKind::None;
  case swift::StaticSpellingKind::KeywordStatic:
    return serialization::StaticSpellingKind::KeywordStatic;
  case swift::StaticSpellingKind::KeywordClass:
    return serialization::StaticSpellingKind::KeywordClass;
  }

  llvm_unreachable("Unhandled StaticSpellingKind in switch.");
}

static uint8_t getRawStableAccessLevel(swift::AccessLevel access) {
  switch (access) {
#define CASE(NAME) \
  case swift::AccessLevel::NAME: \
    return static_cast<uint8_t>(serialization::AccessLevel::NAME);
  CASE(Private)
  CASE(FilePrivate)
  CASE(Internal)
  CASE(Public)
  CASE(Open)
#undef CASE
  }

  llvm_unreachable("Unhandled AccessLevel in switch.");
}

static serialization::SelfAccessKind
getStableSelfAccessKind(swift::SelfAccessKind MM) {
  switch (MM) {
  case swift::SelfAccessKind::NonMutating:
    return serialization::SelfAccessKind::NonMutating;
  case swift::SelfAccessKind::Mutating:
    return serialization::SelfAccessKind::Mutating;
  case swift::SelfAccessKind::__Consuming:
    return serialization::SelfAccessKind::__Consuming;
  }

  llvm_unreachable("Unhandled StaticSpellingKind in switch.");
}

#ifndef NDEBUG
#define DEF_VERIFY_ATTR(DECL)\
static void verifyAttrSerializable(const DECL ## Decl *D) {\
  for (auto Attr : D->getAttrs()) {\
    assert(Attr->canAppearOnDecl(D) && "attribute cannot appear on a " #DECL);\
}\
}

DEF_VERIFY_ATTR(Func)
DEF_VERIFY_ATTR(Extension)
DEF_VERIFY_ATTR(PatternBinding)
DEF_VERIFY_ATTR(Operator)
DEF_VERIFY_ATTR(PrecedenceGroup)
DEF_VERIFY_ATTR(TypeAlias)
DEF_VERIFY_ATTR(Type)
DEF_VERIFY_ATTR(Struct)
DEF_VERIFY_ATTR(Enum)
DEF_VERIFY_ATTR(Class)
DEF_VERIFY_ATTR(Protocol)
DEF_VERIFY_ATTR(Var)
DEF_VERIFY_ATTR(Subscript)
DEF_VERIFY_ATTR(Constructor)
DEF_VERIFY_ATTR(Destructor)

#undef DEF_VERIFY_ATTR
#else
static void verifyAttrSerializable(const Decl *D) {}
#endif

static inline unsigned getOptionalOrZero(const llvm::Optional<unsigned> &X) {
  if (X.hasValue())
    return X.getValue();
  return 0;
}

void Serializer::writeDeclAttribute(const DeclAttribute *DA) {
  using namespace decls_block;

  // Completely ignore attributes that aren't serialized.
  if (DA->isNotSerialized())
    return;

  // Ignore attributes that have been marked invalid. (This usually means
  // type-checking removed them, but only provided a warning rather than an
  // error.)
  if (DA->isInvalid())
    return;

  switch (DA->getKind()) {
  case DAK_RawDocComment:
  case DAK_ReferenceOwnership: // Serialized as part of the type.
  case DAK_AccessControl:
  case DAK_SetterAccess:
  case DAK_ObjCBridged:
  case DAK_SynthesizedProtocol:
  case DAK_Implements:
  case DAK_ObjCRuntimeName:
  case DAK_RestatedObjCConformance:
  case DAK_ClangImporterSynthesizedType:
  case DAK_PrivateImport:
    llvm_unreachable("cannot serialize attribute");

  case DAK_Count:
    llvm_unreachable("not a real attribute");

#define SIMPLE_DECL_ATTR(_, CLASS, ...)\
  case DAK_##CLASS: { \
    auto abbrCode = DeclTypeAbbrCodes[CLASS##DeclAttrLayout::Code]; \
    CLASS##DeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode, \
                                      DA->isImplicit()); \
    return; \
  }
#include "swift/AST/Attr.def"

  case DAK_SILGenName: {
    auto *theAttr = cast<SILGenNameAttr>(DA);
    auto abbrCode = DeclTypeAbbrCodes[SILGenNameDeclAttrLayout::Code];
    SILGenNameDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      theAttr->isImplicit(),
                                      theAttr->Name);
    return;
  }

  case DAK_CDecl: {
    auto *theAttr = cast<CDeclAttr>(DA);
    auto abbrCode = DeclTypeAbbrCodes[CDeclDeclAttrLayout::Code];
    CDeclDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                    theAttr->isImplicit(),
                                    theAttr->Name);
    return;
  }

  case DAK_Alignment: {
    auto *theAlignment = cast<AlignmentAttr>(DA);
    auto abbrCode = DeclTypeAbbrCodes[AlignmentDeclAttrLayout::Code];
    AlignmentDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                        theAlignment->isImplicit(),
                                        theAlignment->getValue());
    return;
  }
  
  case DAK_SwiftNativeObjCRuntimeBase: {
    auto *theBase = cast<SwiftNativeObjCRuntimeBaseAttr>(DA);
    auto abbrCode
      = DeclTypeAbbrCodes[SwiftNativeObjCRuntimeBaseDeclAttrLayout::Code];
    auto nameID = addDeclBaseNameRef(theBase->BaseClassName);
    
    SwiftNativeObjCRuntimeBaseDeclAttrLayout::emitRecord(Out, ScratchRecord,
                                                     abbrCode,
                                                     theBase->isImplicit(),
                                                     nameID);
    return;
  }
  
  case DAK_Semantics: {
    auto *theAttr = cast<SemanticsAttr>(DA);
    auto abbrCode = DeclTypeAbbrCodes[SemanticsDeclAttrLayout::Code];
    SemanticsDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      theAttr->isImplicit(),
                                      theAttr->Value);
    return;
  }

  case DAK_Inline: {
    auto *theAttr = cast<InlineAttr>(DA);
    auto abbrCode = DeclTypeAbbrCodes[InlineDeclAttrLayout::Code];
    InlineDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     (unsigned)theAttr->getKind());
    return;
  }

  case DAK_Optimize: {
    auto *theAttr = cast<OptimizeAttr>(DA);
    auto abbrCode = DeclTypeAbbrCodes[OptimizeDeclAttrLayout::Code];
    OptimizeDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                       (unsigned)theAttr->getMode());
    return;
  }

  case DAK_Effects: {
    auto *theAttr = cast<EffectsAttr>(DA);
    auto abbrCode = DeclTypeAbbrCodes[EffectsDeclAttrLayout::Code];
    EffectsDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     (unsigned)theAttr->getKind());
    return;
  }

  case DAK_Available: {
#define LIST_VER_TUPLE_PIECES(X)\
  X##_Major, X##_Minor, X##_Subminor, X##_HasMinor, X##_HasSubminor
#define DEF_VER_TUPLE_PIECES(X, X_Expr)\
  unsigned X##_Major = 0, X##_Minor = 0, X##_Subminor = 0,\
           X##_HasMinor = 0, X##_HasSubminor = 0;\
  const auto &X##_Val = X_Expr;\
  if (X##_Val.hasValue()) {\
    const auto &Y = X##_Val.getValue();\
    X##_Major = Y.getMajor();\
    X##_Minor = getOptionalOrZero(Y.getMinor());\
    X##_Subminor = getOptionalOrZero(Y.getSubminor());\
    X##_HasMinor = Y.getMinor().hasValue();\
    X##_HasSubminor = Y.getSubminor().hasValue();\
  }

    auto *theAttr = cast<AvailableAttr>(DA);
    DEF_VER_TUPLE_PIECES(Introduced, theAttr->Introduced)
    DEF_VER_TUPLE_PIECES(Deprecated, theAttr->Deprecated)
    DEF_VER_TUPLE_PIECES(Obsoleted, theAttr->Obsoleted)

    llvm::SmallString<32> blob;
    blob.append(theAttr->Message);
    blob.append(theAttr->Rename);
    auto abbrCode = DeclTypeAbbrCodes[AvailableDeclAttrLayout::Code];
    AvailableDeclAttrLayout::emitRecord(
        Out, ScratchRecord, abbrCode,
        theAttr->isImplicit(),
        theAttr->isUnconditionallyUnavailable(),
        theAttr->isUnconditionallyDeprecated(),
        LIST_VER_TUPLE_PIECES(Introduced),
        LIST_VER_TUPLE_PIECES(Deprecated),
        LIST_VER_TUPLE_PIECES(Obsoleted),
        static_cast<unsigned>(theAttr->Platform),
        theAttr->Message.size(),
        theAttr->Rename.size(),
        blob);
    return;
#undef LIST_VER_TUPLE_PIECES
#undef DEF_VER_TUPLE_PIECES
  }

  case DAK_ObjC: {
    auto *theAttr = cast<ObjCAttr>(DA);
    SmallVector<IdentifierID, 4> pieces;
    unsigned numArgs = 0;
    if (auto name = theAttr->getName()) {
      numArgs = name->getNumArgs() + 1;
      for (auto piece : name->getSelectorPieces()) {
        pieces.push_back(addDeclBaseNameRef(piece));
      }
    }
    auto abbrCode = DeclTypeAbbrCodes[ObjCDeclAttrLayout::Code];
    ObjCDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   theAttr->isImplicit(),
                                   theAttr->isSwift3Inferred(),
                                   theAttr->isNameImplicit(), numArgs, pieces);
    return;
  }

  case DAK_Specialize: {
    auto abbrCode = DeclTypeAbbrCodes[SpecializeDeclAttrLayout::Code];
    auto SA = cast<SpecializeAttr>(DA);

    SpecializeDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                         (unsigned)SA->isExported(),
                                         (unsigned)SA->getSpecializationKind());
    writeGenericRequirements(SA->getRequirements(), DeclTypeAbbrCodes);
    return;
  }

  case DAK_DynamicReplacement: {
    auto abbrCode = DeclTypeAbbrCodes[DynamicReplacementDeclAttrLayout::Code];
    auto theAttr = cast<DynamicReplacementAttr>(DA);
    auto replacedFun = theAttr->getReplacedFunctionName();
    SmallVector<IdentifierID, 4> pieces;
    pieces.push_back(addDeclBaseNameRef(replacedFun.getBaseName()));
    for (auto argName : replacedFun.getArgumentNames())
      pieces.push_back(addDeclBaseNameRef(argName));
    assert(theAttr->getReplacedFunction());
    DynamicReplacementDeclAttrLayout::emitRecord(
        Out, ScratchRecord, abbrCode, false, /*implicit flag*/
        addDeclRef(theAttr->getReplacedFunction()), pieces.size(), pieces);
    return;
  }
  }
}

bool Serializer::isDeclXRef(const Decl *D) const {
  const DeclContext *topLevel = D->getDeclContext()->getModuleScopeContext();
  if (topLevel->getParentModule() != M)
    return true;
  if (!SF || topLevel == SF)
    return false;
  // Special-case for SIL generic parameter decls, which don't have a real
  // DeclContext.
  if (!isa<FileUnit>(topLevel)) {
    assert(isa<GenericTypeParamDecl>(D) && "unexpected decl kind");
    return false;
  }
  return true;
}

void Serializer::writeDeclContext(const DeclContext *DC) {
  using namespace decls_block;
  auto isDecl = false;
  auto id = DeclContextIDs[DC];
  assert(id != 0 && "decl context not referenced properly");
  (void)id;

  assert((id - 1) == DeclContextOffsets.size());
  DeclContextOffsets.push_back(Out.GetCurrentBitNo());

  auto abbrCode = DeclTypeAbbrCodes[DeclContextLayout::Code];
  DeclContextID declOrDeclContextID = 0;

  switch (DC->getContextKind()) {
  case DeclContextKind::AbstractFunctionDecl:
  case DeclContextKind::SubscriptDecl:
  case DeclContextKind::GenericTypeDecl:
  case DeclContextKind::ExtensionDecl:
    declOrDeclContextID = addDeclRef(getDeclForContext(DC));
    isDecl = true;
    break;

  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::Initializer:
  case DeclContextKind::SerializedLocal:
    declOrDeclContextID = addLocalDeclContextRef(DC);
    break;
  case DeclContextKind::Module:
    llvm_unreachable("References to the module are serialized implicitly");
  case DeclContextKind::FileUnit:
    llvm_unreachable("Can't serialize a FileUnit");
  }

  DeclContextLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                declOrDeclContextID, isDecl);
}

void Serializer::writePatternBindingInitializer(PatternBindingDecl *binding,
                                                unsigned bindingIndex) {
  using namespace decls_block;
  auto abbrCode = DeclTypeAbbrCodes[PatternBindingInitializerLayout::Code];

  StringRef initStr;
  SmallString<128> scratch;
  auto &entry = binding->getPatternList()[bindingIndex];
  auto varDecl = entry.getAnchoringVarDecl();
  if (entry.hasInitStringRepresentation() &&
      varDecl->isInitExposedToClients()) {
    initStr = entry.getInitStringRepresentation(scratch);
  }

  PatternBindingInitializerLayout::emitRecord(Out, ScratchRecord,
                                              abbrCode, addDeclRef(binding),
                                              bindingIndex, initStr);
}

void
Serializer::writeDefaultArgumentInitializer(const DeclContext *parentContext,
                                            unsigned index) {
  using namespace decls_block;
  auto abbrCode = DeclTypeAbbrCodes[DefaultArgumentInitializerLayout::Code];
  DefaultArgumentInitializerLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                               addDeclContextRef(parentContext),
                                               index);
}

void Serializer::writeAbstractClosureExpr(const DeclContext *parentContext,
                                          Type Ty, bool isImplicit,
                                          unsigned discriminator) {
  using namespace decls_block;
  auto abbrCode = DeclTypeAbbrCodes[AbstractClosureExprLayout::Code];
  AbstractClosureExprLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                        addTypeRef(Ty), isImplicit,
                                        discriminator,
                                        addDeclContextRef(parentContext));
}

void Serializer::writeLocalDeclContext(const DeclContext *DC) {
  using namespace decls_block;

  assert(shouldSerializeAsLocalContext(DC) &&
         "Can't serialize as local context");

  auto id = LocalDeclContextIDs[DC];
  assert(id != 0 && "decl context not referenced properly");
  (void)id;

  assert((id - 1)== LocalDeclContextOffsets.size());
  LocalDeclContextOffsets.push_back(Out.GetCurrentBitNo());

  switch (DC->getContextKind()) {
  case DeclContextKind::AbstractClosureExpr: {
    auto ACE = cast<AbstractClosureExpr>(DC);
    writeAbstractClosureExpr(ACE->getParent(), ACE->getType(),
                             ACE->isImplicit(), ACE->getDiscriminator());
    break;
  }

  case DeclContextKind::Initializer: {
    if (auto PBI = dyn_cast<PatternBindingInitializer>(DC)) {
      writePatternBindingInitializer(PBI->getBinding(), PBI->getBindingIndex());
    } else if (auto DAI = dyn_cast<DefaultArgumentInitializer>(DC)) {
      writeDefaultArgumentInitializer(DAI->getParent(), DAI->getIndex());
    }
    break;
  }

  case DeclContextKind::TopLevelCodeDecl: {
    auto abbrCode = DeclTypeAbbrCodes[TopLevelCodeDeclContextLayout::Code];
    TopLevelCodeDeclContextLayout::emitRecord(Out, ScratchRecord, abbrCode,
      addDeclContextRef(DC->getParent()));
    break;
  }

  // If we are merging already serialized modules with local decl contexts,
  // we handle them here in a similar fashion.
  case DeclContextKind::SerializedLocal: {
    auto local = cast<SerializedLocalDeclContext>(DC);
    switch (local->getLocalDeclContextKind()) {
    case LocalDeclContextKind::AbstractClosure: {
      auto SACE = cast<SerializedAbstractClosureExpr>(local);
      writeAbstractClosureExpr(SACE->getParent(), SACE->getType(),
                               SACE->isImplicit(), SACE->getDiscriminator());
      return;
    }
    case LocalDeclContextKind::DefaultArgumentInitializer: {
      auto DAI = cast<SerializedDefaultArgumentInitializer>(local);
      writeDefaultArgumentInitializer(DAI->getParent(), DAI->getIndex());
      return;
    }
    case LocalDeclContextKind::PatternBindingInitializer: {
      auto PBI = cast<SerializedPatternBindingInitializer>(local);
      writePatternBindingInitializer(PBI->getBinding(), PBI->getBindingIndex());
      return;
    }
    case LocalDeclContextKind::TopLevelCodeDecl: {
      auto abbrCode = DeclTypeAbbrCodes[TopLevelCodeDeclContextLayout::Code];
      TopLevelCodeDeclContextLayout::emitRecord(Out, ScratchRecord,
        abbrCode, addDeclContextRef(DC->getParent()));
      return;
    }
    }
  }

  default:
    llvm_unreachable("Trying to write a DeclContext that isn't local");
  }
}

static ForeignErrorConventionKind getRawStableForeignErrorConventionKind(
                                    ForeignErrorConvention::Kind kind) {
  switch (kind) {
  case ForeignErrorConvention::ZeroResult:
    return ForeignErrorConventionKind::ZeroResult;
  case ForeignErrorConvention::NonZeroResult:
    return ForeignErrorConventionKind::NonZeroResult;
  case ForeignErrorConvention::ZeroPreservedResult:
    return ForeignErrorConventionKind::ZeroPreservedResult;
  case ForeignErrorConvention::NilResult:
    return ForeignErrorConventionKind::NilResult;
  case ForeignErrorConvention::NonNilError:
    return ForeignErrorConventionKind::NonNilError;
  }

  llvm_unreachable("Unhandled ForeignErrorConvention in switch.");
}

/// Translate from the AST VarDeclSpecifier enum to the
/// Serialization enum values, which are guaranteed to be stable.
static uint8_t getRawStableVarDeclSpecifier(swift::VarDecl::Specifier sf) {
  switch (sf) {
  case swift::VarDecl::Specifier::Let:
    return uint8_t(serialization::VarDeclSpecifier::Let);
  case swift::VarDecl::Specifier::Var:
    return uint8_t(serialization::VarDeclSpecifier::Var);
  case swift::VarDecl::Specifier::InOut:
    return uint8_t(serialization::VarDeclSpecifier::InOut);
  case swift::VarDecl::Specifier::Shared:
    return uint8_t(serialization::VarDeclSpecifier::Shared);
  case swift::VarDecl::Specifier::Owned:
    return uint8_t(serialization::VarDeclSpecifier::Owned);
  }
  llvm_unreachable("bad variable decl specifier kind");
}

void Serializer::writeForeignErrorConvention(const ForeignErrorConvention &fec){
  using namespace decls_block;

  auto kind = getRawStableForeignErrorConventionKind(fec.getKind());
  uint8_t isOwned = fec.isErrorOwned() == ForeignErrorConvention::IsOwned;
  uint8_t isReplaced = bool(fec.isErrorParameterReplacedWithVoid());
  TypeID errorParameterTypeID = addTypeRef(fec.getErrorParameterType());
  TypeID resultTypeID;
  switch (fec.getKind()) {
  case ForeignErrorConvention::ZeroResult:
  case ForeignErrorConvention::NonZeroResult:
    resultTypeID = addTypeRef(fec.getResultType());
    break;

  case ForeignErrorConvention::ZeroPreservedResult:
  case ForeignErrorConvention::NilResult:
  case ForeignErrorConvention::NonNilError:
    resultTypeID = 0;
    break;
  }

  auto abbrCode = DeclTypeAbbrCodes[ForeignErrorConventionLayout::Code];
  ForeignErrorConventionLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                           static_cast<uint8_t>(kind),
                                           isOwned,
                                           isReplaced,
                                           fec.getErrorParameterIndex(),
                                           errorParameterTypeID,
                                           resultTypeID);
}

/// Returns true if the declaration of \p decl depends on \p problemContext
/// based on lexical nesting.
///
/// - \p decl is \p problemContext
/// - \p decl is declared within \p problemContext
/// - \p decl is declared in an extension of a type that depends on
///   \p problemContext
static bool contextDependsOn(const NominalTypeDecl *decl,
                             const ModuleDecl *problemModule) {
  return decl->getParentModule() == problemModule;
}

static void collectDependenciesFromType(llvm::SmallSetVector<Type, 4> &seen,
                                        Type ty,
                                        const ModuleDecl *excluding) {
  ty.visit([&](Type next) {
    auto *nominal = next->getAnyNominal();
    if (!nominal)
      return;
    // FIXME: Types in the same module are still important for enums. It's
    // possible an enum element has a payload that references a type declaration
    // from the same module that can't be imported (for whatever reason).
    // However, we need a more robust handling of deserialization dependencies
    // that can handle circularities. rdar://problem/32359173
    if (contextDependsOn(nominal, excluding))
      return;
    seen.insert(nominal->getDeclaredInterfaceType());
  });
}

static void
collectDependenciesFromRequirement(llvm::SmallSetVector<Type, 4> &seen,
                                   const Requirement &req,
                                   const ModuleDecl *excluding) {
  collectDependenciesFromType(seen, req.getFirstType(), excluding);
  if (req.getKind() != RequirementKind::Layout)
    collectDependenciesFromType(seen, req.getSecondType(), excluding);
}

static SmallVector<Type, 4> collectDependenciesFromType(Type ty) {
  llvm::SmallSetVector<Type, 4> result;
  collectDependenciesFromType(result, ty, /*excluding*/nullptr);
  return result.takeVector();
}

void Serializer::writeDecl(const Decl *D) {
  using namespace decls_block;

  PrettyStackTraceDecl trace("serializing", D);

  auto id = DeclAndTypeIDs[D];
  assert(id != 0 && "decl or type not referenced properly");
  (void)id;

  assert((id - 1) == DeclOffsets.size());
  DeclOffsets.push_back(Out.GetCurrentBitNo());

  assert(!D->isInvalid() && "cannot create a module with an invalid decl");
  if (isDeclXRef(D)) {
    writeCrossReference(D);
    return;
  }

  assert(!D->hasClangNode() && "imported decls should use cross-references");

  // Emit attributes (if any).
  auto &Attrs = D->getAttrs();
  if (Attrs.begin() != Attrs.end()) {
    for (auto Attr : Attrs)
      writeDeclAttribute(Attr);
  }

  if (auto *value = dyn_cast<ValueDecl>(D)) {
    if (value->getFormalAccess() <= swift::AccessLevel::FilePrivate &&
        !value->getDeclContext()->isLocalContext()) {
      auto topLevelContext = value->getDeclContext()->getModuleScopeContext();
      if (auto *enclosingFile = dyn_cast<FileUnit>(topLevelContext)) {
        // FIXME: We shouldn't need to encode this for /all/ private decls.
        // In theory we can follow the same rules as mangling and only include
        // the outermost private context.
        Identifier discriminator =
          enclosingFile->getDiscriminatorForPrivateValue(value);
        unsigned abbrCode =
          DeclTypeAbbrCodes[PrivateDiscriminatorLayout::Code];
        PrivateDiscriminatorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                        addDeclBaseNameRef(discriminator));
        auto getFilename = [](FileUnit *enclosingFile,
                              const ValueDecl *decl) -> StringRef {
          if (auto *SF = dyn_cast<SourceFile>(enclosingFile)) {
            return llvm::sys::path::filename(SF->getFilename());
          } else if (auto *LF = dyn_cast<LoadedFile>(enclosingFile)) {
            return LF->getFilenameForPrivateDecl(decl);
          }
          return StringRef();
        };
        // Only if compiled with -enable-private-imports.
        if (M->arePrivateImportsEnabled()) {
          auto filename = getFilename(enclosingFile, value);
          if (!filename.empty()) {
            auto filenameID = addFilename(filename);
            FilenameForPrivateLayout::emitRecord(
                Out, ScratchRecord,
                DeclTypeAbbrCodes[FilenameForPrivateLayout::Code], filenameID);
          }
        }
      }
    }

    if (value->getDeclContext()->isLocalContext()) {
      auto discriminator = value->getLocalDiscriminator();
      auto abbrCode = DeclTypeAbbrCodes[LocalDiscriminatorLayout::Code];
      LocalDiscriminatorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                           discriminator);
    }
  }

  switch (D->getKind()) {
  case DeclKind::Import:
    llvm_unreachable("import decls should not be serialized");

  case DeclKind::IfConfig:
    llvm_unreachable("#if block declarations should not be serialized");

  case DeclKind::PoundDiagnostic:
    llvm_unreachable("#warning/#error declarations should not be serialized");

  case DeclKind::Extension: {
    auto extension = cast<ExtensionDecl>(D);
    verifyAttrSerializable(extension);

    auto contextID = addDeclContextRef(extension->getDeclContext());
    Type baseTy = extension->getExtendedType();
    assert(!baseTy->hasUnboundGenericType());
    assert(!baseTy->hasArchetype());

    // FIXME: Use the canonical type here in order to minimize circularity
    // issues at deserialization time. A known problematic case here is
    // "extension of typealias Foo"; "typealias Foo = SomeKit.Bar"; and then
    // trying to import Bar accidentally asking for all of its extensions
    // (perhaps because we're searching for a conformance).
    //
    // We could limit this to only the problematic cases, but it seems like a
    // simpler user model to just always desugar extension types.
    baseTy = baseTy->getCanonicalType();

    // Make sure the base type has registered itself as a provider of generic
    // parameters.
    auto baseNominal = baseTy->getAnyNominal();
    (void)addDeclRef(baseNominal);

    auto conformances = extension->getLocalConformances(
                          ConformanceLookupKind::All,
                          nullptr, /*sorted=*/true);

    SmallVector<TypeID, 8> inheritedAndDependencyTypes;
    for (auto inherited : extension->getInherited()) {
      assert(!inherited.getType()->hasArchetype());
      inheritedAndDependencyTypes.push_back(addTypeRef(inherited.getType()));
    }
    size_t numInherited = inheritedAndDependencyTypes.size();

    llvm::SmallSetVector<Type, 4> dependencies;
    collectDependenciesFromType(dependencies, baseTy, /*excluding*/nullptr);
    for (Requirement req : extension->getGenericRequirements()) {
      collectDependenciesFromRequirement(dependencies, req,
                                         /*excluding*/nullptr);
    }
    for (auto dependencyTy : dependencies)
      inheritedAndDependencyTypes.push_back(addTypeRef(dependencyTy));

    unsigned abbrCode = DeclTypeAbbrCodes[ExtensionLayout::Code];
    ExtensionLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addTypeRef(baseTy),
                                contextID,
                                extension->isImplicit(),
                                addGenericEnvironmentRef(
                                           extension->getGenericEnvironment()),
                                conformances.size(),
                                numInherited,
                                inheritedAndDependencyTypes);

    bool isClassExtension = false;
    if (baseNominal) {
      isClassExtension = isa<ClassDecl>(baseNominal) ||
                         isa<ProtocolDecl>(baseNominal);
    }

    // Extensions of nested generic types have multiple generic parameter
    // lists. Collect them all, from the innermost to outermost.
    SmallVector<GenericParamList *, 2> allGenericParams;
    for (auto *genericParams = extension->getGenericParams();
         genericParams != nullptr;
         genericParams = genericParams->getOuterParameters()) {
      allGenericParams.push_back(genericParams);
    }

    // Reverse the list, and write the parameter lists, from outermost
    // to innermost.
    std::reverse(allGenericParams.begin(), allGenericParams.end());
    for (auto *genericParams : allGenericParams)
      writeGenericParams(genericParams);

    writeMembers(id, extension->getMembers(), isClassExtension);
    writeConformances(conformances, DeclTypeAbbrCodes);
    break;
  }

  case DeclKind::EnumCase:
    llvm_unreachable("enum case decls should not be serialized");

  case DeclKind::PatternBinding: {
    auto binding = cast<PatternBindingDecl>(D);
    verifyAttrSerializable(binding);

    auto contextID = addDeclContextRef(binding->getDeclContext());
    SmallVector<uint64_t, 2> initContextIDs;
    for (unsigned i : range(binding->getNumPatternEntries())) {
      auto initContextID =
        addDeclContextRef(binding->getPatternList()[i].getInitContext());
      if (!initContextIDs.empty()) {
        initContextIDs.push_back(initContextID);
      } else if (initContextID) {
        initContextIDs.append(i, 0);
        initContextIDs.push_back(initContextID);
      }
    }

    unsigned abbrCode = DeclTypeAbbrCodes[PatternBindingLayout::Code];
    PatternBindingLayout::emitRecord(
        Out, ScratchRecord, abbrCode, contextID, binding->isImplicit(),
        binding->isStatic(),
        uint8_t(getStableStaticSpelling(binding->getStaticSpelling())),
                                     binding->getNumPatternEntries(),
        initContextIDs);

    DeclContext *owningDC = nullptr;
    if (binding->getDeclContext()->isTypeContext())
      owningDC = binding->getDeclContext();

    for (auto entry : binding->getPatternList()) {
      writePattern(entry.getPattern(), owningDC);
      // Ignore initializer; external clients don't need to know about it.
    }

    break;
  }

  case DeclKind::TopLevelCode:
    // Top-level code is ignored; external clients don't need to know about it.
    break;

  case DeclKind::PrecedenceGroup: {
    auto group = cast<PrecedenceGroupDecl>(D);
    verifyAttrSerializable(group);

    auto contextID = addDeclContextRef(group->getDeclContext());
    auto nameID = addDeclBaseNameRef(group->getName());
    auto associativity = getRawStableAssociativity(group->getAssociativity());

    SmallVector<DeclID, 8> relations;
    for (auto &rel : group->getHigherThan())
      relations.push_back(addDeclRef(rel.Group));
    for (auto &rel : group->getLowerThan())
      relations.push_back(addDeclRef(rel.Group));

    unsigned abbrCode = DeclTypeAbbrCodes[PrecedenceGroupLayout::Code];
    PrecedenceGroupLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      nameID, contextID, associativity,
                                      group->isAssignment(),
                                      group->getHigherThan().size(),
                                      relations);
    break;
  }

  case DeclKind::MissingMember:
    llvm_unreachable("member placeholders shouldn't be serialized");

  case DeclKind::InfixOperator: {
    auto op = cast<InfixOperatorDecl>(D);
    verifyAttrSerializable(op);

    auto contextID = addDeclContextRef(op->getDeclContext());
    auto nameID = addDeclBaseNameRef(op->getName());
    auto groupID = addDeclRef(op->getPrecedenceGroup());
    SmallVector<DeclID, 1> designatedNominalTypeDeclIDs;
    for (auto *decl : op->getDesignatedNominalTypes())
      designatedNominalTypeDeclIDs.push_back(addDeclRef(decl));

    unsigned abbrCode = DeclTypeAbbrCodes[InfixOperatorLayout::Code];
    InfixOperatorLayout::emitRecord(Out, ScratchRecord, abbrCode, nameID,
                                    contextID, groupID,
                                    designatedNominalTypeDeclIDs);
    break;
  }

  case DeclKind::PrefixOperator: {
    auto op = cast<PrefixOperatorDecl>(D);
    verifyAttrSerializable(op);

    auto contextID = addDeclContextRef(op->getDeclContext());
    SmallVector<DeclID, 1> designatedNominalTypeDeclIDs;
    for (auto *decl : op->getDesignatedNominalTypes())
      designatedNominalTypeDeclIDs.push_back(addDeclRef(decl));

    unsigned abbrCode = DeclTypeAbbrCodes[PrefixOperatorLayout::Code];
    PrefixOperatorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addDeclBaseNameRef(op->getName()),
                                     contextID, designatedNominalTypeDeclIDs);
    break;
  }

  case DeclKind::PostfixOperator: {
    auto op = cast<PostfixOperatorDecl>(D);
    verifyAttrSerializable(op);

    auto contextID = addDeclContextRef(op->getDeclContext());
    SmallVector<DeclID, 1> designatedNominalTypeDeclIDs;
    for (auto *decl : op->getDesignatedNominalTypes())
      designatedNominalTypeDeclIDs.push_back(addDeclRef(decl));

    unsigned abbrCode = DeclTypeAbbrCodes[PostfixOperatorLayout::Code];
    PostfixOperatorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      addDeclBaseNameRef(op->getName()),
                                      contextID, designatedNominalTypeDeclIDs);
    break;
  }

  case DeclKind::TypeAlias: {
    auto typeAlias = cast<TypeAliasDecl>(D);
    assert(!typeAlias->isObjC() && "ObjC typealias is not meaningful");
    verifyAttrSerializable(typeAlias);

    auto contextID = addDeclContextRef(typeAlias->getDeclContext());

    auto underlying = typeAlias->getUnderlyingTypeLoc().getType();

    llvm::SmallSetVector<Type, 4> dependencies;
    collectDependenciesFromType(dependencies, underlying->getCanonicalType(),
                                /*excluding*/nullptr);
    for (Requirement req : typeAlias->getGenericRequirements()) {
      collectDependenciesFromRequirement(dependencies, req,
                                         /*excluding*/nullptr);
    }

    SmallVector<TypeID, 4> dependencyIDs;
    for (Type dep : dependencies)
      dependencyIDs.push_back(addTypeRef(dep));

    uint8_t rawAccessLevel =
      getRawStableAccessLevel(typeAlias->getFormalAccess());

    unsigned abbrCode = DeclTypeAbbrCodes[TypeAliasLayout::Code];
    TypeAliasLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addDeclBaseNameRef(typeAlias->getName()),
                                contextID,
                                addTypeRef(underlying),
                                /*no longer used*/TypeID(),
                                typeAlias->isImplicit(),
                                addGenericEnvironmentRef(
                                             typeAlias->getGenericEnvironment()),
                                rawAccessLevel,
                                dependencyIDs);
    writeGenericParams(typeAlias->getGenericParams());
    break;
  }

  case DeclKind::GenericTypeParam: {
    auto genericParam = cast<GenericTypeParamDecl>(D);
    verifyAttrSerializable(genericParam);

    unsigned abbrCode = DeclTypeAbbrCodes[GenericTypeParamDeclLayout::Code];
    GenericTypeParamDeclLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addDeclBaseNameRef(genericParam->getName()),
                                genericParam->isImplicit(),
                                genericParam->getDepth(),
                                genericParam->getIndex());
    break;
  }

  case DeclKind::AssociatedType: {
    auto assocType = cast<AssociatedTypeDecl>(D);
    verifyAttrSerializable(assocType);

    auto contextID = addDeclContextRef(assocType->getDeclContext());
    SmallVector<DeclID, 4> overriddenAssocTypeIDs;
    for (auto overridden : assocType->getOverriddenDecls()) {
      overriddenAssocTypeIDs.push_back(addDeclRef(overridden));
    }

    unsigned abbrCode = DeclTypeAbbrCodes[AssociatedTypeDeclLayout::Code];
    AssociatedTypeDeclLayout::emitRecord(
      Out, ScratchRecord, abbrCode,
      addDeclBaseNameRef(assocType->getName()),
      contextID,
      addTypeRef(assocType->getDefaultDefinitionType()),
      assocType->isImplicit(),
      overriddenAssocTypeIDs);
    break;
  }

  case DeclKind::Struct: {
    auto theStruct = cast<StructDecl>(D);
    verifyAttrSerializable(theStruct);

    auto contextID = addDeclContextRef(theStruct->getDeclContext());

    auto conformances = theStruct->getLocalConformances(
                          ConformanceLookupKind::All,
                          nullptr, /*sorted=*/true);

    SmallVector<TypeID, 4> inheritedTypes;
    for (auto inherited : theStruct->getInherited()) {
      assert(!inherited.getType()->hasArchetype());
      inheritedTypes.push_back(addTypeRef(inherited.getType()));
    }

    uint8_t rawAccessLevel =
      getRawStableAccessLevel(theStruct->getFormalAccess());

    unsigned abbrCode = DeclTypeAbbrCodes[StructLayout::Code];
    StructLayout::emitRecord(Out, ScratchRecord, abbrCode,
                             addDeclBaseNameRef(theStruct->getName()),
                             contextID,
                             theStruct->isImplicit(),
                             theStruct->isObjC(),
                             addGenericEnvironmentRef(
                                            theStruct->getGenericEnvironment()),
                             rawAccessLevel,
                             conformances.size(),
                             inheritedTypes);


    writeGenericParams(theStruct->getGenericParams());
    writeMembers(id, theStruct->getMembers(), false);
    writeConformances(conformances, DeclTypeAbbrCodes);
    break;
  }

  case DeclKind::Enum: {
    auto theEnum = cast<EnumDecl>(D);
    verifyAttrSerializable(theEnum);

    auto contextID = addDeclContextRef(theEnum->getDeclContext());

    auto conformances = theEnum->getLocalConformances(
                          ConformanceLookupKind::All,
                          nullptr, /*sorted=*/true);

    SmallVector<TypeID, 4> inheritedAndDependencyTypes;
    for (auto inherited : theEnum->getInherited()) {
      assert(!inherited.getType()->hasArchetype());
      inheritedAndDependencyTypes.push_back(addTypeRef(inherited.getType()));
    }

    llvm::SmallSetVector<Type, 4> dependencyTypes;
    for (const EnumElementDecl *nextElt : theEnum->getAllElements()) {
      if (!nextElt->hasAssociatedValues())
        continue;
      collectDependenciesFromType(dependencyTypes,
                                  nextElt->getArgumentInterfaceType(),
                                  /*excluding*/theEnum->getParentModule());
    }
    for (Requirement req : theEnum->getGenericRequirements()) {
      collectDependenciesFromRequirement(dependencyTypes, req,
                                         /*excluding*/nullptr);
    }
    for (Type ty : dependencyTypes)
      inheritedAndDependencyTypes.push_back(addTypeRef(ty));

    uint8_t rawAccessLevel =
      getRawStableAccessLevel(theEnum->getFormalAccess());

    unsigned abbrCode = DeclTypeAbbrCodes[EnumLayout::Code];
    EnumLayout::emitRecord(Out, ScratchRecord, abbrCode,
                            addDeclBaseNameRef(theEnum->getName()),
                            contextID,
                            theEnum->isImplicit(),
                            theEnum->isObjC(),
                            addGenericEnvironmentRef(
                                             theEnum->getGenericEnvironment()),
                            addTypeRef(theEnum->getRawType()),
                            rawAccessLevel,
                            conformances.size(),
                            theEnum->getInherited().size(),
                            inheritedAndDependencyTypes);

    writeGenericParams(theEnum->getGenericParams());
    writeMembers(id, theEnum->getMembers(), false);
    writeConformances(conformances, DeclTypeAbbrCodes);
    break;
  }

  case DeclKind::Class: {
    auto theClass = cast<ClassDecl>(D);
    verifyAttrSerializable(theClass);
    assert(!theClass->isForeign());

    auto contextID = addDeclContextRef(theClass->getDeclContext());

    auto conformances = theClass->getLocalConformances(
                          ConformanceLookupKind::All,
                          nullptr, /*sorted=*/true);

    SmallVector<TypeID, 4> inheritedTypes;
    for (auto inherited : theClass->getInherited()) {
      assert(!inherited.getType()->hasArchetype());
      inheritedTypes.push_back(addTypeRef(inherited.getType()));
    }

    uint8_t rawAccessLevel =
      getRawStableAccessLevel(theClass->getFormalAccess());

    bool inheritsSuperclassInitializers =
        const_cast<ClassDecl *>(theClass)->
          inheritsSuperclassInitializers(nullptr);

    unsigned abbrCode = DeclTypeAbbrCodes[ClassLayout::Code];
    ClassLayout::emitRecord(Out, ScratchRecord, abbrCode,
                            addDeclBaseNameRef(theClass->getName()),
                            contextID,
                            theClass->isImplicit(),
                            theClass->isObjC(),
                            theClass->requiresStoredPropertyInits(),
                            inheritsSuperclassInitializers,
                            addGenericEnvironmentRef(
                                             theClass->getGenericEnvironment()),
                            addTypeRef(theClass->getSuperclass()),
                            rawAccessLevel,
                            conformances.size(),
                            inheritedTypes);

    writeGenericParams(theClass->getGenericParams());
    writeMembers(id, theClass->getMembers(), true);
    writeConformances(conformances, DeclTypeAbbrCodes);
    break;
  }


  case DeclKind::Protocol: {
    auto proto = cast<ProtocolDecl>(D);
    verifyAttrSerializable(proto);

    auto contextID = addDeclContextRef(proto->getDeclContext());

    SmallVector<DeclID, 8> inherited;
    for (auto element : proto->getInherited()) {
      assert(!element.getType()->hasArchetype());
      inherited.push_back(addTypeRef(element.getType()));
    }

    uint8_t rawAccessLevel = getRawStableAccessLevel(proto->getFormalAccess());

    unsigned abbrCode = DeclTypeAbbrCodes[ProtocolLayout::Code];
    ProtocolLayout::emitRecord(Out, ScratchRecord, abbrCode,
                               addDeclBaseNameRef(proto->getName()),
                               contextID,
                               proto->isImplicit(),
                               const_cast<ProtocolDecl *>(proto)
                                 ->requiresClass(),
                               proto->isObjC(),
                               proto->existentialTypeSupported(
                                 /*resolver=*/nullptr),
                               addGenericEnvironmentRef(
                                                proto->getGenericEnvironment()),
                               addTypeRef(proto->getSuperclass()),
                               rawAccessLevel,
                               inherited);

    writeGenericParams(proto->getGenericParams());
    writeGenericRequirements(
      proto->getRequirementSignature(), DeclTypeAbbrCodes);
    writeMembers(id, proto->getMembers(), true);
    writeDefaultWitnessTable(proto, DeclTypeAbbrCodes);
    break;
  }

  case DeclKind::Var: {
    auto var = cast<VarDecl>(D);
    verifyAttrSerializable(var);

    auto contextID = addDeclContextRef(var->getDeclContext());
    
    Accessors accessors = getAccessors(var);
    uint8_t rawAccessLevel = getRawStableAccessLevel(var->getFormalAccess());
    uint8_t rawSetterAccessLevel = rawAccessLevel;
    if (var->isSettable(nullptr))
      rawSetterAccessLevel =
        getRawStableAccessLevel(var->getSetterFormalAccess());

    Type ty = var->getInterfaceType();
    SmallVector<TypeID, 2> accessorsAndDependencies;
    for (auto accessor : accessors.Decls)
      accessorsAndDependencies.push_back(addDeclRef(accessor));
    for (Type dependency : collectDependenciesFromType(ty->getCanonicalType()))
      accessorsAndDependencies.push_back(addTypeRef(dependency));

    unsigned abbrCode = DeclTypeAbbrCodes[VarLayout::Code];
    VarLayout::emitRecord(Out, ScratchRecord, abbrCode,
                          addDeclBaseNameRef(var->getName()),
                          contextID,
                          var->isImplicit(),
                          var->isObjC(),
                          var->isStatic(),
                          getRawStableVarDeclSpecifier(var->getSpecifier()),
                          var->hasNonPatternBindingInit(),
                          var->isGetterMutating(),
                          var->isSetterMutating(),
                          accessors.OpaqueReadOwnership,
                          accessors.ReadImpl,
                          accessors.WriteImpl,
                          accessors.ReadWriteImpl,
                          accessors.Decls.size(),
                          addTypeRef(ty),
                          addDeclRef(var->getOverriddenDecl()),
                          rawAccessLevel, rawSetterAccessLevel,
                          accessorsAndDependencies);
    break;
  }

  case DeclKind::Param: {
    auto param = cast<ParamDecl>(D);
    verifyAttrSerializable(param);

    auto contextID = addDeclContextRef(param->getDeclContext());
    Type interfaceType = param->getInterfaceType();

    // Only save the text for normal default arguments, not any of the special
    // ones.
    StringRef defaultArgumentText;
    SmallString<128> scratch;
    if (param->getDefaultArgumentKind() == swift::DefaultArgumentKind::Normal)
      defaultArgumentText =
        param->getDefaultValueStringRepresentation(scratch);

    unsigned abbrCode = DeclTypeAbbrCodes[ParamLayout::Code];
    ParamLayout::emitRecord(Out, ScratchRecord, abbrCode,
        addDeclBaseNameRef(param->getArgumentName()),
        addDeclBaseNameRef(param->getName()),
        contextID,
        getRawStableVarDeclSpecifier(param->getSpecifier()),
        addTypeRef(interfaceType),
        param->isVariadic(),
        param->isAutoClosure(),
        getRawStableDefaultArgumentKind(param->getDefaultArgumentKind()),
        defaultArgumentText);

    if (interfaceType->hasError()) {
      param->getDeclContext()->dumpContext();
      interfaceType->dump();
      llvm_unreachable("error in interface type of parameter");
    }
    break;
  }

  case DeclKind::Func: {
    auto fn = cast<FuncDecl>(D);
    verifyAttrSerializable(fn);

    auto contextID = addDeclContextRef(fn->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[FuncLayout::Code];
    SmallVector<IdentifierID, 4> nameComponentsAndDependencies;
    nameComponentsAndDependencies.push_back(
        addDeclBaseNameRef(fn->getFullName().getBaseName()));
    for (auto argName : fn->getFullName().getArgumentNames())
      nameComponentsAndDependencies.push_back(addDeclBaseNameRef(argName));

    uint8_t rawAccessLevel = getRawStableAccessLevel(fn->getFormalAccess());
    uint8_t rawDefaultArgumentResilienceExpansion =
      getRawStableResilienceExpansion(
          fn->getDefaultArgumentResilienceExpansion());

    Type ty = fn->getInterfaceType();
    for (auto dependency : collectDependenciesFromType(ty->getCanonicalType()))
      nameComponentsAndDependencies.push_back(addTypeRef(dependency));

    FuncLayout::emitRecord(Out, ScratchRecord, abbrCode,
                           contextID,
                           fn->isImplicit(),
                           fn->isStatic(),
                           uint8_t(
                             getStableStaticSpelling(fn->getStaticSpelling())),
                           fn->isObjC(),
                           uint8_t(
                             getStableSelfAccessKind(fn->getSelfAccessKind())),
                           fn->hasDynamicSelf(),
                           fn->hasForcedStaticDispatch(),
                           fn->hasThrows(),
                           addGenericEnvironmentRef(
                                                  fn->getGenericEnvironment()),
                           addTypeRef(fn->getResultInterfaceType()),
                           addDeclRef(fn->getOperatorDecl()),
                           addDeclRef(fn->getOverriddenDecl()),
                           fn->getFullName().getArgumentNames().size() +
                             fn->getFullName().isCompoundName(),
                           rawAccessLevel,
                           fn->needsNewVTableEntry(),
                           rawDefaultArgumentResilienceExpansion,
                           nameComponentsAndDependencies);

    writeGenericParams(fn->getGenericParams());

    // Write the body parameters.
    writeParameterList(fn->getParameters());

    if (auto errorConvention = fn->getForeignErrorConvention())
      writeForeignErrorConvention(*errorConvention);

    writeInlinableBodyTextIfNeeded(fn);

    break;
  }

  case DeclKind::Accessor: {
    auto fn = cast<AccessorDecl>(D);
    verifyAttrSerializable(fn);

    auto contextID = addDeclContextRef(fn->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[AccessorLayout::Code];

    uint8_t rawAccessLevel = getRawStableAccessLevel(fn->getFormalAccess());
    uint8_t rawAccessorKind =
      uint8_t(getStableAccessorKind(fn->getAccessorKind()));
    uint8_t rawDefaultArgumentResilienceExpansion =
      getRawStableResilienceExpansion(
          fn->getDefaultArgumentResilienceExpansion());

    Type ty = fn->getInterfaceType();
    SmallVector<IdentifierID, 4> dependencies;
    for (auto dependency : collectDependenciesFromType(ty->getCanonicalType()))
      dependencies.push_back(addTypeRef(dependency));

    AccessorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                               contextID,
                               fn->isImplicit(),
                               fn->isStatic(),
                               uint8_t(getStableStaticSpelling(
                                                  fn->getStaticSpelling())),
                               fn->isObjC(),
                               uint8_t(getStableSelfAccessKind(
                                                  fn->getSelfAccessKind())),
                               fn->hasDynamicSelf(),
                               fn->hasForcedStaticDispatch(),
                               fn->hasThrows(),
                               addGenericEnvironmentRef(
                                                  fn->getGenericEnvironment()),
                               addTypeRef(fn->getResultInterfaceType()),
                               addDeclRef(fn->getOverriddenDecl()),
                               addDeclRef(fn->getStorage()),
                               rawAccessorKind,
                               rawAccessLevel,
                               fn->needsNewVTableEntry(),
                               rawDefaultArgumentResilienceExpansion,
                               dependencies);

    writeGenericParams(fn->getGenericParams());

    // Write the body parameters.
    writeParameterList(fn->getParameters());

    if (auto errorConvention = fn->getForeignErrorConvention())
      writeForeignErrorConvention(*errorConvention);

    writeInlinableBodyTextIfNeeded(fn);

    break;
  }

  case DeclKind::EnumElement: {
    auto elem = cast<EnumElementDecl>(D);
    auto contextID = addDeclContextRef(elem->getDeclContext());

    SmallVector<IdentifierID, 4> nameComponentsAndDependencies;
    nameComponentsAndDependencies.push_back(addDeclBaseNameRef(elem->getBaseName()));
    for (auto argName : elem->getFullName().getArgumentNames())
      nameComponentsAndDependencies.push_back(addDeclBaseNameRef(argName));

    Type ty = elem->getInterfaceType();
    for (Type dependency : collectDependenciesFromType(ty->getCanonicalType()))
      nameComponentsAndDependencies.push_back(addTypeRef(dependency));

    // We only serialize the raw values of @objc enums, because they're part
    // of the ABI. That isn't the case for Swift enums.
    auto RawValueKind = EnumElementRawValueKind::None;
    bool Negative = false;
    StringRef RawValueText;
    if (elem->getParentEnum()->isObjC()) {
      // Currently ObjC enums always have integer raw values.
      RawValueKind = EnumElementRawValueKind::IntegerLiteral;
      auto ILE = cast<IntegerLiteralExpr>(elem->getRawValueExpr());
      RawValueText = ILE->getDigitsText();
      Negative = ILE->isNegative();
    }

    uint8_t rawResilienceExpansion =
        getRawStableResilienceExpansion(
            elem->getDefaultArgumentResilienceExpansion());
    unsigned abbrCode = DeclTypeAbbrCodes[EnumElementLayout::Code];
    EnumElementLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                  contextID,
                                  elem->isImplicit(),
                                  elem->hasAssociatedValues(),
                                  (unsigned)RawValueKind,
                                  Negative,
                                  addUniquedStringRef(RawValueText),
                                  rawResilienceExpansion,
                                  elem->getFullName().getArgumentNames().size()+1,
                                  nameComponentsAndDependencies);
    if (auto *PL = elem->getParameterList())
      writeParameterList(PL);
    break;
  }

  case DeclKind::Subscript: {
    auto subscript = cast<SubscriptDecl>(D);
    verifyAttrSerializable(subscript);

    auto contextID = addDeclContextRef(subscript->getDeclContext());

    Accessors accessors = getAccessors(subscript);

    SmallVector<IdentifierID, 4> nameComponentsAndDependencies;
    for (auto argName : subscript->getFullName().getArgumentNames())
      nameComponentsAndDependencies.push_back(addDeclBaseNameRef(argName));

    for (auto accessor : accessors.Decls)
      nameComponentsAndDependencies.push_back(addDeclRef(accessor));

    Type ty = subscript->getInterfaceType();
    for (Type dependency : collectDependenciesFromType(ty->getCanonicalType()))
      nameComponentsAndDependencies.push_back(addTypeRef(dependency));

    uint8_t rawAccessLevel =
      getRawStableAccessLevel(subscript->getFormalAccess());
    uint8_t rawSetterAccessLevel = rawAccessLevel;
    if (subscript->isSettable())
      rawSetterAccessLevel =
        getRawStableAccessLevel(subscript->getSetterFormalAccess());

    unsigned abbrCode = DeclTypeAbbrCodes[SubscriptLayout::Code];
    SubscriptLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                contextID,
                                subscript->isImplicit(),
                                subscript->isObjC(),
                                subscript->isGetterMutating(),
                                subscript->isSetterMutating(),
                                accessors.OpaqueReadOwnership,
                                accessors.ReadImpl,
                                accessors.WriteImpl,
                                accessors.ReadWriteImpl,
                                accessors.Decls.size(),
                                addGenericEnvironmentRef(
                                            subscript->getGenericEnvironment()),
                                addTypeRef(subscript->getElementInterfaceType()),
                                addDeclRef(subscript->getOverriddenDecl()),
                                rawAccessLevel,
                                rawSetterAccessLevel,
                                subscript->
                                  getFullName().getArgumentNames().size(),
                                nameComponentsAndDependencies);

    writeGenericParams(subscript->getGenericParams());
    writeParameterList(subscript->getIndices());
    break;
  }


  case DeclKind::Constructor: {
    auto ctor = cast<ConstructorDecl>(D);
    verifyAttrSerializable(ctor);

    auto contextID = addDeclContextRef(ctor->getDeclContext());

    SmallVector<IdentifierID, 4> nameComponentsAndDependencies;
    for (auto argName : ctor->getFullName().getArgumentNames())
      nameComponentsAndDependencies.push_back(addDeclBaseNameRef(argName));

    Type ty = ctor->getInterfaceType();
    for (Type dependency : collectDependenciesFromType(ty->getCanonicalType()))
      nameComponentsAndDependencies.push_back(addTypeRef(dependency));

    uint8_t rawAccessLevel = getRawStableAccessLevel(ctor->getFormalAccess());
    uint8_t rawDefaultArgumentResilienceExpansion =
        getRawStableResilienceExpansion(
            ctor->getDefaultArgumentResilienceExpansion());

    bool firstTimeRequired = ctor->isRequired();
    if (auto *overridden = ctor->getOverriddenDecl())
      if (firstTimeRequired && overridden->isRequired())
        firstTimeRequired = false;

    unsigned abbrCode = DeclTypeAbbrCodes[ConstructorLayout::Code];
    ConstructorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                  contextID,
                                  getRawStableOptionalTypeKind(
                                    ctor->getFailability()),
                                  ctor->isImplicit(),
                                  ctor->isObjC(),
                                  ctor->hasStubImplementation(),
                                  ctor->hasThrows(),
                                  getStableCtorInitializerKind(
                                    ctor->getInitKind()),
                                  addGenericEnvironmentRef(
                                                 ctor->getGenericEnvironment()),
                                  addDeclRef(ctor->getOverriddenDecl()),
                                  rawAccessLevel,
                                  ctor->needsNewVTableEntry(),
                                  rawDefaultArgumentResilienceExpansion,
                                  firstTimeRequired,
                                  ctor->getFullName().getArgumentNames().size(),
                                  nameComponentsAndDependencies);

    writeGenericParams(ctor->getGenericParams());
    writeParameterList(ctor->getParameters());

    if (auto errorConvention = ctor->getForeignErrorConvention())
      writeForeignErrorConvention(*errorConvention);

    writeInlinableBodyTextIfNeeded(ctor);
    break;
  }

  case DeclKind::Destructor: {
    auto dtor = cast<DestructorDecl>(D);
    verifyAttrSerializable(dtor);

    auto contextID = addDeclContextRef(dtor->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[DestructorLayout::Code];
    DestructorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 contextID,
                                 dtor->isImplicit(),
                                 dtor->isObjC(),
                                 addGenericEnvironmentRef(
                                                dtor->getGenericEnvironment()));
    writeInlinableBodyTextIfNeeded(dtor);
    break;
  }

  case DeclKind::Module: {
    llvm_unreachable("FIXME: serialize these");
  }
  }
}

#define SIMPLE_CASE(TYPENAME, VALUE) \
  case swift::TYPENAME::VALUE: return uint8_t(serialization::TYPENAME::VALUE);

/// Translate from the AST function representation enum to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t getRawStableFunctionTypeRepresentation(
                                       swift::FunctionType::Representation cc) {
  switch (cc) {
  SIMPLE_CASE(FunctionTypeRepresentation, Swift)
  SIMPLE_CASE(FunctionTypeRepresentation, Block)
  SIMPLE_CASE(FunctionTypeRepresentation, Thin)
  SIMPLE_CASE(FunctionTypeRepresentation, CFunctionPointer)
  }
  llvm_unreachable("bad calling convention");
}

/// Translate from the AST function representation enum to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t getRawStableSILFunctionTypeRepresentation(
                                    swift::SILFunctionType::Representation cc) {
  switch (cc) {
  SIMPLE_CASE(SILFunctionTypeRepresentation, Thick)
  SIMPLE_CASE(SILFunctionTypeRepresentation, Block)
  SIMPLE_CASE(SILFunctionTypeRepresentation, Thin)
  SIMPLE_CASE(SILFunctionTypeRepresentation, CFunctionPointer)
  SIMPLE_CASE(SILFunctionTypeRepresentation, Method)
  SIMPLE_CASE(SILFunctionTypeRepresentation, ObjCMethod)
  SIMPLE_CASE(SILFunctionTypeRepresentation, WitnessMethod)
  SIMPLE_CASE(SILFunctionTypeRepresentation, Closure)
  }
  llvm_unreachable("bad calling convention");
}

/// Translate from the AST coroutine-kind enum to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t getRawStableSILCoroutineKind(
                                    swift::SILCoroutineKind kind) {
  switch (kind) {
  SIMPLE_CASE(SILCoroutineKind, None)
  SIMPLE_CASE(SILCoroutineKind, YieldOnce)
  SIMPLE_CASE(SILCoroutineKind, YieldMany)
  }
  llvm_unreachable("bad kind");
}

/// Translate from the AST ownership enum to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t
getRawStableReferenceOwnership(swift::ReferenceOwnership ownership) {
  switch (ownership) {
  SIMPLE_CASE(ReferenceOwnership, Strong)
#define REF_STORAGE(Name, ...) \
  SIMPLE_CASE(ReferenceOwnership, Name)
#include "swift/AST/ReferenceStorage.def"
  }
  llvm_unreachable("bad ownership kind");
}
/// Translate from the AST ownership enum to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t getRawStableValueOwnership(swift::ValueOwnership ownership) {
  switch (ownership) {
  SIMPLE_CASE(ValueOwnership, Default)
  SIMPLE_CASE(ValueOwnership, InOut)
  SIMPLE_CASE(ValueOwnership, Shared)
  SIMPLE_CASE(ValueOwnership, Owned)
  }
  llvm_unreachable("bad ownership kind");
}

/// Translate from the AST ParameterConvention enum to the
/// Serialization enum values, which are guaranteed to be stable.
static uint8_t getRawStableParameterConvention(swift::ParameterConvention pc) {
  switch (pc) {
  SIMPLE_CASE(ParameterConvention, Indirect_In)
  SIMPLE_CASE(ParameterConvention, Indirect_In_Constant)
  SIMPLE_CASE(ParameterConvention, Indirect_In_Guaranteed)
  SIMPLE_CASE(ParameterConvention, Indirect_Inout)
  SIMPLE_CASE(ParameterConvention, Indirect_InoutAliasable)
  SIMPLE_CASE(ParameterConvention, Direct_Owned)
  SIMPLE_CASE(ParameterConvention, Direct_Unowned)
  SIMPLE_CASE(ParameterConvention, Direct_Guaranteed)
  }
  llvm_unreachable("bad parameter convention kind");
}

/// Translate from the AST ResultConvention enum to the
/// Serialization enum values, which are guaranteed to be stable.
static uint8_t getRawStableResultConvention(swift::ResultConvention rc) {
  switch (rc) {
  SIMPLE_CASE(ResultConvention, Indirect)
  SIMPLE_CASE(ResultConvention, Owned)
  SIMPLE_CASE(ResultConvention, Unowned)
  SIMPLE_CASE(ResultConvention, UnownedInnerPointer)
  SIMPLE_CASE(ResultConvention, Autoreleased)
  }
  llvm_unreachable("bad result convention kind");
}

#undef SIMPLE_CASE

/// Find the typealias given a builtin type.
static TypeAliasDecl *findTypeAliasForBuiltin(ASTContext &Ctx, Type T) {
  /// Get the type name by chopping off "Builtin.".
  llvm::SmallString<32> FullName;
  llvm::raw_svector_ostream OS(FullName);
  T->print(OS);
  assert(FullName.startswith(BUILTIN_TYPE_NAME_PREFIX));
  StringRef TypeName = FullName.substr(8);

  SmallVector<ValueDecl*, 4> CurModuleResults;
  Ctx.TheBuiltinModule->lookupValue(ModuleDecl::AccessPathTy(),
                                    Ctx.getIdentifier(TypeName),
                                    NLKind::QualifiedLookup,
                                    CurModuleResults);
  assert(CurModuleResults.size() == 1);
  return cast<TypeAliasDecl>(CurModuleResults[0]);
}

void Serializer::writeType(Type ty) {
  using namespace decls_block;

  auto id = DeclAndTypeIDs[ty];
  assert(id != 0 && "type not referenced properly");
  (void)id;

  assert((id - 1) == TypeOffsets.size());

  TypeOffsets.push_back(Out.GetCurrentBitNo());

  switch (ty->getKind()) {
  case TypeKind::Error:
  case TypeKind::Unresolved:
    llvm_unreachable("should not serialize an invalid type");

  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinIntegerLiteral:
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinNativeObject:
  case TypeKind::BuiltinBridgeObject:
  case TypeKind::BuiltinUnknownObject:
  case TypeKind::BuiltinUnsafeValueBuffer:
  case TypeKind::BuiltinVector:
  case TypeKind::SILToken: {
    TypeAliasDecl *typeAlias =
      findTypeAliasForBuiltin(M->getASTContext(), ty);

    unsigned abbrCode = DeclTypeAbbrCodes[BuiltinAliasTypeLayout::Code];
    BuiltinAliasTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                       addDeclRef(typeAlias,
                                                  /*allowTypeAliasXRef*/true),
                                       TypeID());
    break;
  }
  case TypeKind::NameAlias: {
    auto alias = cast<NameAliasType>(ty.getPointer());
    const TypeAliasDecl *typeAlias = alias->getDecl();
    auto underlyingType = typeAlias->getUnderlyingTypeLoc().getType();

    unsigned abbrCode = DeclTypeAbbrCodes[NameAliasTypeLayout::Code];
    NameAliasTypeLayout::emitRecord(
                           Out, ScratchRecord, abbrCode,
                           addDeclRef(typeAlias, /*allowTypeAliasXRef*/true),
                           addTypeRef(alias->getParent()),
                           addTypeRef(underlyingType),
                           addTypeRef(alias->getSinglyDesugaredType()),
                           addSubstitutionMapRef(alias->getSubstitutionMap()));
    break;
  }

  case TypeKind::Paren: {
    auto parenTy = cast<ParenType>(ty.getPointer());
    assert(parenTy->getParameterFlags().isNone());

    unsigned abbrCode = DeclTypeAbbrCodes[ParenTypeLayout::Code];
    ParenTypeLayout::emitRecord(
        Out, ScratchRecord, abbrCode, addTypeRef(parenTy->getUnderlyingType()));
    break;
  }

  case TypeKind::Tuple: {
    auto tupleTy = cast<TupleType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[TupleTypeLayout::Code];
    TupleTypeLayout::emitRecord(Out, ScratchRecord, abbrCode);

    abbrCode = DeclTypeAbbrCodes[TupleTypeEltLayout::Code];
    for (auto &elt : tupleTy->getElements()) {
      assert(elt.getParameterFlags().isNone());
      TupleTypeEltLayout::emitRecord(
          Out, ScratchRecord, abbrCode,
          addDeclBaseNameRef(elt.getName()),
          addTypeRef(elt.getType()));
    }

    break;
  }

  case TypeKind::Struct:
  case TypeKind::Enum:
  case TypeKind::Class:
  case TypeKind::Protocol: {
    auto nominalTy = cast<NominalType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[NominalTypeLayout::Code];
    NominalTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                  addDeclRef(nominalTy->getDecl()),
                                  addTypeRef(nominalTy->getParent()));
    break;
  }

  case TypeKind::ExistentialMetatype: {
    auto metatypeTy = cast<ExistentialMetatypeType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[ExistentialMetatypeTypeLayout::Code];

    // Map the metatype representation.
    auto repr = getRawStableMetatypeRepresentation(metatypeTy);
    ExistentialMetatypeTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addTypeRef(metatypeTy->getInstanceType()),
                                              static_cast<uint8_t>(repr));
    break;
  }

  case TypeKind::Metatype: {
    auto metatypeTy = cast<MetatypeType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[MetatypeTypeLayout::Code];

    // Map the metatype representation.
    auto repr = getRawStableMetatypeRepresentation(metatypeTy);
    MetatypeTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addTypeRef(metatypeTy->getInstanceType()),
                                   static_cast<uint8_t>(repr));
    break;
  }

  case TypeKind::Module:
    llvm_unreachable("modules are currently not first-class values");

  case TypeKind::DynamicSelf: {
    auto dynamicSelfTy = cast<DynamicSelfType>(ty.getPointer());
    unsigned abbrCode = DeclTypeAbbrCodes[DynamicSelfTypeLayout::Code];
    DynamicSelfTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      addTypeRef(dynamicSelfTy->getSelfType()));
    break;
  }

  case TypeKind::Archetype: {
    auto archetypeTy = cast<ArchetypeType>(ty.getPointer());

    // Opened existential types use a separate layout.
    if (auto existentialTy = archetypeTy->getOpenedExistentialType()) {
      unsigned abbrCode = DeclTypeAbbrCodes[OpenedExistentialTypeLayout::Code];
      OpenedExistentialTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                              addTypeRef(existentialTy));
      break;
    }

    auto env = archetypeTy->getGenericEnvironment();
    assert(env && "Primary archetype without generic environment?");

    GenericEnvironmentID envID = addGenericEnvironmentRef(env);
    Type interfaceType = archetypeTy->getInterfaceType();

    unsigned abbrCode = DeclTypeAbbrCodes[ArchetypeTypeLayout::Code];
    ArchetypeTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                    envID, addTypeRef(interfaceType));
    break;
  }

  case TypeKind::GenericTypeParam: {
    auto genericParam = cast<GenericTypeParamType>(ty.getPointer());
    unsigned abbrCode = DeclTypeAbbrCodes[GenericTypeParamTypeLayout::Code];
    DeclID declIDOrDepth;
    unsigned indexPlusOne;
    if (genericParam->getDecl() &&
        !(genericParam->getDecl()->getDeclContext()->isModuleScopeContext() &&
          isDeclXRef(genericParam->getDecl()))) {
      declIDOrDepth = addDeclRef(genericParam->getDecl());
      indexPlusOne = 0;
    } else {
      declIDOrDepth = genericParam->getDepth();
      indexPlusOne = genericParam->getIndex() + 1;
    }
    GenericTypeParamTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                           declIDOrDepth, indexPlusOne);
    break;
  }

  case TypeKind::DependentMember: {
    auto dependent = cast<DependentMemberType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[DependentMemberTypeLayout::Code];
    assert(dependent->getAssocType() && "Unchecked dependent member type");
    DependentMemberTypeLayout::emitRecord(
      Out, ScratchRecord, abbrCode,
      addTypeRef(dependent->getBase()),
      addDeclRef(dependent->getAssocType()));
    break;
  }

  case TypeKind::Function:
  case TypeKind::GenericFunction: {
    auto *fnTy = cast<AnyFunctionType>(ty.getPointer());

    if (isa<FunctionType>(fnTy)) {
      unsigned abbrCode = DeclTypeAbbrCodes[FunctionTypeLayout::Code];
      FunctionTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
             addTypeRef(fnTy->getResult()),
             getRawStableFunctionTypeRepresentation(fnTy->getRepresentation()),
             fnTy->isNoEscape(),
             fnTy->throws());
    } else {
      assert(!fnTy->isNoEscape());

      auto *genericSig = cast<GenericFunctionType>(fnTy)->getGenericSignature();
      unsigned abbrCode = DeclTypeAbbrCodes[GenericFunctionTypeLayout::Code];
      GenericFunctionTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
              addTypeRef(fnTy->getResult()),
              getRawStableFunctionTypeRepresentation(fnTy->getRepresentation()),
              fnTy->throws(),
              addGenericSignatureRef(genericSig));
    }

    unsigned abbrCode = DeclTypeAbbrCodes[FunctionParamLayout::Code];
    for (auto &param : fnTy->getParams()) {
      auto paramFlags = param.getParameterFlags();
      auto rawOwnership =
          getRawStableValueOwnership(paramFlags.getValueOwnership());
      FunctionParamLayout::emitRecord(
          Out, ScratchRecord, abbrCode, addDeclBaseNameRef(param.getLabel()),
          addTypeRef(param.getPlainType()), paramFlags.isVariadic(),
          paramFlags.isAutoClosure(), paramFlags.isEscaping(), rawOwnership);
    }

    break;
  }
      
  case TypeKind::SILBlockStorage: {
    auto storageTy = cast<SILBlockStorageType>(ty.getPointer());
    
    unsigned abbrCode = DeclTypeAbbrCodes[SILBlockStorageTypeLayout::Code];
    SILBlockStorageTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      addTypeRef(storageTy->getCaptureType()));
    break;
  }
      
  case TypeKind::SILBox: {
    auto boxTy = cast<SILBoxType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[SILBoxTypeLayout::Code];
    SILLayoutID layoutRef = addSILLayoutRef(boxTy->getLayout());

    SILBoxTypeLayout::emitRecord(Out, ScratchRecord, abbrCode, layoutRef,
                          addSubstitutionMapRef(boxTy->getSubstitutions()));
    break;
  }
      
  case TypeKind::SILFunction: {
    auto fnTy = cast<SILFunctionType>(ty.getPointer());

    auto representation = fnTy->getRepresentation();
    auto stableRepresentation =
      getRawStableSILFunctionTypeRepresentation(representation);
    
    SmallVector<TypeID, 8> variableData;
    for (auto param : fnTy->getParameters()) {
      variableData.push_back(addTypeRef(param.getType()));
      unsigned conv = getRawStableParameterConvention(param.getConvention());
      variableData.push_back(TypeID(conv));
    }
    for (auto yield : fnTy->getYields()) {
      variableData.push_back(addTypeRef(yield.getType()));
      unsigned conv = getRawStableParameterConvention(yield.getConvention());
      variableData.push_back(TypeID(conv));
    }
    for (auto result : fnTy->getResults()) {
      variableData.push_back(addTypeRef(result.getType()));
      unsigned conv = getRawStableResultConvention(result.getConvention());
      variableData.push_back(TypeID(conv));
    }
    if (fnTy->hasErrorResult()) {
      auto abResult = fnTy->getErrorResult();
      variableData.push_back(addTypeRef(abResult.getType()));
      unsigned conv = getRawStableResultConvention(abResult.getConvention());
      variableData.push_back(TypeID(conv));
    }

    auto sig = fnTy->getGenericSignature();

    auto stableCoroutineKind = 
      getRawStableSILCoroutineKind(fnTy->getCoroutineKind());

    auto stableCalleeConvention =
      getRawStableParameterConvention(fnTy->getCalleeConvention());

    unsigned abbrCode = DeclTypeAbbrCodes[SILFunctionTypeLayout::Code];
    SILFunctionTypeLayout::emitRecord(
        Out, ScratchRecord, abbrCode,
        stableCoroutineKind, stableCalleeConvention,
        stableRepresentation, fnTy->isPseudogeneric(), fnTy->isNoEscape(),
        fnTy->hasErrorResult(), fnTy->getParameters().size(),
        fnTy->getNumYields(), fnTy->getNumResults(),
        addGenericSignatureRef(sig), variableData);

    if (auto conformance = fnTy->getWitnessMethodConformanceOrNone())
      writeConformance(*conformance, DeclTypeAbbrCodes);

    break;
  }
      
  case TypeKind::ArraySlice: {
    auto sliceTy = cast<ArraySliceType>(ty.getPointer());

    Type base = sliceTy->getBaseType();

    unsigned abbrCode = DeclTypeAbbrCodes[ArraySliceTypeLayout::Code];
    ArraySliceTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addTypeRef(base));
    break;
  }

  case TypeKind::Dictionary: {
    auto dictTy = cast<DictionaryType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[DictionaryTypeLayout::Code];
    DictionaryTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addTypeRef(dictTy->getKeyType()),
                                     addTypeRef(dictTy->getValueType()));
    break;
  }

  case TypeKind::Optional: {
    auto optionalTy = cast<OptionalType>(ty.getPointer());

    Type base = optionalTy->getBaseType();

    unsigned abbrCode = DeclTypeAbbrCodes[OptionalTypeLayout::Code];
    OptionalTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addTypeRef(base));
    break;
  }

  case TypeKind::ProtocolComposition: {
    auto composition = cast<ProtocolCompositionType>(ty.getPointer());

    SmallVector<TypeID, 4> protocols;
    for (auto proto : composition->getMembers())
      protocols.push_back(addTypeRef(proto));

    unsigned abbrCode = DeclTypeAbbrCodes[ProtocolCompositionTypeLayout::Code];
    ProtocolCompositionTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                              composition->hasExplicitAnyObject(),
                                              protocols);
    break;
  }

#define REF_STORAGE(Name, ...) \
  case TypeKind::Name##Storage:
#include "swift/AST/ReferenceStorage.def"
  {
    auto refTy = cast<ReferenceStorageType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[ReferenceStorageTypeLayout::Code];
    auto stableOwnership =
        getRawStableReferenceOwnership(refTy->getOwnership());
    ReferenceStorageTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                           stableOwnership,
                                  addTypeRef(refTy->getReferentType()));
    break;
  }

  case TypeKind::UnboundGeneric: {
    auto generic = cast<UnboundGenericType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[UnboundGenericTypeLayout::Code];
    UnboundGenericTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                         addDeclRef(generic->getDecl(),
                                                    /*allowTypeAliasXRef*/true),
                                         addTypeRef(generic->getParent()));
    break;
  }

  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct: {
    auto generic = cast<BoundGenericType>(ty.getPointer());
    SmallVector<TypeID, 8> genericArgIDs;

    for (auto next : generic->getGenericArgs())
      genericArgIDs.push_back(addTypeRef(next));

    unsigned abbrCode = DeclTypeAbbrCodes[BoundGenericTypeLayout::Code];
    BoundGenericTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                       addDeclRef(generic->getDecl()),
                                       addTypeRef(generic->getParent()),
                                       genericArgIDs);
    break;
  }

  case TypeKind::InOut:
    llvm_unreachable("inout types are only used in function type parameters");
  case TypeKind::LValue:
    llvm_unreachable("lvalue types are only used in function bodies");
  case TypeKind::TypeVariable:
    llvm_unreachable("type variables should not escape the type checker");
  }
}

void Serializer::writeAllDeclsAndTypes() {
  BCBlockRAII restoreBlock(Out, DECLS_AND_TYPES_BLOCK_ID, 8);
  using namespace decls_block;
  registerDeclTypeAbbr<BuiltinAliasTypeLayout>();
  registerDeclTypeAbbr<NameAliasTypeLayout>();
  registerDeclTypeAbbr<GenericTypeParamDeclLayout>();
  registerDeclTypeAbbr<AssociatedTypeDeclLayout>();
  registerDeclTypeAbbr<NominalTypeLayout>();
  registerDeclTypeAbbr<ParenTypeLayout>();
  registerDeclTypeAbbr<TupleTypeLayout>();
  registerDeclTypeAbbr<TupleTypeEltLayout>();
  registerDeclTypeAbbr<FunctionTypeLayout>();
  registerDeclTypeAbbr<FunctionParamLayout>();
  registerDeclTypeAbbr<MetatypeTypeLayout>();
  registerDeclTypeAbbr<ExistentialMetatypeTypeLayout>();
  registerDeclTypeAbbr<ArchetypeTypeLayout>();
  registerDeclTypeAbbr<ProtocolCompositionTypeLayout>();
  registerDeclTypeAbbr<BoundGenericTypeLayout>();
  registerDeclTypeAbbr<GenericFunctionTypeLayout>();
  registerDeclTypeAbbr<SILBlockStorageTypeLayout>();
  registerDeclTypeAbbr<SILBoxTypeLayout>();
  registerDeclTypeAbbr<SILFunctionTypeLayout>();
  registerDeclTypeAbbr<ArraySliceTypeLayout>();
  registerDeclTypeAbbr<DictionaryTypeLayout>();
  registerDeclTypeAbbr<ReferenceStorageTypeLayout>();
  registerDeclTypeAbbr<UnboundGenericTypeLayout>();
  registerDeclTypeAbbr<OptionalTypeLayout>();
  registerDeclTypeAbbr<DynamicSelfTypeLayout>();
  registerDeclTypeAbbr<OpenedExistentialTypeLayout>();

  registerDeclTypeAbbr<TypeAliasLayout>();
  registerDeclTypeAbbr<GenericTypeParamTypeLayout>();
  registerDeclTypeAbbr<DependentMemberTypeLayout>();
  registerDeclTypeAbbr<StructLayout>();
  registerDeclTypeAbbr<ConstructorLayout>();
  registerDeclTypeAbbr<VarLayout>();
  registerDeclTypeAbbr<ParamLayout>();
  registerDeclTypeAbbr<FuncLayout>();
  registerDeclTypeAbbr<AccessorLayout>();
  registerDeclTypeAbbr<PatternBindingLayout>();
  registerDeclTypeAbbr<ProtocolLayout>();
  registerDeclTypeAbbr<DefaultWitnessTableLayout>();
  registerDeclTypeAbbr<PrefixOperatorLayout>();
  registerDeclTypeAbbr<PostfixOperatorLayout>();
  registerDeclTypeAbbr<InfixOperatorLayout>();
  registerDeclTypeAbbr<PrecedenceGroupLayout>();
  registerDeclTypeAbbr<ClassLayout>();
  registerDeclTypeAbbr<EnumLayout>();
  registerDeclTypeAbbr<EnumElementLayout>();
  registerDeclTypeAbbr<SubscriptLayout>();
  registerDeclTypeAbbr<ExtensionLayout>();
  registerDeclTypeAbbr<DestructorLayout>();

  registerDeclTypeAbbr<ParameterListLayout>();

  registerDeclTypeAbbr<ParenPatternLayout>();
  registerDeclTypeAbbr<TuplePatternLayout>();
  registerDeclTypeAbbr<TuplePatternEltLayout>();
  registerDeclTypeAbbr<NamedPatternLayout>();
  registerDeclTypeAbbr<VarPatternLayout>();
  registerDeclTypeAbbr<AnyPatternLayout>();
  registerDeclTypeAbbr<TypedPatternLayout>();
  registerDeclTypeAbbr<InlinableBodyTextLayout>();
  registerDeclTypeAbbr<GenericParamListLayout>();
  registerDeclTypeAbbr<GenericSignatureLayout>();
  registerDeclTypeAbbr<GenericRequirementLayout>();
  registerDeclTypeAbbr<LayoutRequirementLayout>();
  registerDeclTypeAbbr<SILGenericEnvironmentLayout>();
  registerDeclTypeAbbr<SubstitutionMapLayout>();

  registerDeclTypeAbbr<ForeignErrorConventionLayout>();
  registerDeclTypeAbbr<DeclContextLayout>();
  registerDeclTypeAbbr<AbstractClosureExprLayout>();
  registerDeclTypeAbbr<PatternBindingInitializerLayout>();
  registerDeclTypeAbbr<DefaultArgumentInitializerLayout>();
  registerDeclTypeAbbr<TopLevelCodeDeclContextLayout>();

  registerDeclTypeAbbr<XRefTypePathPieceLayout>();
  registerDeclTypeAbbr<XRefValuePathPieceLayout>();
  registerDeclTypeAbbr<XRefExtensionPathPieceLayout>();
  registerDeclTypeAbbr<XRefOperatorOrAccessorPathPieceLayout>();
  registerDeclTypeAbbr<XRefGenericParamPathPieceLayout>();
  registerDeclTypeAbbr<XRefInitializerPathPieceLayout>();

  registerDeclTypeAbbr<AbstractProtocolConformanceLayout>();
  registerDeclTypeAbbr<NormalProtocolConformanceLayout>();
  registerDeclTypeAbbr<SpecializedProtocolConformanceLayout>();
  registerDeclTypeAbbr<InheritedProtocolConformanceLayout>();
  registerDeclTypeAbbr<InvalidProtocolConformanceLayout>();
  registerDeclTypeAbbr<NormalProtocolConformanceIdLayout>();
  registerDeclTypeAbbr<ProtocolConformanceXrefLayout>();

  registerDeclTypeAbbr<SILLayoutLayout>();

  registerDeclTypeAbbr<LocalDiscriminatorLayout>();
  registerDeclTypeAbbr<PrivateDiscriminatorLayout>();
  registerDeclTypeAbbr<FilenameForPrivateLayout>();
  registerDeclTypeAbbr<MembersLayout>();
  registerDeclTypeAbbr<XRefLayout>();

#define DECL_ATTR(X, NAME, ...) \
  registerDeclTypeAbbr<NAME##DeclAttrLayout>();
#include "swift/AST/Attr.def"

  do {
    // Each of these loops can trigger the others to execute again, so repeat
    // until /all/ of the pending lists are empty.
    while (!DeclsAndTypesToWrite.empty()) {
      auto next = DeclsAndTypesToWrite.front();
      DeclsAndTypesToWrite.pop();

      if (next.isDecl())
        writeDecl(next.getDecl());
      else
        writeType(next.getType());
    }

    while (!LocalDeclContextsToWrite.empty()) {
      auto next = LocalDeclContextsToWrite.front();
      LocalDeclContextsToWrite.pop();
      writeLocalDeclContext(next);
    }

    while (!DeclContextsToWrite.empty()) {
      auto next = DeclContextsToWrite.front();
      DeclContextsToWrite.pop();
      writeDeclContext(next);
    }

    while (!GenericSignaturesToWrite.empty()) {
      auto next = GenericSignaturesToWrite.front();
      GenericSignaturesToWrite.pop();
      writeGenericSignature(next);
    }

    while (!GenericEnvironmentsToWrite.empty()) {
      auto next = GenericEnvironmentsToWrite.front();
      GenericEnvironmentsToWrite.pop();
      writeGenericEnvironment(next);
    }

    while (!SubstitutionMapsToWrite.empty()) {
      auto next = SubstitutionMapsToWrite.front();
      SubstitutionMapsToWrite.pop();
      writeSubstitutionMap(next);
    }

    while (!NormalConformancesToWrite.empty()) {
      auto next = NormalConformancesToWrite.front();
      NormalConformancesToWrite.pop();
      writeNormalConformance(next);
    }
    
    while (!SILLayoutsToWrite.empty()) {
      auto next = SILLayoutsToWrite.front();
      SILLayoutsToWrite.pop();
      writeSILLayout(next);
    }
  } while (!DeclsAndTypesToWrite.empty() ||
           !LocalDeclContextsToWrite.empty() ||
           !DeclContextsToWrite.empty() ||
           !SILLayoutsToWrite.empty() ||
           !GenericSignaturesToWrite.empty() ||
           !GenericEnvironmentsToWrite.empty() ||
           !SubstitutionMapsToWrite.empty() ||
           !NormalConformancesToWrite.empty());
}

void Serializer::writeAllIdentifiers() {
  BCBlockRAII restoreBlock(Out, IDENTIFIER_DATA_BLOCK_ID, 3);
  identifier_block::IdentifierDataLayout IdentifierData(Out);

  llvm::SmallString<4096> stringData;

  // Make sure no identifier has an offset of 0.
  stringData.push_back('\0');

  for (StringRef str : StringsToWrite) {
    IdentifierOffsets.push_back(stringData.size());
    stringData.append(str);
    stringData.push_back('\0');
  }

  IdentifierData.emit(ScratchRecord, stringData.str());
}

void Serializer::writeOffsets(const index_block::OffsetsLayout &Offsets,
                              const std::vector<BitOffset> &values) {
  Offsets.emit(ScratchRecord, getOffsetRecordCode(values), values);
}

/// Writes an in-memory decl table to an on-disk representation, using the
/// given layout.
static void writeDeclTable(const index_block::DeclListLayout &DeclList,
                           index_block::RecordKind kind,
                           const Serializer::DeclTable &table) {
  if (table.empty())
    return;

  SmallVector<uint64_t, 8> scratch;
  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::OnDiskChainedHashTableGenerator<DeclTableInfo> generator;
    for (auto &entry : table)
      generator.insert(entry.first, entry.second);

    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    endian::write<uint32_t>(blobStream, 0, little);
    tableOffset = generator.Emit(blobStream);
  }

  DeclList.emit(scratch, kind, tableOffset, hashTableBlob);
}

static void
writeExtensionTable(const index_block::ExtensionTableLayout &ExtensionTable,
                    const Serializer::ExtensionTable &table,
                    Serializer &serializer) {
  if (table.empty())
    return;

  SmallVector<uint64_t, 8> scratch;
  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::OnDiskChainedHashTableGenerator<ExtensionTableInfo> generator;
    ExtensionTableInfo info{serializer};
    for (auto &entry : table) {
      generator.insert(entry.first, entry.second, info);
    }

    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    endian::write<uint32_t>(blobStream, 0, little);
    tableOffset = generator.Emit(blobStream, info);
  }

  ExtensionTable.emit(scratch, tableOffset, hashTableBlob);
}

static void writeLocalDeclTable(const index_block::DeclListLayout &DeclList,
                                index_block::RecordKind kind,
                                LocalTypeHashTableGenerator &generator) {
  SmallVector<uint64_t, 8> scratch;
  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    endian::write<uint32_t>(blobStream, 0, little);
    tableOffset = generator.Emit(blobStream);
  }

  DeclList.emit(scratch, kind, tableOffset, hashTableBlob);
}

static void
writeNestedTypeDeclsTable(const index_block::NestedTypeDeclsLayout &declList,
                          const Serializer::NestedTypeDeclsTable &table) {
  SmallVector<uint64_t, 8> scratch;
  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::OnDiskChainedHashTableGenerator<NestedTypeDeclsTableInfo> generator;
    for (auto &entry : table)
      generator.insert(entry.first, entry.second);

    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    endian::write<uint32_t>(blobStream, 0, little);
    tableOffset = generator.Emit(blobStream);
  }

  declList.emit(scratch, tableOffset, hashTableBlob);
}

static void
writeDeclMemberNamesTable(const index_block::DeclMemberNamesLayout &declNames,
                          const Serializer::DeclMemberNamesTable &table) {
  SmallVector<uint64_t, 8> scratch;
  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::OnDiskChainedHashTableGenerator<DeclMemberNamesTableInfo> generator;
    // Emit the offsets of the sub-tables; the tables themselves have been
    // separately emitted into DECL_MEMBER_TABLES_BLOCK by now.
    for (auto &entry : table) {
      // Or they _should_ have been; check for nonzero offsets.
      assert(static_cast<unsigned>(entry.second.first) != 0);
      generator.insert(entry.first, entry.second.first);
    }

    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    endian::write<uint32_t>(blobStream, 0, little);
    tableOffset = generator.Emit(blobStream);
  }

  declNames.emit(scratch, tableOffset, hashTableBlob);
}

static void
writeDeclMembersTable(const decl_member_tables_block::DeclMembersLayout &mems,
                      const Serializer::DeclMembersTable &table) {
  SmallVector<uint64_t, 8> scratch;
  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::OnDiskChainedHashTableGenerator<DeclMembersTableInfo> generator;
    for (auto &entry : table)
      generator.insert(entry.first, entry.second);

    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    endian::write<uint32_t>(blobStream, 0, little);
    tableOffset = generator.Emit(blobStream);
  }

  mems.emit(scratch, tableOffset, hashTableBlob);
}

namespace {
  /// Used to serialize the on-disk Objective-C method hash table.
  class ObjCMethodTableInfo {
  public:
    using key_type = ObjCSelector;
    using key_type_ref = key_type;
    using data_type = Serializer::ObjCMethodTableData;
    using data_type_ref = const data_type &;
    using hash_value_type = uint32_t;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      llvm::SmallString<32> scratch;
      // FIXME: DJB seed=0, audit whether the default seed could be used.
      return llvm::djbHash(key.getString(scratch), 0);
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      llvm::SmallString<32> scratch;
      auto keyLength = key.getString(scratch).size();
      assert(keyLength <= std::numeric_limits<uint16_t>::max() &&
             "selector too long");
      uint32_t dataLength = 0;
      for (const auto &entry : data) {
        dataLength += sizeof(uint32_t) + 1 + sizeof(uint32_t);
        dataLength += std::get<0>(entry).size();
      }

      endian::Writer writer(out, little);
      writer.write<uint16_t>(keyLength);
      writer.write<uint32_t>(dataLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
#ifndef NDEBUG
      uint64_t start = out.tell();
#endif
      out << key;
      assert((out.tell() - start == len) && "measured key length incorrectly");
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      static_assert(declIDFitsIn32Bits(), "DeclID too large");
      endian::Writer writer(out, little);
      for (const auto &entry : data) {
        writer.write<uint32_t>(std::get<0>(entry).size());
        writer.write<uint8_t>(std::get<1>(entry));
        writer.write<uint32_t>(std::get<2>(entry));
        out.write(std::get<0>(entry).c_str(), std::get<0>(entry).size());
      }
    }
  };
} // end anonymous namespace

static void writeObjCMethodTable(const index_block::ObjCMethodTableLayout &out,
                                 Serializer::ObjCMethodTable &objcMethods) {
  // Collect all of the Objective-C selectors in the method table.
  std::vector<ObjCSelector> selectors;
  for (const auto &entry : objcMethods) {
    selectors.push_back(entry.first);
  }

  // Sort the Objective-C selectors so we emit them in a stable order.
  llvm::array_pod_sort(selectors.begin(), selectors.end());

  // Create the on-disk hash table.
  llvm::OnDiskChainedHashTableGenerator<ObjCMethodTableInfo> generator;
  llvm::SmallString<32> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::raw_svector_ostream blobStream(hashTableBlob);
    for (auto selector : selectors) {
      generator.insert(selector, objcMethods[selector]);
    }

    // Make sure that no bucket is at offset 0
    endian::write<uint32_t>(blobStream, 0, little);
    tableOffset = generator.Emit(blobStream);
  }

  SmallVector<uint64_t, 8> scratch;
  out.emit(scratch, tableOffset, hashTableBlob);
}

/// Recursively walks the members and derived global decls of any nominal types
/// to build up global tables.
template<typename Range>
static void collectInterestingNestedDeclarations(
    Serializer &S,
    Range members,
    Serializer::DeclTable &operatorMethodDecls,
    Serializer::ObjCMethodTable &objcMethods,
    Serializer::NestedTypeDeclsTable &nestedTypeDecls,
    bool isLocal = false) {
  const NominalTypeDecl *nominalParent = nullptr;

  for (const Decl *member : members) {
    // If there is a corresponding Objective-C method, record it.
    auto recordObjCMethod = [&] {
      if (isLocal)
        return;

      if (auto func = dyn_cast<AbstractFunctionDecl>(member)) {
        if (func->isObjC()) {
          if (auto owningClass = func->getDeclContext()->getSelfClassDecl()) {
            Mangle::ASTMangler mangler;
            std::string ownerName = mangler.mangleNominalType(owningClass);
            assert(!ownerName.empty() && "Mangled type came back empty!");

            objcMethods[func->getObjCSelector()].push_back(
              std::make_tuple(ownerName,
                              func->isObjCInstanceMethod(),
                              S.addDeclRef(func)));
          }
        }
      }
    };

    if (auto memberValue = dyn_cast<ValueDecl>(member)) {
      if (!memberValue->hasName()) {
        recordObjCMethod();
        continue;
      }

      if (memberValue->isOperator()) {
        // Add operator methods.
        // Note that we don't have to add operators that are already in the
        // top-level list.
        operatorMethodDecls[memberValue->getBaseName()].push_back({
          /*ignored*/0,
          S.addDeclRef(memberValue)
        });
      }
    }

    if (auto nestedType = dyn_cast<TypeDecl>(member)) {
      if (nestedType->getEffectiveAccess() > swift::AccessLevel::FilePrivate) {
        if (!nominalParent) {
          const DeclContext *DC = member->getDeclContext();
          nominalParent = DC->getSelfNominalTypeDecl();
          assert(nominalParent && "parent context is not a type or extension");
        }
        nestedTypeDecls[nestedType->getName()].push_back({
          S.addDeclRef(nominalParent),
          S.addDeclRef(nestedType)
        });
      }
    }

    // Recurse into nested declarations.
    if (auto iterable = dyn_cast<IterableDeclContext>(member)) {
      collectInterestingNestedDeclarations(S, iterable->getMembers(),
                                           operatorMethodDecls,
                                           objcMethods, nestedTypeDecls,
                                           isLocal);
    }

    // Record Objective-C methods.
    recordObjCMethod();
  }
}

void Serializer::writeAST(ModuleOrSourceFile DC,
                          bool enableNestedTypeLookupTable) {
  DeclTable topLevelDecls, operatorDecls, operatorMethodDecls;
  DeclTable precedenceGroupDecls;
  ObjCMethodTable objcMethods;
  NestedTypeDeclsTable nestedTypeDecls;
  LocalTypeHashTableGenerator localTypeGenerator;
  ExtensionTable extensionDecls;
  bool hasLocalTypes = false;

  Optional<DeclID> entryPointClassID;
  SmallVector<DeclID, 16> orderedTopLevelDecls;

  ArrayRef<const FileUnit *> files;
  SmallVector<const FileUnit *, 1> Scratch;
  if (SF) {
    Scratch.push_back(SF);
    files = llvm::makeArrayRef(Scratch);
  } else {
    files = M->getFiles();
  }
  for (auto nextFile : files) {
    if (nextFile->hasEntryPoint())
      entryPointClassID = addDeclRef(nextFile->getMainClass());

    // FIXME: Switch to a visitor interface?
    SmallVector<Decl *, 32> fileDecls;
    nextFile->getTopLevelDecls(fileDecls);

    for (auto D : fileDecls) {
      if (isa<ImportDecl>(D) || isa<IfConfigDecl>(D) ||
          isa<PoundDiagnosticDecl>(D) || isa<TopLevelCodeDecl>(D)) {
        continue;
      }

      if (auto VD = dyn_cast<ValueDecl>(D)) {
        if (!VD->hasName())
          continue;
        topLevelDecls[VD->getBaseName()]
          .push_back({ getKindForTable(D), addDeclRef(D) });
      } else if (auto ED = dyn_cast<ExtensionDecl>(D)) {
        const NominalTypeDecl *extendedNominal = ED->getExtendedNominal();
        extensionDecls[extendedNominal->getName()]
          .push_back({ extendedNominal, addDeclRef(D) });
      } else if (auto OD = dyn_cast<OperatorDecl>(D)) {
        operatorDecls[OD->getName()]
          .push_back({ getStableFixity(OD->getKind()), addDeclRef(D) });
      } else if (auto PGD = dyn_cast<PrecedenceGroupDecl>(D)) {
        precedenceGroupDecls[PGD->getName()]
          .push_back({ decls_block::PRECEDENCE_GROUP_DECL, addDeclRef(D) });
      } else if (isa<PatternBindingDecl>(D)) {
        // No special handling needed.
      } else {
        llvm_unreachable("all top-level declaration kinds accounted for");
      }

      orderedTopLevelDecls.push_back(addDeclRef(D));

      // If this is a global variable, force the accessors to be
      // serialized.
      if (auto VD = dyn_cast<VarDecl>(D)) {
        if (VD->getGetter())
          addDeclRef(VD->getGetter());
        if (VD->getSetter())
          addDeclRef(VD->getSetter());
      }

      // If this nominal type has associated top-level decls for a
      // derived conformance (for example, ==), force them to be
      // serialized.
      if (auto IDC = dyn_cast<IterableDeclContext>(D)) {
        collectInterestingNestedDeclarations(*this, IDC->getMembers(),
                                             operatorMethodDecls, objcMethods,
                                             nestedTypeDecls);
      }
    }

    SmallVector<TypeDecl *, 16> localTypeDecls;
    nextFile->getLocalTypeDecls(localTypeDecls);

    for (auto TD : localTypeDecls) {
      hasLocalTypes = true;
      Mangle::ASTMangler Mangler;
      std::string MangledName = Mangler.mangleDeclAsUSR(TD, /*USRPrefix*/"");
      assert(!MangledName.empty() && "Mangled type came back empty!");
      localTypeGenerator.insert(MangledName, addDeclRef(TD));

      if (auto IDC = dyn_cast<IterableDeclContext>(TD)) {
        collectInterestingNestedDeclarations(*this, IDC->getMembers(),
                                             operatorMethodDecls, objcMethods,
                                             nestedTypeDecls, /*isLocal=*/true);
      }
    }
  }

  writeAllDeclsAndTypes();
  writeAllIdentifiers();

  {
    BCBlockRAII restoreBlock(Out, INDEX_BLOCK_ID, 4);

    index_block::OffsetsLayout Offsets(Out);
    writeOffsets(Offsets, DeclOffsets);
    writeOffsets(Offsets, TypeOffsets);
    writeOffsets(Offsets, IdentifierOffsets);
    writeOffsets(Offsets, DeclContextOffsets);
    writeOffsets(Offsets, LocalDeclContextOffsets);
    writeOffsets(Offsets, GenericSignatureOffsets);
    writeOffsets(Offsets, GenericEnvironmentOffsets);
    writeOffsets(Offsets, SubstitutionMapOffsets);
    writeOffsets(Offsets, NormalConformanceOffsets);
    writeOffsets(Offsets, SILLayoutOffsets);

    index_block::DeclListLayout DeclList(Out);
    writeDeclTable(DeclList, index_block::TOP_LEVEL_DECLS, topLevelDecls);
    writeDeclTable(DeclList, index_block::OPERATORS, operatorDecls);
    writeDeclTable(DeclList, index_block::PRECEDENCE_GROUPS, precedenceGroupDecls);
    writeDeclTable(DeclList, index_block::CLASS_MEMBERS_FOR_DYNAMIC_LOOKUP,
                   ClassMembersForDynamicLookup);
    writeDeclTable(DeclList, index_block::OPERATOR_METHODS, operatorMethodDecls);
    if (hasLocalTypes)
      writeLocalDeclTable(DeclList, index_block::LOCAL_TYPE_DECLS,
                          localTypeGenerator);

    if (!extensionDecls.empty()) {
      index_block::ExtensionTableLayout ExtensionTable(Out);
      writeExtensionTable(ExtensionTable, extensionDecls, *this);
    }

    index_block::OrderedDeclsLayout OrderedDecls(Out);
    OrderedDecls.emit(ScratchRecord, index_block::ORDERED_TOP_LEVEL_DECLS,
                      orderedTopLevelDecls);

    index_block::ObjCMethodTableLayout ObjCMethodTable(Out);
    writeObjCMethodTable(ObjCMethodTable, objcMethods);

    if (enableNestedTypeLookupTable &&
        !nestedTypeDecls.empty()) {
      index_block::NestedTypeDeclsLayout NestedTypeDeclsTable(Out);
      writeNestedTypeDeclsTable(NestedTypeDeclsTable, nestedTypeDecls);
    }

    if (entryPointClassID.hasValue()) {
      index_block::EntryPointLayout EntryPoint(Out);
      EntryPoint.emit(ScratchRecord, entryPointClassID.getValue());
    }

    {
      // Write sub-tables to a skippable sub-block.
      BCBlockRAII restoreBlock(Out, DECL_MEMBER_TABLES_BLOCK_ID, 4);
      decl_member_tables_block::DeclMembersLayout DeclMembersTable(Out);
      for (auto &entry : DeclMemberNames) {
        // Save BitOffset we're writing sub-table to.
        static_assert(bitOffsetFitsIn32Bits(), "BitOffset too large");
        assert(Out.GetCurrentBitNo() < (1ull << 32));
        entry.second.first = Out.GetCurrentBitNo();
        // Write sub-table.
        writeDeclMembersTable(DeclMembersTable, *entry.second.second);
      }
    }
    // Write top-level table mapping names to sub-tables.
    index_block::DeclMemberNamesLayout DeclMemberNamesTable(Out);
    writeDeclMemberNamesTable(DeclMemberNamesTable, DeclMemberNames);
  }
}

void SerializerBase::writeToStream(raw_ostream &os) {
  os.write(Buffer.data(), Buffer.size());
  os.flush();
}

SerializerBase::SerializerBase(ArrayRef<unsigned char> signature,
                               ModuleOrSourceFile DC) {
  for (unsigned char byte : signature)
    Out.Emit(byte, 8);

  this->M = getModule(DC);
  this->SF = DC.dyn_cast<SourceFile *>();
}

void Serializer::writeToStream(raw_ostream &os, ModuleOrSourceFile DC,
                               const SILModule *SILMod,
                               const SerializationOptions &options) {
  Serializer S{SWIFTMODULE_SIGNATURE, DC};

  // FIXME: This is only really needed for debugging. We don't actually use it.
  S.writeBlockInfoBlock();

  {
    BCBlockRAII moduleBlock(S.Out, MODULE_BLOCK_ID, 2);
    S.writeHeader(options);
    S.writeInputBlock(options);
    S.writeSIL(SILMod, options.SerializeAllSIL);
    S.writeAST(DC, options.EnableNestedTypeLookupTable);
  }

  S.writeToStream(os);
}

void swift::serialize(ModuleOrSourceFile DC,
                      const SerializationOptions &options,
                      const SILModule *M) {
  assert(options.OutputPath && options.OutputPath[0] != '\0');

  if (strcmp("-", options.OutputPath) == 0) {
    // Special-case writing to stdout.
    Serializer::writeToStream(llvm::outs(), DC, M, options);
    assert(!options.DocOutputPath || options.DocOutputPath[0] == '\0');
    return;
  }

  bool hadError = withOutputFile(getContext(DC).Diags,
                                 options.OutputPath,
                                 [&](raw_ostream &out) {
    SharedTimer timer("Serialization, swiftmodule");
    Serializer::writeToStream(out, DC, M, options);
    return false;
  });
  if (hadError)
    return;

  if (options.DocOutputPath && options.DocOutputPath[0] != '\0') {
    (void)withOutputFile(getContext(DC).Diags,
                         options.DocOutputPath,
                         [&](raw_ostream &out) {
      SharedTimer timer("Serialization, swiftdoc");
      writeDocToStream(out, DC, options.GroupInfoPath);
      return false;
    });
  }
}
