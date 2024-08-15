//===--- ModuleFileCoreTableInfo.h - Hash table info structures -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_MODULEFILECORETABLEINFO_H
#define SWIFT_SERIALIZATION_MODULEFILECORETABLEINFO_H

#include "ModuleFileSharedCore.h"
#include "DocFormat.h"
#include "SourceInfoFormat.h"
#include "swift/AST/RawComment.h"
#include "llvm/Support/DJB.h"

namespace swift {

/// Used to deserialize entries in the on-disk decl hash table.
class ModuleFileSharedCore::DeclTableInfo {
public:
  using internal_key_type = std::pair<DeclBaseName::Kind, StringRef>;
  using external_key_type = DeclBaseName;
  using data_type = SmallVector<std::pair<uint8_t, DeclID>, 8>;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) {
    if (ID.getKind() == DeclBaseName::Kind::Normal) {
      return {DeclBaseName::Kind::Normal, ID.getIdentifier().str()};
    } else {
      return {ID.getKind(), StringRef()};
    }
  }

  hash_value_type ComputeHash(internal_key_type key) {
    if (key.first == DeclBaseName::Kind::Normal) {
      return llvm::djbHash(key.second, serialization::SWIFTMODULE_HASH_SEED);
    } else {
      return (hash_value_type)key.first;
    }
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    using namespace llvm::support;
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    unsigned dataLength = endian::readNext<uint16_t, little, unaligned>(data);
    return { keyLength, dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    using namespace swift::serialization;
    using namespace llvm::support;
    uint8_t kind = endian::readNext<uint8_t, little, unaligned>(data);
    switch (kind) {
    case static_cast<uint8_t>(DeclNameKind::Normal): {
      StringRef str(reinterpret_cast<const char *>(data),
                    length - sizeof(uint8_t));
      return {DeclBaseName::Kind::Normal, str};
    }
    case static_cast<uint8_t>(DeclNameKind::Subscript):
      return {DeclBaseName::Kind::Subscript, StringRef()};
    case static_cast<uint8_t>(DeclNameKind::Destructor):
      return {DeclBaseName::Kind::Destructor, StringRef()};
    default:
      llvm_unreachable("Unknown DeclNameKind");
    }
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    using namespace llvm::support;
    data_type result;
    while (length > 0) {
      uint8_t kind = *data++;
      DeclID offset = endian::readNext<uint32_t, little, unaligned>(data);
      result.push_back({ kind, offset });
      length -= 5;
    }

    return result;
  }
};

/// Used to deserialize entries in the on-disk decl hash table.
class ModuleFileSharedCore::ExtensionTableInfo {
  const ModuleFileSharedCore &Core;
public:
  using internal_key_type = StringRef;
  using external_key_type = Identifier;
  using data_type = SmallVector<std::pair<StringRef, DeclID>, 8>;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) {
    return ID.str();
  }

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::djbHash(key, serialization::SWIFTMODULE_HASH_SEED);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    using namespace llvm::support;
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    unsigned dataLength = endian::readNext<uint16_t, little, unaligned>(data);
    return { keyLength, dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char *>(data), length);
  }

  data_type ReadData(internal_key_type key, const uint8_t *data,
                     unsigned length) {
    using namespace llvm::support;
    data_type result;
    const uint8_t *limit = data + length;
    while (data < limit) {
      DeclID offset = endian::readNext<uint32_t, little, unaligned>(data);

      int32_t nameIDOrLength =
          endian::readNext<int32_t, little, unaligned>(data);
      StringRef moduleNameOrMangledBase;
      if (nameIDOrLength < 0) {
        StringRef moduleName = Core.getModuleNameFromID(-nameIDOrLength);
        if (!moduleName.empty())
          moduleNameOrMangledBase = moduleName;
      } else {
        moduleNameOrMangledBase =
            StringRef(reinterpret_cast<const char *>(data), nameIDOrLength);
        data += nameIDOrLength;
      }

      result.push_back({ moduleNameOrMangledBase, offset });
    }

    return result;
  }

  explicit ExtensionTableInfo(const ModuleFileSharedCore &core) : Core(core) {}
};

/// Used to deserialize entries in the on-disk decl hash table.
class ModuleFileSharedCore::LocalDeclTableInfo {
public:
  using internal_key_type = StringRef;
  using external_key_type = internal_key_type;
  using data_type = DeclID;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) {
    return ID;
  }

  external_key_type GetExternalKey(internal_key_type ID) {
    return ID;
  }

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::djbHash(key, serialization::SWIFTMODULE_HASH_SEED);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    using namespace llvm::support;
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    return { keyLength, sizeof(uint32_t) };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char *>(data), length);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    using namespace llvm::support;
    return endian::readNext<uint32_t, little, unaligned>(data);
  }
};

class ModuleFileSharedCore::NestedTypeDeclsTableInfo {
public:
  using internal_key_type = StringRef;
  using external_key_type = Identifier;
  using data_type = SmallVector<std::pair<DeclID, DeclID>, 4>;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) {
    return ID.str();
  }

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::djbHash(key, serialization::SWIFTMODULE_HASH_SEED);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    using namespace llvm::support;
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    unsigned dataLength = endian::readNext<uint16_t, little, unaligned>(data);
    return { keyLength, dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char *>(data), length);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    using namespace llvm::support;
    data_type result;
    while (length > 0) {
      DeclID parentID = endian::readNext<uint32_t, little, unaligned>(data);
      DeclID childID = endian::readNext<uint32_t, little, unaligned>(data);
      result.push_back({ parentID, childID });
      length -= sizeof(uint32_t) * 2;
    }

    return result;
  }
};

// Indexing the members of all Decls (well, NominalTypeDecls anyway) is
// accomplished by a 2-level hashtable scheme. The outer table here maps from
// DeclBaseNames to serialization::BitOffsets of sub-tables. The sub-tables --
// SerializedDeclMembersTables, one table per name -- map from the enclosing
// (NominalTypeDecl) DeclID to a vector of DeclIDs of members of the nominal
// with the name. See DeclMembersTableInfo below.
class ModuleFileSharedCore::DeclMemberNamesTableInfo {
public:
  using internal_key_type = std::pair<DeclBaseName::Kind, StringRef>;
  using external_key_type = DeclBaseName;
  using data_type = serialization::BitOffset;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) {
    if (ID.getKind() == DeclBaseName::Kind::Normal) {
      return {DeclBaseName::Kind::Normal, ID.getIdentifier().str()};
    } else {
      return {ID.getKind(), StringRef()};
    }
  }

  hash_value_type ComputeHash(internal_key_type key) {
    if (key.first == DeclBaseName::Kind::Normal) {
      return llvm::djbHash(key.second, serialization::SWIFTMODULE_HASH_SEED);
    } else {
      return (hash_value_type)key.first;
    }
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    using namespace llvm::support;
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    return { keyLength, sizeof(uint32_t) };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    using namespace swift::serialization;
    using namespace llvm::support;
    uint8_t kind = endian::readNext<uint8_t, little, unaligned>(data);
    switch (kind) {
    case static_cast<uint8_t>(DeclNameKind::Normal): {
      StringRef str(reinterpret_cast<const char *>(data),
                    length - sizeof(uint8_t));
      return {DeclBaseName::Kind::Normal, str};
    }
    case static_cast<uint8_t>(DeclNameKind::Subscript):
      return {DeclBaseName::Kind::Subscript, StringRef()};
    case static_cast<uint8_t>(DeclNameKind::Destructor):
      return {DeclBaseName::Kind::Destructor, StringRef()};
    case static_cast<uint8_t>(DeclNameKind::Constructor):
      return {DeclBaseName::Kind::Constructor, StringRef()};
    default:
      llvm_unreachable("Unknown DeclNameKind");
    }
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    using namespace llvm::support;
    assert(length == sizeof(uint32_t));
    return endian::readNext<uint32_t, little, unaligned>(data);
  }
};

// Second half of the 2-level member name lookup scheme, see
// DeclMemberNamesTableInfo above. There is one of these tables for each member
// DeclBaseName N in the module, and it maps from enclosing DeclIDs (say: each
// NominalTypeDecl T that has members named N) to the set of N-named members of
// T. In other words, there are no names in this table: the names are one level
// up, this table just maps { Owner-DeclID => [Member-DeclID, ...] }.
class ModuleFileSharedCore::DeclMembersTableInfo {
public:
  using internal_key_type = uint32_t;
  using external_key_type = DeclID;
  using data_type = SmallVector<DeclID, 2>;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) {
    return ID;
  }

  hash_value_type ComputeHash(internal_key_type key) {
    return key;
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    using namespace llvm::support;
    unsigned dataLength = endian::readNext<uint16_t, little, unaligned>(data);
    return { sizeof(uint32_t), dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    using namespace llvm::support;
    return endian::readNext<uint32_t, little, unaligned>(data);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    using namespace llvm::support;
    data_type result;
    while (length > 0) {
      DeclID declID = endian::readNext<uint32_t, little, unaligned>(data);
      result.push_back(declID);
      length -= sizeof(uint32_t);
    }
    return result;
  }
};

/// Used to deserialize entries in the on-disk Objective-C method table.
class ModuleFileSharedCore::ObjCMethodTableInfo {
public:
  using internal_key_type = std::string;
  using external_key_type = ObjCSelector;
  using data_type = SmallVector<std::tuple<std::string, bool, DeclID>, 8>;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) {
    llvm::SmallString<32> scratch;
    return ID.getString(scratch).str();
  }

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::djbHash(key, serialization::SWIFTMODULE_HASH_SEED);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    using namespace llvm::support;
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    unsigned dataLength = endian::readNext<uint32_t, little, unaligned>(data);
    return { keyLength, dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return std::string(reinterpret_cast<const char *>(data), length);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    using namespace llvm::support;
    const constexpr auto recordSize = sizeof(uint32_t) + 1 + sizeof(uint32_t);
    data_type result;
    while (length > 0) {
      unsigned ownerLen = endian::readNext<uint32_t, little, unaligned>(data);
      bool isInstanceMethod = *data++ != 0;
      DeclID methodID = endian::readNext<uint32_t, little, unaligned>(data);
      std::string ownerName((const char *)data, ownerLen);
      result.push_back(
        std::make_tuple(std::move(ownerName), isInstanceMethod, methodID));
      data += ownerLen;
      length -= (recordSize + ownerLen);
    }

    return result;
  }
};

/// Used to deserialize entries in the on-disk derivative function configuration
/// table.
class ModuleFileSharedCore::DerivativeFunctionConfigTableInfo {
public:
  using internal_key_type = StringRef;
  using external_key_type = internal_key_type;
  using data_type =
      SmallVector<std::pair<std::string, serialization::GenericSignatureID>, 8>;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  external_key_type GetExternalKey(internal_key_type ID) { return ID; }

  internal_key_type GetInternalKey(external_key_type ID) { return ID; }

  hash_value_type ComputeHash(internal_key_type key) {
    return llvm::djbHash(key, serialization::SWIFTMODULE_HASH_SEED);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    using namespace llvm::support;
    unsigned keyLength = endian::readNext<uint16_t, little, unaligned>(data);
    unsigned dataLength = endian::readNext<uint16_t, little, unaligned>(data);
    return {keyLength, dataLength};
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char *>(data), length);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    using namespace llvm::support;
    data_type result;
    const uint8_t *limit = data + length;
    while (data < limit) {
      DeclID genSigId = endian::readNext<uint32_t, little, unaligned>(data);
      int32_t nameLength = endian::readNext<int32_t, little, unaligned>(data);
      std::string mangledName(reinterpret_cast<const char *>(data), nameLength);
      data += nameLength;
      result.push_back({std::string(mangledName), genSigId});
    }
    return result;
  }
};

struct ModuleFileSharedCore::DeserializedCommentInfo {
  SmallVector<SingleRawComment, 2> RawCommentStore;
  CommentInfo Info;
};

class ModuleFileSharedCore::DeclCommentTableInfo {
public:
  using internal_key_type = StringRef;
  using external_key_type = StringRef;
  using data_type = std::unique_ptr<DeserializedCommentInfo>;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type key) {
    return key;
  }

  hash_value_type ComputeHash(internal_key_type key) {
    assert(!key.empty());
    return llvm::djbHash(key, serialization::SWIFTDOC_HASH_SEED_5_1);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    using namespace llvm::support;
    unsigned keyLength = endian::readNext<uint32_t, little, unaligned>(data);
    unsigned dataLength = endian::readNext<uint32_t, little, unaligned>(data);
    return { keyLength, dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char *>(data), length);
  }

  data_type ReadData(internal_key_type key, const uint8_t *data,
                     unsigned length) {
    using namespace llvm::support;
    data_type result = std::make_unique<DeserializedCommentInfo>();

    {
      unsigned BriefSize = endian::readNext<uint32_t, little, unaligned>(data);
      result->Info.Brief = StringRef(reinterpret_cast<const char *>(data), BriefSize);
      data += BriefSize;
    }

    unsigned NumComments = endian::readNext<uint32_t, little, unaligned>(data);

    for (unsigned i = 0; i != NumComments; ++i) {
      unsigned StartColumn =
          endian::readNext<uint32_t, little, unaligned>(data);
      unsigned RawSize = endian::readNext<uint32_t, little, unaligned>(data);
      auto RawText = StringRef(reinterpret_cast<const char *>(data), RawSize);
      data += RawSize;

      result->RawCommentStore.emplace_back(RawText, StartColumn);
    }
    result->Info.Raw = RawComment(result->RawCommentStore);
    result->Info.Group = endian::readNext<uint32_t, little, unaligned>(data);
    result->Info.SourceOrder = endian::readNext<uint32_t, little, unaligned>(data);
    return result;
  }
};

class ModuleFileSharedCore::DeclUSRTableInfo {
public:
  using internal_key_type = StringRef;
  using external_key_type = StringRef;
  using data_type = uint32_t;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type key) { return key; }

  hash_value_type ComputeHash(internal_key_type key) {
    assert(!key.empty());
    return llvm::djbHash(key, serialization::SWIFTSOURCEINFO_HASH_SEED);
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    using namespace llvm::support;
    unsigned keyLength = endian::readNext<uint32_t, little, unaligned>(data);
    unsigned dataLength = 4;
    return { keyLength, dataLength };
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    return StringRef(reinterpret_cast<const char*>(data), length);
  }

  data_type ReadData(internal_key_type key, const uint8_t *data, unsigned length) {
    using namespace llvm::support;
    assert(length == 4);
    return endian::readNext<uint32_t, little, unaligned>(data);
  }
};

/// Serialization for the table mapping module-level declIDs for serialized
/// iterable decl contexts to their corresponding \c Fingerprint values.
class ModuleFileSharedCore::DeclFingerprintsTableInfo {
public:
  using internal_key_type = uint32_t;
  using external_key_type = DeclID;
  using data_type = swift::Fingerprint;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  internal_key_type GetInternalKey(external_key_type ID) { return ID; }

  hash_value_type ComputeHash(internal_key_type key) {
    return key;
  }

  static bool EqualKey(internal_key_type lhs, internal_key_type rhs) {
    return lhs == rhs;
  }

  static std::pair<unsigned, unsigned> ReadKeyDataLength(const uint8_t *&data) {
    using namespace llvm::support;
    const unsigned dataLen = Fingerprint::DIGEST_LENGTH;
    return {sizeof(uint32_t), dataLen};
  }

  static internal_key_type ReadKey(const uint8_t *data, unsigned length) {
    using namespace llvm::support;
    return endian::readNext<uint32_t, little, unaligned>(data);
  }

  static data_type ReadData(internal_key_type key, const uint8_t *data,
                            unsigned length) {
    using namespace llvm::support;
    auto str = llvm::StringRef{reinterpret_cast<const char *>(data),
                               Fingerprint::DIGEST_LENGTH};
    if (auto fp = Fingerprint::fromString(str))
      return fp.value();
    llvm::errs() << "Unconvertable fingerprint '" << str << "'\n";
    abort();
  }
};

} // end namespace swift

#endif
