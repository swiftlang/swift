//===--- SerializeDoc.cpp - Read and write swiftdoc files -----------------===//
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

#include "DocFormat.h"
#include "Serialization.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/Module.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/Support/DJB.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/OnDiskHashTable.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/YAMLParser.h"

#include <vector>

using namespace swift;
using namespace swift::serialization;
using namespace llvm::support;
using swift::version::Version;
using llvm::BCBlockRAII;

using FileNameToGroupNameMap = llvm::StringMap<std::string>;
using pFileNameToGroupNameMap = std::unique_ptr<FileNameToGroupNameMap>;

namespace {
class YamlGroupInputParser {
  StringRef RecordPath;
  static constexpr const char * const Separator = "/";

  // FIXME: This isn't thread-safe.
  static llvm::StringMap<pFileNameToGroupNameMap> AllMaps;

  bool parseRoot(FileNameToGroupNameMap &Map, llvm::yaml::Node *Root,
                 StringRef ParentName) {
    auto *MapNode = dyn_cast<llvm::yaml::MappingNode>(Root);
    if (!MapNode) {
      return true;
    }
    for (auto &Pair : *MapNode) {
      auto *Key = dyn_cast_or_null<llvm::yaml::ScalarNode>(Pair.getKey());
      auto *Value = dyn_cast_or_null<llvm::yaml::SequenceNode>(Pair.getValue());

      if (!Key || !Value) {
        return true;
      }
      llvm::SmallString<16> GroupNameStorage;
      StringRef GroupName = Key->getValue(GroupNameStorage);
      std::string CombinedName;
      if (!ParentName.empty()) {
        CombinedName = (llvm::Twine(ParentName) + Separator + GroupName).str();
      } else {
        CombinedName = GroupName;
      }

      for (llvm::yaml::Node &Entry : *Value) {
        if (auto *FileEntry= dyn_cast<llvm::yaml::ScalarNode>(&Entry)) {
          llvm::SmallString<16> FileNameStorage;
          StringRef FileName = FileEntry->getValue(FileNameStorage);
          llvm::SmallString<32> GroupNameAndFileName;
          GroupNameAndFileName.append(CombinedName);
          GroupNameAndFileName.append(Separator);
          GroupNameAndFileName.append(llvm::sys::path::stem(FileName));
          Map[FileName] = GroupNameAndFileName.str();
        } else if (Entry.getType() == llvm::yaml::Node::NodeKind::NK_Mapping) {
          if (parseRoot(Map, &Entry, CombinedName))
            return true;
        } else
          return true;
      }
    }
    return false;
  }

public:
  YamlGroupInputParser(StringRef RecordPath): RecordPath(RecordPath) {}

  FileNameToGroupNameMap* getParsedMap() {
    return AllMaps[RecordPath].get();
  }

  // Parse the Yaml file that contains the group information.
  // True on failure; false on success.
  bool parse() {
    // If we have already parsed this group info file, return false;
    auto FindMap = AllMaps.find(RecordPath);
    if (FindMap != AllMaps.end())
      return false;

    auto Buffer = llvm::MemoryBuffer::getFile(RecordPath);
    if (!Buffer) {
      // The group info file does not exist.
      return true;
    }
    llvm::SourceMgr SM;
    llvm::yaml::Stream YAMLStream(Buffer.get()->getMemBufferRef(), SM);
    llvm::yaml::document_iterator I = YAMLStream.begin();
    if (I == YAMLStream.end()) {
      // Cannot parse correctly.
      return true;
    }
    llvm::yaml::Node *Root = I->getRoot();
    if (!Root) {
      // Cannot parse correctly.
      return true;
    }

    // The format is a map of ("group0" : ["file1", "file2"]), meaning all
    // symbols from file1 and file2 belong to "group0".
    auto *Map = dyn_cast<llvm::yaml::MappingNode>(Root);
    if (!Map) {
      return true;
    }
    pFileNameToGroupNameMap pMap(new FileNameToGroupNameMap());
    std::string Empty;
    if (parseRoot(*pMap, Root, Empty))
      return true;

    // Save the parsed map to the owner.
    AllMaps[RecordPath] = std::move(pMap);
    return false;
  }
};

llvm::StringMap<pFileNameToGroupNameMap> YamlGroupInputParser::AllMaps;

class DeclGroupNameContext {
  struct GroupNameCollector {
    static const StringLiteral NullGroupName;
    const bool Enable;
    GroupNameCollector(bool Enable) : Enable(Enable) {}
    virtual ~GroupNameCollector() = default;
    virtual StringRef getGroupNameInternal(const Decl *VD) = 0;
    StringRef getGroupName(const Decl *VD) {
      return Enable ? getGroupNameInternal(VD) : StringRef(NullGroupName);
    };
  };

  class GroupNameCollectorFromJson : public GroupNameCollector {
    StringRef RecordPath;
    FileNameToGroupNameMap* pMap = nullptr;
    ASTContext &Ctx;

  public:
    GroupNameCollectorFromJson(StringRef RecordPath, ASTContext &Ctx) :
      GroupNameCollector(!RecordPath.empty()), RecordPath(RecordPath),
      Ctx(Ctx) {}
    StringRef getGroupNameInternal(const Decl *VD) override {
      // We need the file path, so there has to be a location.
      if (VD->getLoc().isInvalid())
        return NullGroupName;
      auto PathOp = VD->getDeclContext()->getParentSourceFile()->getBufferID();
      if (!PathOp.hasValue())
        return NullGroupName;
      StringRef FullPath =
          Ctx.SourceMgr.getIdentifierForBuffer(PathOp.getValue());
      if (!pMap) {
        YamlGroupInputParser Parser(RecordPath);
        if (!Parser.parse()) {

          // Get the file-name to group map if parsing correctly.
          pMap = Parser.getParsedMap();
        }
      }
      if (!pMap)
        return NullGroupName;
      StringRef FileName = llvm::sys::path::filename(FullPath);
      auto Found = pMap->find(FileName);
      if (Found == pMap->end()) {
        Ctx.Diags.diagnose(SourceLoc(), diag::error_no_group_info, FileName);
        return NullGroupName;
      }
      return Found->second;
    }
  };

  llvm::MapVector<StringRef, unsigned> Map;
  std::vector<StringRef> ViewBuffer;
  std::unique_ptr<GroupNameCollector> pNameCollector;

public:
  DeclGroupNameContext(StringRef RecordPath, ASTContext &Ctx) :
    pNameCollector(new GroupNameCollectorFromJson(RecordPath, Ctx)) {}
  uint32_t getGroupSequence(const Decl *VD) {
    return Map.insert(std::make_pair(pNameCollector->getGroupName(VD),
                                     Map.size())).first->second;
  }

  ArrayRef<StringRef> getOrderedGroupNames() {
    ViewBuffer.clear();
    for (auto It = Map.begin(); It != Map.end(); ++ It) {
      ViewBuffer.push_back(It->first);
    }
    return llvm::makeArrayRef(ViewBuffer);
  }

  bool isEnable() {
    return pNameCollector->Enable;
  }
};

const StringLiteral
DeclGroupNameContext::GroupNameCollector::NullGroupName = "";

struct DeclCommentTableData {
  StringRef Brief;
  RawComment Raw;
  uint32_t Group;
  uint32_t Order;
};

class DeclCommentTableInfo {
public:
  using key_type = StringRef;
  using key_type_ref = key_type;
  using data_type = DeclCommentTableData;
  using data_type_ref = const data_type &;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  hash_value_type ComputeHash(key_type_ref key) {
    assert(!key.empty());
    // FIXME: DJB seed=0, audit whether the default seed could be used.
    return llvm::djbHash(key, 0);
  }

  std::pair<unsigned, unsigned>
  EmitKeyDataLength(raw_ostream &out, key_type_ref key, data_type_ref data) {
    uint32_t keyLength = key.size();
    const unsigned numLen = 4;

    // Data consists of brief comment length and brief comment text,
    uint32_t dataLength = numLen + data.Brief.size();
    // number of raw comments,
    dataLength += numLen;
    // for each raw comment: column number of the first line, length of each
    // raw comment and its text.
    for (auto C : data.Raw.Comments)
      dataLength += numLen + numLen + C.RawText.size();

    // Group Id.
    dataLength += numLen;

    // Source order.
    dataLength += numLen;
    endian::Writer writer(out, little);
    writer.write<uint32_t>(keyLength);
    writer.write<uint32_t>(dataLength);
    return { keyLength, dataLength };
  }

  void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
    out << key;
  }

  void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                unsigned len) {
    endian::Writer writer(out, little);
    writer.write<uint32_t>(data.Brief.size());
    out << data.Brief;
    writer.write<uint32_t>(data.Raw.Comments.size());
    for (auto C : data.Raw.Comments) {
      writer.write<uint32_t>(C.StartColumn);
      writer.write<uint32_t>(C.RawText.size());
      out << C.RawText;
    }
    writer.write<uint32_t>(data.Group);
    writer.write<uint32_t>(data.Order);
  }
};

class DocSerializer : public SerializerBase {
public:
  using SerializerBase::SerializerBase;
  using SerializerBase::writeToStream;

  using SerializerBase::Out;
  using SerializerBase::M;
  using SerializerBase::SF;

  /// Writes the BLOCKINFO block for the module documentation file.
  void writeDocBlockInfoBlock() {
    BCBlockRAII restoreBlock(Out, llvm::bitc::BLOCKINFO_BLOCK_ID, 2);

    SmallVector<unsigned char, 64> nameBuffer;
#define BLOCK(X) emitBlockID(X ## _ID, #X, nameBuffer)
#define BLOCK_RECORD(K, X) emitRecordID(K::X, #X, nameBuffer)

    BLOCK(MODULE_DOC_BLOCK);

    BLOCK(CONTROL_BLOCK);
    BLOCK_RECORD(control_block, METADATA);
    BLOCK_RECORD(control_block, MODULE_NAME);
    BLOCK_RECORD(control_block, TARGET);

    BLOCK(COMMENT_BLOCK);
    BLOCK_RECORD(comment_block, DECL_COMMENTS);
    BLOCK_RECORD(comment_block, GROUP_NAMES);

#undef BLOCK
#undef BLOCK_RECORD
  }

  /// Writes the Swift doc module file header and name.
  void writeDocHeader();
};

} // end anonymous namespace

static void writeGroupNames(const comment_block::GroupNamesLayout &GroupNames,
                            ArrayRef<StringRef> Names) {
  llvm::SmallString<32> Blob;
  llvm::raw_svector_ostream BlobStream(Blob);
  endian::Writer Writer(BlobStream, little);
  Writer.write<uint32_t>(Names.size());
  for (auto N : Names) {
    Writer.write<uint32_t>(N.size());
    BlobStream << N;
  }
  SmallVector<uint64_t, 8> Scratch;
  GroupNames.emit(Scratch, BlobStream.str());
}

static void writeDeclCommentTable(
    const comment_block::DeclCommentListLayout &DeclCommentList,
    const SourceFile *SF, const ModuleDecl *M,
    DeclGroupNameContext &GroupContext) {

  struct DeclCommentTableWriter : public ASTWalker {
    llvm::BumpPtrAllocator Arena;
    llvm::SmallString<512> USRBuffer;
    llvm::OnDiskChainedHashTableGenerator<DeclCommentTableInfo> generator;
    DeclGroupNameContext &GroupContext;
    unsigned SourceOrder;

    DeclCommentTableWriter(DeclGroupNameContext &GroupContext) :
      GroupContext(GroupContext) {}

    void resetSourceOrder() {
      SourceOrder = 0;
    }

    StringRef copyString(StringRef String) {
      char *Mem = static_cast<char *>(Arena.Allocate(String.size(), 1));
      std::copy(String.begin(), String.end(), Mem);
      return StringRef(Mem, String.size());
    }

    void writeDocForExtensionDecl(ExtensionDecl *ED) {
      RawComment Raw = ED->getRawComment();
      if (Raw.Comments.empty() && !GroupContext.isEnable())
        return;
      // Compute USR.
      {
        USRBuffer.clear();
        llvm::raw_svector_ostream OS(USRBuffer);
        if (ide::printExtensionUSR(ED, OS))
          return;
      }
      generator.insert(copyString(USRBuffer.str()),
                       { ED->getBriefComment(), Raw,
                         GroupContext.getGroupSequence(ED),
                         SourceOrder++ });
    }

    bool walkToDeclPre(Decl *D) override {
      if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
        writeDocForExtensionDecl(ED);
        return true;
      }

      auto *VD = dyn_cast<ValueDecl>(D);
      if (!VD)
        return true;

      RawComment Raw = VD->getRawComment();
      // When building the stdlib we intend to serialize unusual comments.
      // This situation is represented by GroupContext.isEnable().  In that
      // case, we perform fewer serialization checks.
      if (!GroupContext.isEnable()) {
        // Skip the decl if it cannot have a comment.
        if (!VD->canHaveComment()) {
          return true;
        }

        // Skip the decl if it does not have a comment.
        if (Raw.Comments.empty())
          return true;

        // Skip the decl if it's not visible to clients. The use of
        // getEffectiveAccess is unusual here; we want to take the testability
        // state into account and emit documentation if and only if they are
        // visible to clients (which means public ordinarily, but
        // public+internal when testing enabled).
        if (VD->getEffectiveAccess() < swift::AccessLevel::Public)
          return true;
      }

      // Compute USR.
      {
        USRBuffer.clear();
        llvm::raw_svector_ostream OS(USRBuffer);
        if (ide::printDeclUSR(VD, OS))
          return true;
      }

      generator.insert(copyString(USRBuffer.str()),
                       { VD->getBriefComment(), Raw,
                         GroupContext.getGroupSequence(VD),
                         SourceOrder++ });
      return true;
    }

    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      return { false, S };
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      return { false, E };
    }

    bool walkToTypeLocPre(TypeLoc &TL) override { return false; }
    bool walkToTypeReprPre(TypeRepr *T) override { return false; }
    bool walkToParameterListPre(ParameterList *PL) override { return false; }
  };

  DeclCommentTableWriter Writer(GroupContext);

  ArrayRef<const FileUnit *> files;
  SmallVector<const FileUnit *, 1> Scratch;
  if (SF) {
    Scratch.push_back(SF);
    files = llvm::makeArrayRef(Scratch);
  } else {
    files = M->getFiles();
  }
  for (auto nextFile : files) {
    Writer.resetSourceOrder();
    const_cast<FileUnit *>(nextFile)->walk(Writer);
  }
  SmallVector<uint64_t, 8> scratch;
  llvm::SmallString<32> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    endian::write<uint32_t>(blobStream, 0, little);
    tableOffset = Writer.generator.Emit(blobStream);
  }

  DeclCommentList.emit(scratch, tableOffset, hashTableBlob);
}

void DocSerializer::writeDocHeader() {
  {
    BCBlockRAII restoreBlock(Out, CONTROL_BLOCK_ID, 3);
    control_block::ModuleNameLayout ModuleName(Out);
    control_block::MetadataLayout Metadata(Out);
    control_block::TargetLayout Target(Out);

    auto& LangOpts = M->getASTContext().LangOpts;
    Metadata.emit(ScratchRecord,
                  SWIFTMODULE_VERSION_MAJOR, SWIFTMODULE_VERSION_MINOR,
                  /*short version string length*/0, /*compatibility length*/0,
                  version::getSwiftFullVersion(
                    LangOpts.EffectiveLanguageVersion));

    ModuleName.emit(ScratchRecord, M->getName().str());
    Target.emit(ScratchRecord, LangOpts.Target.str());
  }
}

void serialization::writeDocToStream(raw_ostream &os, ModuleOrSourceFile DC,
                                     StringRef GroupInfoPath) {
  DocSerializer S{SWIFTDOC_SIGNATURE, DC};
  // FIXME: This is only really needed for debugging. We don't actually use it.
  S.writeDocBlockInfoBlock();

  {
    BCBlockRAII moduleBlock(S.Out, MODULE_DOC_BLOCK_ID, 2);
    S.writeDocHeader();
    {
      BCBlockRAII restoreBlock(S.Out, COMMENT_BLOCK_ID, 4);
      DeclGroupNameContext GroupContext(GroupInfoPath, S.M->getASTContext());
      comment_block::DeclCommentListLayout DeclCommentList(S.Out);
      writeDeclCommentTable(DeclCommentList, S.SF, S.M, GroupContext);
      comment_block::GroupNamesLayout GroupNames(S.Out);

      // FIXME: Multi-file compilation may cause group id collision.
      writeGroupNames(GroupNames, GroupContext.getOrderedGroupNames());
    }
  }

  S.writeToStream(os);
}
