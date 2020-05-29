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
#include "SourceInfoFormat.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/StringSet.h"
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

namespace {
class YamlGroupInputParser {
  ASTContext &Ctx;
  StringRef RecordPath;
  static constexpr const char * const Separator = "/";

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
        CombinedName = GroupName.str();
      }

      for (llvm::yaml::Node &Entry : *Value) {
        if (auto *FileEntry= dyn_cast<llvm::yaml::ScalarNode>(&Entry)) {
          llvm::SmallString<16> FileNameStorage;
          StringRef FileName = FileEntry->getValue(FileNameStorage);
          llvm::SmallString<32> GroupNameAndFileName;
          GroupNameAndFileName.append(CombinedName);
          GroupNameAndFileName.append(Separator);
          GroupNameAndFileName.append(llvm::sys::path::stem(FileName));
          Map[FileName] = std::string(GroupNameAndFileName.str());
        } else if (Entry.getType() == llvm::yaml::Node::NodeKind::NK_Mapping) {
          if (parseRoot(Map, &Entry, CombinedName))
            return true;
        } else
          return true;
      }
    }
    return false;
  }

  FileNameToGroupNameMap diagnoseGroupInfoFile(bool FileMissing = false) {
    Ctx.Diags.diagnose(SourceLoc(),
      FileMissing ? diag::cannot_find_group_info_file:
      diag::cannot_parse_group_info_file, RecordPath);
    return {};
  }

public:
  YamlGroupInputParser(ASTContext &Ctx, StringRef RecordPath):
    Ctx(Ctx), RecordPath(RecordPath) {}

  /// Parse the Yaml file that contains the group information.
  ///
  /// If the record path is empty, returns an empty map.
  FileNameToGroupNameMap parse() {
    if (RecordPath.empty())
      return {};

    auto Buffer = llvm::MemoryBuffer::getFile(RecordPath);
    if (!Buffer) {
      return diagnoseGroupInfoFile(/*Missing File*/true);
    }
    llvm::SourceMgr SM;
    llvm::yaml::Stream YAMLStream(Buffer.get()->getMemBufferRef(), SM);
    llvm::yaml::document_iterator I = YAMLStream.begin();
    if (I == YAMLStream.end()) {
      // Cannot parse correctly.
      return diagnoseGroupInfoFile();
    }
    llvm::yaml::Node *Root = I->getRoot();
    if (!Root) {
      // Cannot parse correctly.
      return diagnoseGroupInfoFile();
    }

    // The format is a map of ("group0" : ["file1", "file2"]), meaning all
    // symbols from file1 and file2 belong to "group0".
    auto *Map = dyn_cast<llvm::yaml::MappingNode>(Root);
    if (!Map) {
      return diagnoseGroupInfoFile();
    }
    FileNameToGroupNameMap Result;
    if (parseRoot(Result, Root, ""))
      return diagnoseGroupInfoFile();

    // Return the parsed map.
    return Result;
  }
};

class DeclGroupNameContext {
  ASTContext &Ctx;
  FileNameToGroupNameMap FileToGroupMap;
  llvm::MapVector<StringRef, unsigned> Map;
  std::vector<StringRef> ViewBuffer;

public:
  DeclGroupNameContext(StringRef RecordPath, ASTContext &Ctx) :
    Ctx(Ctx), FileToGroupMap(YamlGroupInputParser(Ctx, RecordPath).parse()) {}

  uint32_t getGroupSequence(const Decl *VD) {
    if (FileToGroupMap.empty())
      return 0;

    // We need the file path, so there has to be a location.
    if (VD->getLoc().isInvalid())
      return 0;

    // If the decl being serialized isn't actually from a source file, don't
    // put it in a group.
    // FIXME: How do we preserve group information through partial module
    // merging for multi-frontend builds, then?
    SourceFile *SF = VD->getDeclContext()->getParentSourceFile();
    if (!SF)
      return 0;

    StringRef FullPath = SF->getFilename();
    if (FullPath.empty())
      return 0;
    StringRef FileName = llvm::sys::path::filename(FullPath);
    auto Found = FileToGroupMap.find(FileName);
    if (Found == FileToGroupMap.end()) {
      Ctx.Diags.diagnose(SourceLoc(), diag::error_no_group_info, FileName);
      return 0;
    }

    StringRef GroupName = Found->second;
    return Map.insert(std::make_pair(GroupName, Map.size()+1)).first->second;
  }

  ArrayRef<StringRef> getOrderedGroupNames() {
    ViewBuffer.clear();
    ViewBuffer.push_back(""); // 0 is always outside of any group.
    for (auto It = Map.begin(); It != Map.end(); ++ It) {
      ViewBuffer.push_back(It->first);
    }
    return llvm::makeArrayRef(ViewBuffer);
  }

  bool isEnable() {
    return !FileToGroupMap.empty();
  }
};

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
    return llvm::djbHash(key, SWIFTDOC_HASH_SEED_5_1);
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

static bool hasDoubleUnderscore(Decl *D) {
  // Exclude decls with double-underscored names, either in arguments or
  // base names.
  static StringRef Prefix = "__";

  if (auto AFD = dyn_cast<AbstractFunctionDecl>(D)) {
    // If it's a function with a parameter with leading double underscore,
    // it's a private function.
    if (AFD->getParameters()->hasInternalParameter(Prefix))
      return true;
  }

  if (auto SubscriptD = dyn_cast<SubscriptDecl>(D)) {
    if (SubscriptD->getIndices()->hasInternalParameter(Prefix))
      return true;
  }
  if (auto *VD = dyn_cast<ValueDecl>(D)) {
    auto Name = VD->getBaseName();
    if (!Name.isSpecial() &&
        Name.getIdentifier().str().startswith(Prefix)) {
      return true;
    }
  }
  return false;
}

static bool shouldIncludeDecl(Decl *D, bool ExcludeDoubleUnderscore) {
  if (auto *VD = dyn_cast<ValueDecl>(D)) {
    // Skip the decl if it's not visible to clients. The use of
    // getEffectiveAccess is unusual here; we want to take the testability
    // state into account and emit documentation if and only if they are
    // visible to clients (which means public ordinarily, but
    // public+internal when testing enabled).
    if (VD->getEffectiveAccess() < swift::AccessLevel::Public)
      return false;
  }

  // Skip SPI decls.
  if (D->isSPI())
    return false;

  if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
    return shouldIncludeDecl(ED->getExtendedNominal(), ExcludeDoubleUnderscore);
  }
  if (ExcludeDoubleUnderscore && hasDoubleUnderscore(D)) {
    return false;
  }
  return true;
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

    DeclCommentTableWriter(DeclGroupNameContext &GroupContext):
      GroupContext(GroupContext) {}

    void resetSourceOrder() {
      SourceOrder = 0;
    }

    StringRef copyString(StringRef String) {
      char *Mem = static_cast<char *>(Arena.Allocate(String.size(), 1));
      std::copy(String.begin(), String.end(), Mem);
      return StringRef(Mem, String.size());
    }

    bool shouldSerializeDoc(Decl *D) {
      // When building the stdlib we intend to serialize unusual comments.
      // This situation is represented by GroupContext.isEnable().  In that
      // case, we perform more serialization to keep track of source order.
      if (GroupContext.isEnable())
        return true;

      // Skip the decl if it cannot have a comment.
      if (!D->canHaveComment())
        return false;

      // Skip the decl if it does not have a comment.
      if (D->getRawComment().Comments.empty())
        return false;
      return true;
    }

    void writeDocForExtensionDecl(ExtensionDecl *ED) {
      // Compute USR.
      {
        USRBuffer.clear();
        llvm::raw_svector_ostream OS(USRBuffer);
        if (ide::printExtensionUSR(ED, OS))
          return;
      }
      generator.insert(copyString(USRBuffer.str()),
                       { ED->getBriefComment(), ED->getRawComment(),
                         GroupContext.getGroupSequence(ED),
                         SourceOrder++ });
    }

    bool walkToDeclPre(Decl *D) override {
      if (!shouldIncludeDecl(D, /*ExcludeDoubleUnderscore*/true))
        return false;
      if (!shouldSerializeDoc(D))
        return true;
      if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
        writeDocForExtensionDecl(ED);
        return true;
      }

      auto *VD = dyn_cast<ValueDecl>(D);
      if (!VD)
        return true;

      // Compute USR.
      {
        USRBuffer.clear();
        llvm::raw_svector_ostream OS(USRBuffer);
        if (ide::printValueDeclUSR(VD, OS))
          return true;
      }

      generator.insert(copyString(USRBuffer.str()),
                       { VD->getBriefComment(), D->getRawComment(),
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
    Metadata.emit(ScratchRecord, SWIFTDOC_VERSION_MAJOR, SWIFTDOC_VERSION_MINOR,
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
namespace {
struct DeclLocationsTableData {
  uint32_t SourceFileOffset;
  uint32_t DocRangesOffset;
  LineColumn Loc;
  LineColumn StartLoc;
  LineColumn EndLoc;
};

class USRTableInfo {
public:
  using key_type = StringRef;
  using key_type_ref = key_type;
  using data_type = uint32_t;
  using data_type_ref = const data_type &;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  hash_value_type ComputeHash(key_type_ref key) {
    assert(!key.empty());
    return llvm::djbHash(key, SWIFTSOURCEINFO_HASH_SEED);
  }

  std::pair<unsigned, unsigned>
  EmitKeyDataLength(raw_ostream &out, key_type_ref key, data_type_ref data) {
    const unsigned numLen = 4;
    uint32_t keyLength = key.size();
    uint32_t dataLength = numLen;
    endian::Writer writer(out, little);
    writer.write<uint32_t>(keyLength);
    return { keyLength, dataLength };
  }

  void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
    out << key;
  }

  void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                unsigned len) {
    endian::Writer writer(out, little);
    writer.write<uint32_t>(data);
  }
};

class DeclUSRsTableWriter {
  llvm::StringSet<> USRs;
  llvm::OnDiskChainedHashTableGenerator<USRTableInfo> generator;
public:
  uint32_t peekNextId() const { return USRs.size(); }
  Optional<uint32_t> getNewUSRId(StringRef USR) {
    // Attempt to insert the USR into the StringSet.
    auto It = USRs.insert(USR);
    // If the USR exists in the StringSet, return None.
    if (!It.second)
      return None;
    auto Id = USRs.size() - 1;
    // We have to insert the USR from the StringSet because it's where the
    // memory is owned.
    generator.insert(It.first->getKey(), Id);
    return Id;
  }
  void emitUSRsRecord(llvm::BitstreamWriter &out) {
    decl_locs_block::DeclUSRSLayout USRsList(out);
    SmallVector<uint64_t, 8> scratch;
    llvm::SmallString<32> hashTableBlob;
    uint32_t tableOffset;
    {
      llvm::raw_svector_ostream blobStream(hashTableBlob);
      // Make sure that no bucket is at offset 0
      endian::write<uint32_t>(blobStream, 0, little);
      tableOffset = generator.Emit(blobStream);
    }
    USRsList.emit(scratch, tableOffset, hashTableBlob);
  }
};

class StringWriter {
  llvm::StringMap<uint32_t> IndexMap;
  llvm::SmallString<1024> Buffer;
public:
  uint32_t getTextOffset(StringRef Text) {
    auto IterAndIsNew = IndexMap.insert({Text, Buffer.size()});
    if (IterAndIsNew.second) {
      Buffer.append(Text);
      Buffer.push_back('\0');
    }
    return IterAndIsNew.first->getValue();
  }

  void emitSourceFilesRecord(llvm::BitstreamWriter &Out) {
    decl_locs_block::TextDataLayout TextBlob(Out);
    SmallVector<uint64_t, 8> scratch;
    TextBlob.emit(scratch, Buffer);
  }
};


/**
 Records the locations of `SingleRawComment` pieces for a declaration
 and emits them into a blob for the DOC_RANGES record.

 See: \c decl_locs_block::DocRangesLayout
 */
class DocRangeWriter {
  llvm::DenseMap<const Decl *, uint32_t> DeclOffsetMap;
  llvm::SmallString<1024> Buffer;
public:
  DocRangeWriter() {
    /**
     Offset 0 is reserved to mean "no offset", meaning that a declaration
     didn't have a doc comment.
     */
    Buffer.push_back(0);
  }

  /**
   \returns the offset into the doc ranges buffer for a declaration. Calling this
   twice on the same declaration will not duplicate data but return the
   original offset.
   */
  uint32_t getDocRangesOffset(const Decl *D,
      ArrayRef<std::pair<LineColumn, uint32_t>> DocRanges) {
    if (DocRanges.empty()) {
      return 0;
    }
    const auto EntryAndAlreadyFound = DeclOffsetMap.insert({ D, Buffer.size() });
    const auto StartOffset = EntryAndAlreadyFound.first->getSecond();
    const auto AlreadyInMap = !EntryAndAlreadyFound.second;
    if (AlreadyInMap) {
      return StartOffset;
    }

    llvm::raw_svector_ostream OS(Buffer);

    endian::write<uint32_t>(OS, DocRanges.size(), little);

    for (const auto &LineColumnAndLength : DocRanges) {
      endian::write<uint32_t>(OS, LineColumnAndLength.first.Line, little);
      endian::write<uint32_t>(OS, LineColumnAndLength.first.Column, little);
      endian::write<uint32_t>(OS, LineColumnAndLength.second, little);
    }

    return StartOffset;
  }

  void emitDocRangesRecord(llvm::BitstreamWriter &Out) {
    decl_locs_block::DocRangesLayout DocRangesBlob(Out);
    SmallVector<uint64_t, 8> Scratch;
    DocRangesBlob.emit(Scratch, Buffer);
  }
};

struct BasicDeclLocsTableWriter : public ASTWalker {
  llvm::SmallString<1024> Buffer;
  DeclUSRsTableWriter &USRWriter;
  StringWriter &FWriter;
  DocRangeWriter &DocWriter;
  BasicDeclLocsTableWriter(DeclUSRsTableWriter &USRWriter,
                           StringWriter &FWriter,
                           DocRangeWriter &DocWriter): USRWriter(USRWriter),
                           FWriter(FWriter),
                           DocWriter(DocWriter) {}

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override { return { false, S };}
  std::pair<bool, Expr *> walkToExprPre(Expr *E) override { return { false, E };}
  bool walkToTypeLocPre(TypeLoc &TL) override { return false; }
  bool walkToTypeReprPre(TypeRepr *T) override { return false; }
  bool walkToParameterListPre(ParameterList *PL) override { return false; }

  void appendToBuffer(DeclLocationsTableData data) {
    llvm::raw_svector_ostream out(Buffer);
    endian::Writer writer(out, little);
    writer.write<uint32_t>(data.SourceFileOffset);
    writer.write<uint32_t>(data.DocRangesOffset);
#define WRITE_LINE_COLUMN(X)                                                  \
writer.write<uint32_t>(data.X.Line);                                          \
writer.write<uint32_t>(data.X.Column);
    WRITE_LINE_COLUMN(Loc)
    WRITE_LINE_COLUMN(StartLoc);
    WRITE_LINE_COLUMN(EndLoc);
#undef WRITE_LINE_COLUMN
  }

  Optional<uint32_t> calculateNewUSRId(Decl *D) {
    llvm::SmallString<512> Buffer;
    llvm::raw_svector_ostream OS(Buffer);
    if (ide::printDeclUSR(D, OS))
      return None;
    return USRWriter.getNewUSRId(OS.str());
  }

  LineColumn getLineColumn(SourceManager &SM, SourceLoc Loc) {
    LineColumn Result;
    if (Loc.isValid()) {
      auto LC = SM.getLineAndColumn(Loc);
      Result.Line = LC.first;
      Result.Column = LC.second;
    }
    return Result;
  }

  Optional<DeclLocationsTableData> getLocData(Decl *D) {
    auto *File = D->getDeclContext()->getModuleScopeContext();
    auto Locs = cast<FileUnit>(File)->getBasicLocsForDecl(D);
    if (!Locs.hasValue())
      return None;
    DeclLocationsTableData Result;
    llvm::SmallString<128> AbsolutePath = Locs->SourceFilePath;
    llvm::sys::fs::make_absolute(AbsolutePath);
    Result.SourceFileOffset = FWriter.getTextOffset(AbsolutePath.str());
    Result.DocRangesOffset = DocWriter.getDocRangesOffset(D,
      llvm::makeArrayRef(Locs->DocRanges));
#define COPY_LINE_COLUMN(X)                                                   \
Result.X.Line = Locs->X.Line;                                                 \
Result.X.Column = Locs->X.Column;
    COPY_LINE_COLUMN(Loc)
    COPY_LINE_COLUMN(StartLoc)
    COPY_LINE_COLUMN(EndLoc)
#undef COPY_LINE_COLUMN
    return Result;
  }

  bool shouldSerializeSourceLoc(Decl *D) {
    if (D->isImplicit())
      return false;
    return true;
  }

  bool walkToDeclPre(Decl *D) override {
    SWIFT_DEFER {
      assert(USRWriter.peekNextId() * sizeof(DeclLocationsTableData)
             == Buffer.size() &&
            "USR Id has a one-to-one mapping with DeclLocationsTableData");
    };
    // .swiftdoc doesn't include comments for double underscored symbols, but
    // for .swiftsourceinfo, having the source location for these symbols isn't
    // a concern because these symbols are in .swiftinterface anyway.
    if (!shouldIncludeDecl(D, /*ExcludeDoubleUnderscore*/false))
      return false;
    if (!shouldSerializeSourceLoc(D))
      return true;
    // If we cannot get loc data for D, don't proceed.
    auto LocData = getLocData(D);
    if (!LocData.hasValue())
      return true;
    // If we have handled this USR before, don't proceed.
    auto USR = calculateNewUSRId(D);
    if (!USR.hasValue())
      return true;
    appendToBuffer(*LocData);
    return true;
  }
};

static void emitBasicLocsRecord(llvm::BitstreamWriter &Out,
                                ModuleOrSourceFile MSF,
                                DeclUSRsTableWriter &USRWriter,
                                StringWriter &FWriter,
                                DocRangeWriter &DocWriter) {
  assert(MSF);
  const decl_locs_block::BasicDeclLocsLayout DeclLocsList(Out);
  BasicDeclLocsTableWriter Writer(USRWriter, FWriter, DocWriter);
  if (auto *SF = MSF.dyn_cast<SourceFile*>()) {
    SF->walk(Writer);
  } else {
    MSF.get<ModuleDecl*>()->walk(Writer);
  }

  SmallVector<uint64_t, 8> scratch;
  DeclLocsList.emit(scratch, Writer.Buffer);
}

class SourceInfoSerializer : public SerializerBase {
public:
  using SerializerBase::SerializerBase;
  using SerializerBase::writeToStream;

  using SerializerBase::Out;
  using SerializerBase::M;
  using SerializerBase::SF;
  /// Writes the BLOCKINFO block for the module sourceinfo file.
  void writeSourceInfoBlockInfoBlock() {
    BCBlockRAII restoreBlock(Out, llvm::bitc::BLOCKINFO_BLOCK_ID, 2);

    SmallVector<unsigned char, 64> nameBuffer;
#define BLOCK(X) emitBlockID(X ## _ID, #X, nameBuffer)
#define BLOCK_RECORD(K, X) emitRecordID(K::X, #X, nameBuffer)

    BLOCK(MODULE_SOURCEINFO_BLOCK);

    BLOCK(CONTROL_BLOCK);
    BLOCK_RECORD(control_block, METADATA);
    BLOCK_RECORD(control_block, MODULE_NAME);
    BLOCK_RECORD(control_block, TARGET);

    BLOCK(DECL_LOCS_BLOCK);
    BLOCK_RECORD(decl_locs_block, BASIC_DECL_LOCS);
    BLOCK_RECORD(decl_locs_block, DECL_USRS);
    BLOCK_RECORD(decl_locs_block, TEXT_DATA);
    BLOCK_RECORD(decl_locs_block, DOC_RANGES);

#undef BLOCK
#undef BLOCK_RECORD
  }
  /// Writes the Swift sourceinfo file header and name.
  void writeSourceInfoHeader() {
    {
      BCBlockRAII restoreBlock(Out, CONTROL_BLOCK_ID, 3);
      control_block::ModuleNameLayout ModuleName(Out);
      control_block::MetadataLayout Metadata(Out);
      control_block::TargetLayout Target(Out);

      auto& LangOpts = M->getASTContext().LangOpts;
      Metadata.emit(ScratchRecord, SWIFTSOURCEINFO_VERSION_MAJOR,
                    SWIFTSOURCEINFO_VERSION_MINOR,
                    /*short version string length*/0, /*compatibility length*/0,
              version::getSwiftFullVersion(LangOpts.EffectiveLanguageVersion));

      ModuleName.emit(ScratchRecord, M->getName().str());
      Target.emit(ScratchRecord, LangOpts.Target.str());
    }
  }
};
}
void serialization::writeSourceInfoToStream(raw_ostream &os,
                                            ModuleOrSourceFile DC) {
  assert(DC);
  SourceInfoSerializer S{SWIFTSOURCEINFO_SIGNATURE, DC};
  // FIXME: This is only really needed for debugging. We don't actually use it.
  S.writeSourceInfoBlockInfoBlock();
  {
    BCBlockRAII moduleBlock(S.Out, MODULE_SOURCEINFO_BLOCK_ID, 2);
    S.writeSourceInfoHeader();
    {
      BCBlockRAII restoreBlock(S.Out, DECL_LOCS_BLOCK_ID, 4);
      DeclUSRsTableWriter USRWriter;
      StringWriter FPWriter;
      DocRangeWriter DocWriter;
      emitBasicLocsRecord(S.Out, DC, USRWriter, FPWriter, DocWriter);
      // Emit USR table mapping from a USR to USR Id.
      // The basic locs record uses USR Id instead of actual USR, so that we
      // don't need to repeat USR texts for newly added records.
      USRWriter.emitUSRsRecord(S.Out);
      // A blob of 0 terminated strings referenced by the location records,
      // e.g. file paths.
      FPWriter.emitSourceFilesRecord(S.Out);
      // A blob of fixed-size location records of `SingleRawComment` pieces.
      DocWriter.emitDocRangesRecord(S.Out);
    }
  }

  S.writeToStream(os);
}
