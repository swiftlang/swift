//===--- Utils.h - Misc utilities -------------------------------*- C++ -*-===//
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

#ifndef SWIFT_IDE_UTILS_H
#define SWIFT_IDE_UTILS_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/Module.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/SourceEntityWalker.h"
#include "llvm/ADT/StringRef.h"
#include <memory>
#include <string>
#include <functional>
#include <vector>

namespace llvm {
  template<typename Fn> class function_ref;
  class MemoryBuffer;
}

namespace clang {
  class Module;
  class NamedDecl;
}

namespace swift {
  class ModuleDecl;
  class ValueDecl;
  class ASTContext;
  class CompilerInvocation;
  class SourceFile;
  class TypeDecl;
  class SourceLoc;
  class Type;
  class Decl;
  class DeclContext;
  class ClangNode;
  class ClangImporter;

namespace ide {
struct SourceCompleteResult {
  // Set to true if the input source is fully formed, false otherwise.
  bool IsComplete;
  // The text to use as the indent string when auto indenting the next line.
  // This will contain the exactly what the client typed (any whitespaces and
  // tabs) and can be used to indent subsequent lines. It does not include
  // the current indent level, IDE clients should insert the correct indentation
  // with spaces or tabs to account for the current indent level. The indent
  // prefix will contain the leading space characters of the line that
  // contained the '{', '(' or '[' character that was unbalanced.
  std::string IndentPrefix;
  // Returns the indent level as an indentation count (number of indentations
  // to apply). Clients can translate this into the standard indentation that
  // is being used by the IDE (3 spaces? 1 tab?) and should use the indent
  // prefix string followed by the correct indentation.
  uint32_t IndentLevel;

  SourceCompleteResult() :
      IsComplete(false),
      IndentPrefix(),
      IndentLevel(0) {}
};

SourceCompleteResult
isSourceInputComplete(std::unique_ptr<llvm::MemoryBuffer> MemBuf);
SourceCompleteResult isSourceInputComplete(StringRef Text);

bool initInvocationByClangArguments(ArrayRef<const char *> ArgList,
                                    CompilerInvocation &Invok,
                                    std::string &Error);

/// Visits all overridden declarations exhaustively from VD, including protocol
/// conformances and clang declarations.
void walkOverriddenDecls(const ValueDecl *VD,
                         std::function<void(llvm::PointerUnion<
                             const ValueDecl*, const clang::NamedDecl*>)> Fn);

void collectModuleNames(StringRef SDKPath, std::vector<std::string> &Modules);

std::string getSDKName(StringRef Path);

std::string getSDKVersion(StringRef Path);

struct PlaceholderOccurrence {
  /// The complete placeholder string.
  StringRef FullPlaceholder;
  /// The inner string of the placeholder.
  StringRef PlaceholderContent;
  /// The dollar identifier that was used to replace the placeholder.
  StringRef IdentifierReplacement;
};

/// Replaces Xcode editor placeholders (<#such as this#>) with dollar
/// identifiers and returns a new memory buffer.
///
/// The replacement identifier will be the same size as the placeholder so that
/// the new buffer will have the same size as the input buffer.
std::unique_ptr<llvm::MemoryBuffer>
  replacePlaceholders(std::unique_ptr<llvm::MemoryBuffer> InputBuf,
              llvm::function_ref<void(const PlaceholderOccurrence &)> Callback);

std::unique_ptr<llvm::MemoryBuffer>
  replacePlaceholders(std::unique_ptr<llvm::MemoryBuffer> InputBuf,
                      bool *HadPlaceholder = nullptr);

void getLocationInfo(
    const ValueDecl *VD,
    llvm::Optional<std::pair<unsigned, unsigned>> &DeclarationLoc,
    StringRef &Filename);

void getLocationInfoForClangNode(ClangNode ClangNode,
                                 ClangImporter *Importer,
       llvm::Optional<std::pair<unsigned, unsigned>> &DeclarationLoc,
                                 StringRef &Filename);

Optional<std::pair<unsigned, unsigned>> parseLineCol(StringRef LineCol);

Decl *getDeclFromUSR(ASTContext &context, StringRef USR, std::string &error);
Decl *getDeclFromMangledSymbolName(ASTContext &context, StringRef mangledName,
                                   std::string &error);

Type getTypeFromMangledTypename(ASTContext &Ctx, StringRef mangledName,
                                std::string &error);

Type getTypeFromMangledSymbolname(ASTContext &Ctx, StringRef mangledName,
                                  std::string &error);

class XMLEscapingPrinter : public StreamPrinter {
  public:
  XMLEscapingPrinter(raw_ostream &OS) : StreamPrinter(OS){};
  void printText(StringRef Text) override;
  void printXML(StringRef Text);
};

struct SemaToken {
  ValueDecl *ValueD = nullptr;
  TypeDecl *CtorTyRef = nullptr;
  ExtensionDecl *ExtTyRef = nullptr;
  ModuleEntity Mod;
  SourceLoc Loc;
  bool IsRef = true;
  bool IsKeywordArgument = false;
  Type Ty;
  DeclContext *DC = nullptr;
  Type ContainerType;

  SemaToken() = default;
  SemaToken(ValueDecl *ValueD, TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef,
            SourceLoc Loc, bool IsRef, Type Ty, Type ContainerType) :
            ValueD(ValueD), CtorTyRef(CtorTyRef), ExtTyRef(ExtTyRef), Loc(Loc),
            IsRef(IsRef), Ty(Ty), DC(ValueD->getDeclContext()),
            ContainerType(ContainerType) {}
  SemaToken(ModuleEntity Mod, SourceLoc Loc) : Mod(Mod), Loc(Loc) { }

  bool isValid() const { return ValueD != nullptr || Mod; }
  bool isInvalid() const { return !isValid(); }
};

class SemaLocResolver : public SourceEntityWalker {
  SourceFile &SrcFile;
  SourceLoc LocToResolve;
  SemaToken SemaTok;
  Type ContainerType;

public:
  explicit SemaLocResolver(SourceFile &SrcFile) : SrcFile(SrcFile) { }
  SemaToken resolve(SourceLoc Loc);
  SourceManager &getSourceMgr() const;
private:
  bool walkToExprPre(Expr *E) override;
  bool walkToDeclPre(Decl *D, CharSourceRange Range) override;
  bool walkToDeclPost(Decl *D) override;
  bool walkToStmtPre(Stmt *S) override;
  bool walkToStmtPost(Stmt *S) override;
  bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                          TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef, Type T,
                          SemaReferenceKind Kind) override;
  bool visitCallArgName(Identifier Name, CharSourceRange Range,
                        ValueDecl *D) override;
  bool visitModuleReference(ModuleEntity Mod, CharSourceRange Range) override;
  bool rangeContainsLoc(SourceRange Range) const {
    return getSourceMgr().rangeContainsTokenLoc(Range, LocToResolve);
  }
  bool isDone() const { return SemaTok.isValid(); }
  bool tryResolve(ValueDecl *D, TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef,
                  SourceLoc Loc, bool IsRef, Type Ty = Type());
  bool tryResolve(ModuleEntity Mod, SourceLoc Loc);
  bool visitSubscriptReference(ValueDecl *D, CharSourceRange Range,
                               bool IsOpenBracket) override;
};

enum class RangeKind : int8_t{
  Invalid = -1,
  SingleExpression,
  SingleStatement,
  SingleDecl,

  MultiStatement,
};

struct DeclaredDecl {
  ValueDecl *VD;
  bool ReferredAfterRange;
  DeclaredDecl(ValueDecl* VD) : VD(VD), ReferredAfterRange(false) {}
  DeclaredDecl(): DeclaredDecl(nullptr) {}
  bool operator==(const DeclaredDecl& other);
};

struct ReferencedDecl {
  ValueDecl *VD;
  Type Ty;
  ReferencedDecl(ValueDecl* VD, Type Ty) : VD(VD), Ty(Ty) {}
  ReferencedDecl() : ReferencedDecl(nullptr, Type()) {}
  bool operator==(const ReferencedDecl& other);
};

enum class OrphanKind : int8_t {
  None,
  Break,
  Continue,
};
struct ResolvedRangeInfo {
  RangeKind Kind;
  Type Ty;
  StringRef Content;
  bool HasSingleEntry;
  OrphanKind Orphan;

  // The topmost ast nodes contained in the given range.
  ArrayRef<ASTNode> ContainedNodes;
  ArrayRef<DeclaredDecl> DeclaredDecls;
  ArrayRef<ReferencedDecl> ReferencedDecls;
  DeclContext* RangeContext;
  ResolvedRangeInfo(RangeKind Kind, Type Ty, StringRef Content,
                    DeclContext* RangeContext,
                    bool HasSingleEntry,
                    OrphanKind Orphan,
                    ArrayRef<ASTNode> ContainedNodes,
                    ArrayRef<DeclaredDecl> DeclaredDecls,
                    ArrayRef<ReferencedDecl> ReferencedDecls): Kind(Kind),
                      Ty(Ty), Content(Content), HasSingleEntry(HasSingleEntry),
                      Orphan(Orphan),
                      ContainedNodes(ContainedNodes),
                      DeclaredDecls(DeclaredDecls),
                      ReferencedDecls(ReferencedDecls),
                      RangeContext(RangeContext) {}
  ResolvedRangeInfo() :
  ResolvedRangeInfo(RangeKind::Invalid, Type(), StringRef(), nullptr,
                    /*Single entry*/true, OrphanKind::None, {}, {}, {}) {}
  void print(llvm::raw_ostream &OS);
};

class RangeResolver : public SourceEntityWalker {
  struct Implementation;
  Implementation *Impl;
  bool walkToExprPre(Expr *E) override;
  bool walkToExprPost(Expr *E) override;
  bool walkToStmtPre(Stmt *S) override;
  bool walkToStmtPost(Stmt *S) override;
  bool walkToDeclPre(Decl *D, CharSourceRange Range) override;
  bool walkToDeclPost(Decl *D) override;
  bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                          TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef, Type T,
                          SemaReferenceKind Kind) override;
public:
  RangeResolver(SourceFile &File, SourceLoc Start, SourceLoc End);
  RangeResolver(SourceFile &File, unsigned Offset, unsigned Length);
  ResolvedRangeInfo resolve();
  ~RangeResolver();
};

/// This provides a utility to view a printed name by parsing the components
/// of that name. The components include a base name and an array of argument
/// labels.
class DeclNameViewer {
  StringRef BaseName;
  SmallVector<StringRef, 4> Labels;
public:
  DeclNameViewer(StringRef Text);
  StringRef base() const { return BaseName; }
  llvm::ArrayRef<StringRef> args() const { return llvm::makeArrayRef(Labels); }
  unsigned partsCount() const { return 1 + Labels.size(); }
  unsigned commonPartsCount(DeclNameViewer &Other) const;
};

/// This provide a utility for writing to a underlying string buffer mulitiple
/// string pieces and retrieve them later when the underlying buffer is stable.
class DelayedStringRetriever : public raw_ostream {
    SmallVectorImpl<char> &OS;
    llvm::raw_svector_ostream Underlying;
    SmallVector<std::pair<unsigned, unsigned>, 4> StartEnds;
    unsigned CurrentStart;

public:
    explicit DelayedStringRetriever(SmallVectorImpl<char> &OS) : OS(OS), Underlying(OS) {}
    ~DelayedStringRetriever() override {}
    void startPiece() {
      CurrentStart = OS.size();
    }
    void endPiece() {
      StartEnds.emplace_back(CurrentStart, OS.size());
    }
    void write_impl(const char *ptr, size_t size) override {
      Underlying.write(ptr, size);
    }
    uint64_t current_pos() const override {
      return Underlying.tell();
    }
    size_t preferred_buffer_size() const override {
      return 0;
    }
    void retrieve(llvm::function_ref<void(StringRef)> F) {
      for (auto P : StartEnds) {
        F(StringRef(OS.begin() + P.first, P.second - P.first));
      }
    }
  };
} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_UTILS_H

