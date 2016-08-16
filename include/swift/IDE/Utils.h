//===--- Utils.h - Misc utilities -------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_UTILS_H
#define SWIFT_IDE_UTILS_H

#include "swift/Basic/LLVM.h"
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
  ModuleEntity Mod;
  SourceLoc Loc;
  bool IsRef = true;
  bool IsKeywordArgument = false;
  Type Ty;
  DeclContext *DC = nullptr;
  Type ContainerType;

  SemaToken() = default;
  SemaToken(ValueDecl *ValueD, TypeDecl *CtorTyRef, SourceLoc Loc, bool IsRef,
            Type Ty, Type ContainerType) : ValueD(ValueD), CtorTyRef(CtorTyRef), Loc(Loc),
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
                          TypeDecl *CtorTyRef, Type T) override;
  bool visitCallArgName(Identifier Name, CharSourceRange Range,
                        ValueDecl *D) override;
  bool visitModuleReference(ModuleEntity Mod, CharSourceRange Range) override;
  bool rangeContainsLoc(SourceRange Range) const {
    return getSourceMgr().rangeContainsTokenLoc(Range, LocToResolve);
  }
  bool isDone() const { return SemaTok.isValid(); }
  bool tryResolve(ValueDecl *D, TypeDecl *CtorTyRef,
                  SourceLoc Loc, bool IsRef, Type Ty = Type());
  bool tryResolve(ModuleEntity Mod, SourceLoc Loc);
  bool visitSubscriptReference(ValueDecl *D, CharSourceRange Range,
                               bool IsOpenBracket) override;
};
} // namespace ide

class ArchetypeTransformer {
  struct Implementation;
  Implementation &Impl;
public:
  ArchetypeTransformer(DeclContext *DC, Type Ty);
  llvm::function_ref<Type(Type)> getTransformerFunc();
  ~ArchetypeTransformer();
};
} // namespace swift

#endif // SWIFT_IDE_UTILS_H

