//===--- ASTPrinter.h - Class for printing the AST --------------*- C++ -*-===//
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

#ifndef SWIFT_AST_ASTPRINTER_H
#define SWIFT_AST_ASTPRINTER_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/UUID.h"
#include "swift/AST/Identifier.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include "swift/AST/PrintOptions.h"

namespace swift {
  class Decl;
  class DeclContext;
  class DynamicSelfType;
  class ModuleEntity;
  class TypeDecl;
  class Type;
  struct TypeLoc;
  class Pattern;
  class ExtensionDecl;
  class NominalTypeDecl;
  class ValueDecl;
  class SourceLoc;
  enum class tok;

/// Describes the context in which a name is being printed, which
/// affects the keywords that need to be escaped.
enum class PrintNameContext {
  /// Normal context
  Normal,
  /// Keyword context, where no keywords are escaped.
  Keyword,
  /// Generic parameter context, where 'Self' is not escaped.
  GenericParameter,
  /// Class method return type, where 'Self' is not escaped.
  ClassDynamicSelf,
  /// Function parameter context, where keywords other than let/var/inout are
  /// not escaped.
  FunctionParameterExternal,
  FunctionParameterLocal,
  /// Tuple element context, similar to \c FunctionParameterExternal.
  TupleElement,
  /// Attributes, which are escaped as 'Normal', but differentiated for
  /// the purposes of printName* callbacks.
  Attribute,
};

/// Describes the kind of structured entity being printed.
///
/// This includes printables with sub-structure that cannot be completely
/// handled by the printDeclPre/printDeclPost callbacks.
/// E.g.
/// \code
///   func foo(<FunctionParameter>x: Int = 2</FunctionParameter>, ...)
/// \endcode
enum class PrintStructureKind {
  GenericParameter,
  GenericRequirement,
  FunctionParameter,
  FunctionType,
  FunctionReturnType,
  BuiltinAttribute,
  TupleType,
  TupleElement,
  NumberLiteral,
  StringLiteral,
};

/// An abstract class used to print an AST.
class ASTPrinter {
  unsigned CurrentIndentation = 0;
  unsigned PendingNewlines = 0;
  const NominalTypeDecl *SynthesizeTarget = nullptr;

  void printTextImpl(StringRef Text);

public:
  virtual ~ASTPrinter() {}

  virtual void printText(StringRef Text) = 0;

  // MARK: Callback interface.

  /// Called after the printer decides not to print D.
  ///
  /// Callers should use callAvoidPrintDeclPost().
  virtual void avoidPrintDeclPost(const Decl *D) {};
  /// Called before printing of a declaration.
  ///
  /// Callers should use callPrintDeclPre().
  virtual void printDeclPre(const Decl *D, Optional<BracketOptions> Bracket) {}
  /// Called before printing at the point which would be considered the location
  /// of the declaration (normally the name of the declaration).
  ///
  /// Callers should use callPrintDeclLoc().
  virtual void printDeclLoc(const Decl *D) {}
  /// Called after printing the name of the declaration.
  virtual void printDeclNameEndLoc(const Decl *D) {}
  /// Called after printing the name of a declaration, or in the case of
  /// functions its signature.
  virtual void printDeclNameOrSignatureEndLoc(const Decl *D) {}
  /// Called after finishing printing of a declaration.
  ///
  /// Callers should use callPrintDeclPost().
  virtual void printDeclPost(const Decl *D, Optional<BracketOptions> Bracket) {}

  /// Called before printing a type.
  virtual void printTypePre(const TypeLoc &TL) {}
  /// Called after printing a type.
  virtual void printTypePost(const TypeLoc &TL) {}

  /// Called when printing the referenced name of a type declaration, possibly
  /// from deep inside another type.
  ///
  /// \param T the original \c Type being referenced. May be null.
  /// \param RefTo the \c TypeDecl this is considered a reference to.
  /// \param Name the name to be printed.
  virtual void printTypeRef(Type T, const TypeDecl *RefTo, Identifier Name);

  /// Called when printing the referenced name of a module.
  virtual void printModuleRef(ModuleEntity Mod, Identifier Name);

  /// Called before printing a synthesized extension.
  virtual void printSynthesizedExtensionPre(const ExtensionDecl *ED,
                                            const NominalTypeDecl *NTD,
                                            Optional<BracketOptions> Bracket) {}

  /// Called after printing a synthesized extension.
  virtual void printSynthesizedExtensionPost(const ExtensionDecl *ED,
                                             const NominalTypeDecl *NTD,
                                             Optional<BracketOptions> Bracket) {}

  /// Called before printing a structured entity.
  ///
  /// Callers should use callPrintStructurePre().
  virtual void printStructurePre(PrintStructureKind Kind,
                                 const Decl *D = nullptr) {}
  /// Called after printing a structured entity.
  virtual void printStructurePost(PrintStructureKind Kind,
                                  const Decl *D = nullptr) {}

  /// Called before printing a name in the given context.
  virtual void printNamePre(PrintNameContext Context) {}
  /// Called after printing a name in the given context.
  virtual void printNamePost(PrintNameContext Context) {}

  // Helper functions.

  void printSeparator(bool &first, StringRef separator) {
    if (first) {
      first = false;
    } else {
      printTextImpl(separator);
    }
  }

  ASTPrinter &operator<<(StringRef Text) {
    printTextImpl(Text);
    return *this;
  }

  ASTPrinter &operator<<(unsigned long long N);
  ASTPrinter &operator<<(UUID UU);

  ASTPrinter &operator<<(DeclName name);

  void printKeyword(StringRef name) {
    callPrintNamePre(PrintNameContext::Keyword);
    *this << name;
    printNamePost(PrintNameContext::Keyword);
  }

  void printAttrName(StringRef name, bool needAt = false) {
    callPrintNamePre(PrintNameContext::Attribute);
    if (needAt)
      *this << "@";
    *this << name;
    printNamePost(PrintNameContext::Attribute);
  }

  ASTPrinter &printSimpleAttr(StringRef name, bool needAt = false) {
    callPrintStructurePre(PrintStructureKind::BuiltinAttribute);
    printAttrName(name, needAt);
    printStructurePost(PrintStructureKind::BuiltinAttribute);
    return *this;
  }

  void printName(Identifier Name,
                 PrintNameContext Context = PrintNameContext::Normal);

  void setIndent(unsigned NumSpaces) {
    CurrentIndentation = NumSpaces;
  }

  void setSynthesizedTarget(NominalTypeDecl *Target) {
    assert((!SynthesizeTarget || !Target || Target == SynthesizeTarget) &&
           "unexpected change of setSynthesizedTarget");
    // FIXME: this can overwrite the original target with nullptr.
    SynthesizeTarget = Target;
  }

  void printNewline() {
    PendingNewlines++;
  }

  void forceNewlines() {
    if (PendingNewlines > 0) {
      llvm::SmallString<16> Str;
      for (unsigned i = 0; i != PendingNewlines; ++i)
        Str += '\n';
      PendingNewlines = 0;
      printText(Str);
      printIndent();
    }
  }

  virtual void printIndent();

  // MARK: Callback interface wrappers that perform ASTPrinter bookkeeping.

   /// Make a callback to printDeclPre(), performing any necessary bookeeping.
  void callPrintDeclPre(const Decl *D, Optional<BracketOptions> Bracket);

  /// Make a callback to printDeclPost(), performing any necessary bookeeping.
  void callPrintDeclPost(const Decl *D, Optional<BracketOptions> Bracket) {
    printDeclPost(D, Bracket);
  }

  /// Make a callback to avoidPrintDeclPost(), performing any necessary
  /// bookkeeping.
  void callAvoidPrintDeclPost(const Decl *D) {
    avoidPrintDeclPost(D);
  }

   /// Make a callback to printDeclLoc(), performing any necessary bookeeping.
  void callPrintDeclLoc(const Decl *D) {
    forceNewlines();
    printDeclLoc(D);
  }

   /// Make a callback to printNamePre(), performing any necessary bookeeping.
  void callPrintNamePre(PrintNameContext Context) {
    forceNewlines();
    printNamePre(Context);
  }

  /// Make a callback to printStructurePre(), performing any necessary
  /// bookkeeping.
  void callPrintStructurePre(PrintStructureKind Kind, const Decl *D = nullptr) {
    forceNewlines();
    printStructurePre(Kind, D);
  }

  /// To sanitize a malformed utf8 string to a well-formed one.
  static std::string sanitizeUtf8(StringRef Text);
  static ValueDecl* findConformancesWithDocComment(ValueDecl *VD);

private:
  virtual void anchor();
};

/// An AST printer for a raw_ostream.
class StreamPrinter : public ASTPrinter {
protected:
  raw_ostream &OS;

public:
  explicit StreamPrinter(raw_ostream &OS) : OS(OS) {}

  void printText(StringRef Text) override;
};

/// AST stream printer that adds extra indentation to each line.
class ExtraIndentStreamPrinter : public StreamPrinter {
  StringRef ExtraIndent;

public:
  ExtraIndentStreamPrinter(raw_ostream &out, StringRef extraIndent)
  : StreamPrinter(out), ExtraIndent(extraIndent) { }

  virtual void printIndent() {
    printText(ExtraIndent);
    StreamPrinter::printIndent();
  }
};

bool shouldPrint(const Decl *D, PrintOptions &Options);
bool shouldPrintPattern(const Pattern *P, PrintOptions &Options);

void printContext(raw_ostream &os, DeclContext *dc);

bool printRequirementStub(ValueDecl *Requirement, DeclContext *Adopter,
                          Type AdopterTy, SourceLoc TypeLoc, raw_ostream &OS);

/// Print a keyword or punctuator directly by its kind.
llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, tok keyword);

} // namespace swift

#endif // LLVM_SWIFT_AST_ASTPRINTER_H
