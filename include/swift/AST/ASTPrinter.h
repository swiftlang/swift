//===--- ASTPrinter.h - Class for printing the AST --------------*- C++ -*-===//
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

#ifndef SWIFT_AST_ASTPRINTER_H
#define SWIFT_AST_ASTPRINTER_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/UUID.h"
#include "swift/AST/Identifier.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
  class Decl;
  class DeclContext;
  class ModuleEntity;
  class TypeDecl;
  class Type;
  class Pattern;
  struct PrintOptions;

/// Describes the context in which a name is being printed, which
/// affects the keywords that need to be escaped.
enum class PrintNameContext {
  /// Normal context
  Normal,
  /// Generic parameter context, where 'Self' is not escaped.
  GenericParameter,
  /// Function parameter context, where keywords other than let/var/inout are
  /// not escaped.
  FunctionParameter,
};

/// An abstract class used to print an AST.
class ASTPrinter {
  unsigned CurrentIndentation = 0;
  unsigned PendingNewlines = 0;
  const Decl *PendingDeclPreCallback = nullptr;
  const Decl *PendingDeclLocCallback = nullptr;

  void printTextImpl(StringRef Text);

public:
  virtual ~ASTPrinter() {}

  virtual void printText(StringRef Text) = 0;

  /// Called after the printer decides not to print D.
  virtual void avoidPrintDeclPost(const Decl *D) {};
  /// Called before printing of a declaration.
  virtual void printDeclPre(const Decl *D) {}
  /// Called before printing at the point which would be considered the location
  /// of the declaration (normally the name of the declaration).
  virtual void printDeclLoc(const Decl *D) {}
  /// Called after printing the name of the declaration (the signature for
  /// functions).
  virtual void printDeclNameEndLoc(const Decl *D) {}
  /// Called after finishing printing of a declaration.
  virtual void printDeclPost(const Decl *D) {}

  /// Called when printing the referenced name of a type declaration.
  virtual void printTypeRef(const TypeDecl *TD, Identifier Name);

  /// Called when printing the referenced name of a module.
  virtual void printModuleRef(ModuleEntity Mod, Identifier Name);

  // Helper functions.

  ASTPrinter &operator<<(StringRef Text) {
    printTextImpl(Text);
    return *this;
  }

  ASTPrinter &operator<<(unsigned long long N);
  ASTPrinter &operator<<(UUID UU);

  void printName(Identifier Name,
                 PrintNameContext Context = PrintNameContext::Normal);

  void setIndent(unsigned NumSpaces) {
    CurrentIndentation = NumSpaces;
  }

  void printNewline() {
    PendingNewlines++;
  }

  virtual void printIndent();

  /// Schedule a \c printDeclPre callback to be called as soon as a
  /// non-whitespace character is printed.
  void callPrintDeclPre(const Decl *D) {
    PendingDeclPreCallback = D;
  }

  void callPrintDeclLoc(const Decl *D) {
    PendingDeclLocCallback = D;
  }

  /// To sanitize a malformed utf8 string to a well-formed one.
  static std::string sanitizeUtf8(StringRef Text);
  static bool printTypeInterface(Type Ty, DeclContext *DC, std::string &Result);
  static bool printTypeInterface(Type Ty, DeclContext *DC, llvm::raw_ostream &Out);

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

bool shouldPrint(const Decl *D, PrintOptions &Options);
bool shouldPrintPattern(const Pattern *P, PrintOptions &Options);

} // namespace swift

#endif // LLVM_SWIFT_AST_ASTPRINTER_H
