//===--- ASTPrinter.h - Class for printing the AST --------------*- C++ -*-===//
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

#ifndef SWIFT_AST_ASTPRINTER_H
#define SWIFT_AST_ASTPRINTER_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
  class Decl;
  class Module;
  class TypeDecl;

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

  /// Called before printing of a declaration.
  virtual void printDeclPre(const Decl *D) {}
  /// Called before printing at the point which would be considered the location
  /// of the declaration (normally the name of the declaration).
  virtual void printDeclLoc(const Decl *D) {}
  /// Called after finishing printing of a declaration.
  virtual void printDeclPost(const Decl *D) {}

  /// Called when printing the referenced name of a type declaration.
  virtual void printTypeRef(const TypeDecl *TD, StringRef Text) {
    printTextImpl(Text);
  }
  /// Called when printing the referenced name of a module.
  virtual void printModuleRef(const Module *Mod, StringRef Text) {
    printTextImpl(Text);
  }

  // Helper functions.

  ASTPrinter &operator<<(StringRef Text) {
    printTextImpl(Text);
    return *this;
  }

  ASTPrinter &operator<<(unsigned long long N);

  void printIndent(unsigned NumSpaces) {
    CurrentIndentation = NumSpaces;
  }

  void printNewline() {
    PendingNewlines++;
  }

  /// Schedule a \c printDeclPre callback to be called as soon as a
  /// non-whitespace character is printed.
  void callPrintDeclPre(const Decl *D) {
    PendingDeclPreCallback = D;
  }

  void callPrintDeclLoc(const Decl *D) {
    PendingDeclLocCallback = D;
  }

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

} // namespace swift

#endif // LLVM_SWIFT_AST_ASTPRINTER_H
