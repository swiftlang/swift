//===--- DeclarationFragmentPrinter.h - Declaration Fragment Printer ------===//
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

#ifndef SWIFT_SYMBOLGRAPHGEN_DECLARATIONFRAGMENTPRINTER_H
#define SWIFT_SYMBOLGRAPHGEN_DECLARATIONFRAGMENTPRINTER_H

#include "llvm/Support/JSON.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/Basic/LLVM.h"

namespace swift {

class Decl;
class Type;
class TypeDecl;

namespace symbolgraphgen {

struct SymbolGraph;

/// Prints AST nodes as a stream of tagged fragments for syntax highlighting.
///
/// These fragments are meant to display a somewhat abbreviated part of the
/// declaration for display in documentation, ignoring things like member and
/// function bodies.
///
/// For example, a function:
///
/// ```swift
/// func foo() {
///   print("Hello, world!")
/// }
/// ```
///
/// Will have fragments representing the `func foo()` part.
class DeclarationFragmentPrinter : public ASTPrinter {
  enum class FragmentKind {
    None,
    Keyword,
    Attribute,
    NumberLiteral,
    StringLiteral,
    Identifier,
    TypeIdentifier,
    GenericParameter,
    Text,
  };

  SymbolGraph &Graph;

  /// The output stream to print fragment objects to.
  llvm::json::OStream &OS;

  /// The current fragment being considered.
  FragmentKind Kind;

  /// The actual source text of the fragment.
  SmallString<256> Spelling;

  SmallString<256> USR;

  StringRef getKindSpelling(FragmentKind Kind) const;

  /// Open a new kind of fragment without committing its spelling.
  void openFragment(FragmentKind Kind);

  /// Close the current fragment if there is one, and commit it for display.
  void closeFragment();

public:
  DeclarationFragmentPrinter(SymbolGraph &Graph,
                             llvm::json::OStream &OS,
                             Optional<StringRef> Key = None)
    : Graph(Graph),
      OS(OS),
      Kind(FragmentKind::None) {
    if (Key) {
      OS.attributeBegin(*Key);
      OS.arrayBegin();
    } else {
      OS.arrayBegin();
    }
  }

  void printDeclLoc(const Decl *D) override;

  void printDeclNameEndLoc(const Decl *D) override {
    closeFragment();
  }

  void printNamePre(PrintNameContext Context) override;

  void printStructurePre(PrintStructureKind Kind, const Decl *D) override;

  void printNamePost(PrintNameContext Context) override {
    closeFragment();
  }

  void printTypeRef(Type T, const TypeDecl *RefTo, Identifier Name,
                    PrintNameContext NameContext) override;

  void printText(StringRef Text) override;

  ~DeclarationFragmentPrinter() {
    closeFragment();
    OS.arrayEnd();
    OS.attributeEnd();
  }
};

} // end namespace symbolgraphgen
} // end namespace swift

#endif // SWIFT_SYMBOLGRAPHGEN_DECLARATIONFRAGMENTPRINTER_H
