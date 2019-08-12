//===--- DiagnosticFormatting.cpp - Diagnostic Message Formatting --  -----===//
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
//
//  This file defines functions for formatting diagnostic messages and
//  arguments.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticFormatting.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/TypeRepr.h"

using namespace swift;

/// Skip forward to one of the given delimiters.
///
/// \param Text The text to search through, which will be updated to point
/// just after the delimiter.
///
/// \param Delim The first character delimiter to search for.
///
/// \param FoundDelim On return, true if the delimiter was found, or false
/// if the end of the string was reached.
///
/// \returns The string leading up to the delimiter, or the empty string
/// if no delimiter is found.
static StringRef skipToDelimiter(StringRef &Text, char Delim,
                                 bool *FoundDelim = nullptr) {
  unsigned Depth = 0;
  if (FoundDelim)
    *FoundDelim = false;

  unsigned I = 0;
  for (unsigned N = Text.size(); I != N; ++I) {
    if (Text[I] == '{') {
      ++Depth;
      continue;
    }
    if (Depth > 0) {
      if (Text[I] == '}')
        --Depth;
      continue;
    }

    if (Text[I] == Delim) {
      if (FoundDelim)
        *FoundDelim = true;
      break;
    }
  }

  assert(Depth == 0 && "Unbalanced {} set in diagnostic text");
  StringRef Result = Text.substr(0, I);
  Text = Text.substr(I + 1);
  return Result;
}

/// Handle the integer 'select' modifier.  This is used like this:
/// %select{foo|bar|baz}2.  This means that the integer argument "%2" has a
/// value from 0-2.  If the value is 0, the diagnostic prints 'foo'.
/// If the value is 1, it prints 'bar'.  If it has the value 2, it prints 'baz'.
/// This is very useful for certain classes of variant diagnostics.
static void formatSelectionArgument(StringRef ModifierArguments,
                                    ArrayRef<DiagnosticArgument> Args,
                                    unsigned SelectedIndex,
                                    DiagnosticFormatOptions FormatOpts,
                                    llvm::raw_ostream &Out) {
  bool foundPipe = false;
  do {
    assert((!ModifierArguments.empty() || foundPipe) &&
           "Index beyond bounds in %select modifier");
    StringRef Text = skipToDelimiter(ModifierArguments, '|', &foundPipe);
    if (SelectedIndex == 0) {
      DiagnosticFormatting::formatDiagnosticText(Out, Text, Args, FormatOpts);
      break;
    }
    --SelectedIndex;
  } while (true);
}

static bool isInterestingTypealias(Type type) {
  // Dig out the typealias declaration, if there is one.
  TypeAliasDecl *aliasDecl = nullptr;
  if (auto aliasTy = dyn_cast<TypeAliasType>(type.getPointer()))
    aliasDecl = aliasTy->getDecl();
  else
    return false;

  if (aliasDecl == type->getASTContext().getVoidDecl())
    return false;

  // The 'Swift.AnyObject' typealias is not 'interesting'.
  if (aliasDecl->getName() ==
          aliasDecl->getASTContext().getIdentifier("AnyObject") &&
      (aliasDecl->getParentModule()->isStdlibModule() ||
       aliasDecl->getParentModule()->isBuiltinModule())) {
    return false;
  }

  // Compatibility aliases are only interesting insofar as their underlying
  // types are interesting.
  if (aliasDecl->isCompatibilityAlias()) {
    auto underlyingTy = aliasDecl->getUnderlyingTypeLoc().getType();
    return isInterestingTypealias(underlyingTy);
  }

  // Builtin types are never interesting typealiases.
  if (type->is<BuiltinType>())
    return false;

  return true;
}

/// Decide whether to show the desugared type or not.  We filter out some
/// cases to avoid too much noise.
static bool shouldShowAKA(Type type, StringRef typeName) {
  // Canonical types are already desugared.
  if (type->isCanonical())
    return false;

  // Don't show generic type parameters.
  if (type->hasTypeParameter())
    return false;

  // Only show 'aka' if there's a typealias involved; other kinds of sugar
  // are easy enough for people to read on their own.
  if (!type.findIf(isInterestingTypealias))
    return false;

  // If they are textually the same, don't show them.  This can happen when
  // they are actually different types, because they exist in different scopes
  // (e.g. everyone names their type parameters 'T').
  if (typeName == type->getCanonicalType()->getString())
    return false;

  return true;
}

/// If a type is part of an argument list which includes another, distinct type
/// with the same string representation, it should be qualified during
/// formatting.
static bool typeSpellingIsAmbiguous(Type type,
                                    ArrayRef<DiagnosticArgument> Args) {
  for (auto arg : Args) {
    if (arg.getKind() == DiagnosticArgumentKind::Type) {
      auto argType = arg.getAsType();
      if (argType && !argType->isEqual(type) &&
          argType->getWithoutParens().getString() == type.getString()) {
        return true;
      }
    }
  }
  return false;
}

/// Format a single diagnostic argument and write it to the given
/// stream.
static void formatDiagnosticArgument(StringRef Modifier,
                                     StringRef ModifierArguments,
                                     ArrayRef<DiagnosticArgument> Args,
                                     unsigned ArgIndex,
                                     DiagnosticFormatOptions FormatOpts,
                                     llvm::raw_ostream &Out) {
  const DiagnosticArgument &Arg = Args[ArgIndex];
  switch (Arg.getKind()) {
  case DiagnosticArgumentKind::Integer:
    if (Modifier == "select") {
      assert(Arg.getAsInteger() >= 0 && "Negative selection index");
      formatSelectionArgument(ModifierArguments, Args, Arg.getAsInteger(),
                              FormatOpts, Out);
    } else if (Modifier == "s") {
      if (Arg.getAsInteger() != 1)
        Out << 's';
    } else {
      assert(Modifier.empty() && "Improper modifier for integer argument");
      Out << Arg.getAsInteger();
    }
    break;

  case DiagnosticArgumentKind::Unsigned:
    if (Modifier == "select") {
      formatSelectionArgument(ModifierArguments, Args, Arg.getAsUnsigned(),
                              FormatOpts, Out);
    } else if (Modifier == "s") {
      if (Arg.getAsUnsigned() != 1)
        Out << 's';
    } else {
      assert(Modifier.empty() && "Improper modifier for unsigned argument");
      Out << Arg.getAsUnsigned();
    }
    break;

  case DiagnosticArgumentKind::String:
    assert(Modifier.empty() && "Improper modifier for string argument");
    Out << Arg.getAsString();
    break;

  case DiagnosticArgumentKind::Identifier:
    assert(Modifier.empty() && "Improper modifier for identifier argument");
    Out << FormatOpts.OpeningQuotationMark;
    Arg.getAsIdentifier().printPretty(Out);
    Out << FormatOpts.ClosingQuotationMark;
    break;

  case DiagnosticArgumentKind::ObjCSelector:
    assert(Modifier.empty() && "Improper modifier for selector argument");
    Out << FormatOpts.OpeningQuotationMark << Arg.getAsObjCSelector()
        << FormatOpts.ClosingQuotationMark;
    break;

  case DiagnosticArgumentKind::ValueDecl:
    Out << FormatOpts.OpeningQuotationMark;
    Arg.getAsValueDecl()->getFullName().printPretty(Out);
    Out << FormatOpts.ClosingQuotationMark;
    break;

  case DiagnosticArgumentKind::Type: {
    assert(Modifier.empty() && "Improper modifier for Type argument");

    // Strip extraneous parentheses; they add no value.
    auto type = Arg.getAsType()->getWithoutParens();

    // If a type has an unresolved type, print it with syntax sugar removed for
    // clarity. For example, print `Array<_>` instead of `[_]`.
    if (type->hasUnresolvedType()) {
      type = type->getWithoutSyntaxSugar();
    }

    bool isAmbiguous = typeSpellingIsAmbiguous(type, Args);

    if (isAmbiguous && isa<OpaqueTypeArchetypeType>(type.getPointer())) {
      auto opaqueTypeDecl = type->castTo<OpaqueTypeArchetypeType>()->getDecl();

      llvm::SmallString<256> NamingDeclText;
      llvm::raw_svector_ostream OutNaming(NamingDeclText);
      auto namingDecl = opaqueTypeDecl->getNamingDecl();
      if (namingDecl->getDeclContext()->isTypeContext()) {
        auto selfTy = namingDecl->getDeclContext()->getSelfInterfaceType();
        selfTy->print(OutNaming);
        OutNaming << '.';
      }
      namingDecl->getFullName().printPretty(OutNaming);

      auto descriptiveKind = opaqueTypeDecl->getDescriptiveKind();

      Out << llvm::format(FormatOpts.OpaqueResultFormatString.c_str(),
                          type->getString().c_str(),
                          Decl::getDescriptiveKindName(descriptiveKind).data(),
                          NamingDeclText.c_str());

    } else {
      auto printOptions = PrintOptions();
      printOptions.FullyQualifiedTypes = isAmbiguous;
      std::string typeName = type->getString(printOptions);

      if (shouldShowAKA(type, typeName)) {
        llvm::SmallString<256> AkaText;
        llvm::raw_svector_ostream OutAka(AkaText);
        OutAka << type->getCanonicalType();
        Out << llvm::format(FormatOpts.AKAFormatString.c_str(),
                            typeName.c_str(), AkaText.c_str());
      } else {
        Out << FormatOpts.OpeningQuotationMark << typeName
            << FormatOpts.ClosingQuotationMark;
      }
    }
    break;
  }
  case DiagnosticArgumentKind::TypeRepr:
    assert(Modifier.empty() && "Improper modifier for TypeRepr argument");
    Out << FormatOpts.OpeningQuotationMark << Arg.getAsTypeRepr()
        << FormatOpts.ClosingQuotationMark;
    break;
  case DiagnosticArgumentKind::PatternKind:
    assert(Modifier.empty() && "Improper modifier for PatternKind argument");
    Out << Arg.getAsPatternKind();
    break;

  case DiagnosticArgumentKind::SelfAccessKind:
    if (Modifier == "select") {
      formatSelectionArgument(ModifierArguments, Args,
                              unsigned(Arg.getAsSelfAccessKind()), FormatOpts,
                              Out);
    } else {
      assert(Modifier.empty() &&
             "Improper modifier for SelfAccessKind argument");
      Out << Arg.getAsSelfAccessKind();
    }
    break;

  case DiagnosticArgumentKind::ReferenceOwnership:
    if (Modifier == "select") {
      formatSelectionArgument(ModifierArguments, Args,
                              unsigned(Arg.getAsReferenceOwnership()),
                              FormatOpts, Out);
    } else {
      assert(Modifier.empty() &&
             "Improper modifier for ReferenceOwnership argument");
      Out << Arg.getAsReferenceOwnership();
    }
    break;

  case DiagnosticArgumentKind::StaticSpellingKind:
    if (Modifier == "select") {
      formatSelectionArgument(ModifierArguments, Args,
                              unsigned(Arg.getAsStaticSpellingKind()),
                              FormatOpts, Out);
    } else {
      assert(Modifier.empty() &&
             "Improper modifier for StaticSpellingKind argument");
      Out << Arg.getAsStaticSpellingKind();
    }
    break;

  case DiagnosticArgumentKind::DescriptiveDeclKind:
    assert(Modifier.empty() &&
           "Improper modifier for DescriptiveDeclKind argument");
    Out << Decl::getDescriptiveKindName(Arg.getAsDescriptiveDeclKind());
    break;

  case DiagnosticArgumentKind::DeclAttribute:
    assert(Modifier.empty() && "Improper modifier for DeclAttribute argument");
    if (Arg.getAsDeclAttribute()->isDeclModifier())
      Out << FormatOpts.OpeningQuotationMark
          << Arg.getAsDeclAttribute()->getAttrName()
          << FormatOpts.ClosingQuotationMark;
    else
      Out << '@' << Arg.getAsDeclAttribute()->getAttrName();
    break;

  case DiagnosticArgumentKind::VersionTuple:
    assert(Modifier.empty() && "Improper modifier for VersionTuple argument");
    Out << Arg.getAsVersionTuple().getAsString();
    break;
  case DiagnosticArgumentKind::LayoutConstraint:
    assert(Modifier.empty() &&
           "Improper modifier for LayoutConstraint argument");
    Out << FormatOpts.OpeningQuotationMark << Arg.getAsLayoutConstraint()
        << FormatOpts.ClosingQuotationMark;
    break;
  }
}

/// Format the given diagnostic text and place the result in the given
/// buffer.
void DiagnosticFormatting::formatDiagnosticText(
    llvm::raw_ostream &Out, StringRef InText, ArrayRef<DiagnosticArgument> Args,
    DiagnosticFormatOptions FormatOpts) {
  while (!InText.empty()) {
    size_t Percent = InText.find('%');
    if (Percent == StringRef::npos) {
      // Write the rest of the string; we're done.
      Out.write(InText.data(), InText.size());
      break;
    }

    // Write the string up to (but not including) the %, then drop that text
    // (including the %).
    Out.write(InText.data(), Percent);
    InText = InText.substr(Percent + 1);

    // '%%' -> '%'.
    if (InText[0] == '%') {
      Out.write('%');
      InText = InText.substr(1);
      continue;
    }

    // Parse an optional modifier.
    StringRef Modifier;
    {
      size_t Length = InText.find_if_not(isalpha);
      Modifier = InText.substr(0, Length);
      InText = InText.substr(Length);
    }

    if (Modifier == "error") {
      assert(false && "encountered %error in diagnostic text");
      Out << StringRef("<<ERROR>>");
      break;
    }

    // Parse the optional argument list for a modifier, which is brace-enclosed.
    StringRef ModifierArguments;
    if (InText[0] == '{') {
      InText = InText.substr(1);
      ModifierArguments = skipToDelimiter(InText, '}');
    }

    // Find the digit sequence, and parse it into an argument index.
    size_t Length = InText.find_if_not(isdigit);
    unsigned ArgIndex;
    bool Result = InText.substr(0, Length).getAsInteger(10, ArgIndex);
    assert(!Result && "Unparseable argument index value?");
    (void)Result;
    assert(ArgIndex < Args.size() && "Out-of-range argument index");
    InText = InText.substr(Length);

    // Convert the argument to a string.
    formatDiagnosticArgument(Modifier, ModifierArguments, Args, ArgIndex,
                             FormatOpts, Out);
  }
}
