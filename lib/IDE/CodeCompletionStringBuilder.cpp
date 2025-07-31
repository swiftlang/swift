//===--- CodeCompletionStringBuilder.cpp --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/CodeCompletionStringBuilder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/Types.h"
#include "swift/IDE/CodeCompletionStringPrinter.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::ide;

void CodeCompletionStringBuilder::addChunkWithText(
    CodeCompletionString::Chunk::ChunkKind Kind, StringRef Text) {
  addChunkWithTextNoCopy(Kind, Text.copy(Allocator));
}

void CodeCompletionStringBuilder::withNestedGroup(
    CodeCompletionString::Chunk::ChunkKind Kind,
    llvm::function_ref<void()> body) {
  ++CurrentNestingLevel;
  addSimpleChunk(Kind);
  body();
  --CurrentNestingLevel;
}

void CodeCompletionStringBuilder::addCallArgument(
    Identifier Name, Identifier LocalName, Type Ty, Type ContextTy,
    bool IsVarArg, bool IsInOut, bool IsIUO, bool IsAutoClosure,
    bool IsLabeledTrailingClosure, bool IsForOperator, bool HasDefault) {
  ++CurrentNestingLevel;
  using ChunkKind = CodeCompletionString::Chunk::ChunkKind;

  addSimpleChunk(ChunkKind::CallArgumentBegin);

  if (AnnotateResults) {
    llvm::SmallString<16> EscapedKeyword;
    if (!Name.empty()) {
      addChunkWithText(ChunkKind::CallArgumentName,
                       escapeKeyword(Name.str(), false, EscapedKeyword));
      if (!LocalName.empty() && Name != LocalName) {
        addChunkWithTextNoCopy(ChunkKind::Text, " ");
        getLastChunk().setIsAnnotation();
        addChunkWithText(ChunkKind::CallArgumentInternalName,
                         escapeKeyword(LocalName.str(), false, EscapedKeyword));
        getLastChunk().setIsAnnotation();
      }
      addChunkWithTextNoCopy(ChunkKind::CallArgumentColon, ": ");
    } else if (!LocalName.empty()) {
      addChunkWithTextNoCopy(ChunkKind::CallArgumentName, "_");
      getLastChunk().setIsAnnotation();
      addChunkWithTextNoCopy(ChunkKind::Text, " ");
      getLastChunk().setIsAnnotation();
      addChunkWithText(ChunkKind::CallArgumentInternalName,
                       escapeKeyword(LocalName.str(), false, EscapedKeyword));
      addChunkWithTextNoCopy(ChunkKind::CallArgumentColon, ": ");
    } else if (!IsForOperator) {
      addChunkWithTextNoCopy(ChunkKind::CallArgumentName, "_");
      if (!IsLabeledTrailingClosure)
        getLastChunk().setIsAnnotation();
      addChunkWithTextNoCopy(ChunkKind::CallArgumentColon, ": ");
      if (!IsLabeledTrailingClosure)
        getLastChunk().setIsAnnotation();
    }
  } else {
    llvm::SmallString<16> stash;
    ChunkKind nameKind;
    StringRef nameStr;
    if (!Name.empty()) {
      nameKind = ChunkKind::CallArgumentName;
      nameStr = escapeKeyword(Name.str(), false, stash);
    } else if (IsLabeledTrailingClosure) {
      nameKind = ChunkKind::CallArgumentName;
      nameStr = "_";
    } else if (!LocalName.empty()) {
      nameKind = ChunkKind::CallArgumentInternalName;
      nameStr = escapeKeyword(LocalName.str(), false, stash);
    }
    if (!nameStr.empty()) {
      addChunkWithText(nameKind, nameStr);
      addChunkWithTextNoCopy(ChunkKind::CallArgumentColon, ": ");
    }
  }

  // 'inout' arguments are printed specially.
  if (IsInOut) {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::Ampersand,
                           "&");
    Ty = Ty->getInOutObjectType();
  }

  // If the parameter is of the type @autoclosure ()->output, then the
  // code completion should show the parameter of the output type
  // instead of the function type ()->output.
  if (IsAutoClosure) {
    // 'Ty' may be ErrorType.
    if (auto funcTy = Ty->getAs<FunctionType>())
      Ty = funcTy->getResult();
  }

  NonRecursivePrintOptions nrOptions;
  if (IsIUO)
    nrOptions |= NonRecursivePrintOption::ImplicitlyUnwrappedOptional;

  PrintOptions PO;
  PO.SkipAttributes = true;
  PO.OpaqueReturnTypePrinting =
      PrintOptions::OpaqueReturnTypePrintingMode::WithoutOpaqueKeyword;
  if (ContextTy)
    PO.setBaseType(ContextTy);
  if (AnnotateResults) {
    withNestedGroup(ChunkKind::CallArgumentTypeBegin, [&]() {
      CodeCompletionStringPrinter printer(*this);
      auto TL = TypeLoc::withoutLoc(Ty);
      printer.printTypePre(TL);
      Ty->print(printer, PO, nrOptions);
      printer.printTypePost(TL);
    });
  } else {
    std::string TypeName = Ty->getString(PO, nrOptions);
    addChunkWithText(ChunkKind::CallArgumentType, TypeName);
  }

  if (HasDefault) {
    withNestedGroup(ChunkKind::CallArgumentDefaultBegin, []() {
      // Possibly add the actual value in the future
    });
  }

  // Look through optional types and type aliases to find out if we have
  // function type.
  Ty = Ty->lookThroughAllOptionalTypes();
  if (auto AFT = Ty->getAs<AnyFunctionType>()) {
    // If this is a closure type, add ChunkKind::CallArgumentClosureType or
    // ChunkKind::CallArgumentClosureExpr for labeled trailing closures.
    PrintOptions PO;
    PO.PrintFunctionRepresentationAttrs =
        PrintOptions::FunctionRepresentationMode::None;
    PO.SkipAttributes = true;
    PO.OpaqueReturnTypePrinting =
        PrintOptions::OpaqueReturnTypePrintingMode::WithoutOpaqueKeyword;
    PO.AlwaysTryPrintParameterLabels = true;
    if (ContextTy)
      PO.setBaseType(ContextTy);

    if (IsLabeledTrailingClosure) {
      // Expand the closure body.
      SmallString<32> buffer;
      llvm::raw_svector_ostream OS(buffer);

      bool firstParam = true;
      for (const auto &param : AFT->getParams()) {
        if (!firstParam)
          OS << ", ";
        firstParam = false;

        if (param.hasLabel()) {
          OS << param.getLabel();
        } else if (param.hasInternalLabel()) {
          OS << param.getInternalLabel();
        } else {
          OS << "<#";
          if (param.isInOut())
            OS << "inout ";
          OS << param.getPlainType()->getString(PO);
          if (param.isVariadic())
            OS << "...";
          OS << "#>";
        }
      }

      if (!firstParam)
        OS << " in";

      addChunkWithText(
          CodeCompletionString::Chunk::ChunkKind::CallArgumentClosureExpr,
          OS.str());
    } else {
      // Add the closure type.
      addChunkWithText(
          CodeCompletionString::Chunk::ChunkKind::CallArgumentClosureType,
          AFT->getString(PO));
    }
  }

  if (IsVarArg)
    addEllipsis();

  --CurrentNestingLevel;
}

void CodeCompletionStringBuilder::addTypeAnnotation(
    Type T, const PrintOptions &PO, NonRecursivePrintOptions nrOptions,
    StringRef suffix) {
  T = T->getReferenceStorageReferent();

  // Replace '()' with 'Void'.
  if (T->isVoid())
    T = T->getASTContext().getVoidDecl()->getDeclaredInterfaceType();

  if (AnnotateResults) {
    withNestedGroup(CodeCompletionString::Chunk::ChunkKind::TypeAnnotationBegin,
                    [&]() {
                      CodeCompletionStringPrinter printer(*this);
                      auto TL = TypeLoc::withoutLoc(T);
                      printer.printTypePre(TL);
                      T->print(printer, PO, nrOptions);
                      printer.printTypePost(TL);
                      if (!suffix.empty())
                        printer.printText(suffix);
                    });
  } else {
    auto str = T.getString(PO, nrOptions);
    if (!suffix.empty())
      str += suffix.str();
    addTypeAnnotation(str);
  }
}
