//===--- SwiftSyntaxMacro.cpp ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SwiftLangSupport.h"
#include "swift/AST/MacroDefinition.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/TypeContextInfo.h"
#include "swift/IDETool/SyntacticMacroExpansion.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Comment.h"
#include "clang/AST/Decl.h"

using namespace SourceKit;
using namespace swift;
using namespace ide;

void SwiftLangSupport::expandMacroSyntactically(
    llvm::MemoryBuffer *inputBuf, ArrayRef<const char *> args,
    ArrayRef<MacroExpansionInfo> reqExpansions,
    CategorizedEditsReceiver receiver) {

  std::string error;
  auto instance = SyntacticMacroExpansions->getInstance(args, inputBuf, error);
  if (!instance) {
    return receiver(
        RequestResult<ArrayRef<CategorizedEdits>>::fromError(error));
  }
  auto &ctx = instance->getASTContext();

  // Convert 'SourceKit::MacroExpansionInfo' to 'ide::MacroExpansionSpecifier'.
  SmallVector<ide::MacroExpansionSpecifier, 4> expansions;
  for (auto &req : reqExpansions) {
    unsigned offset = req.offset;

    swift::MacroRoles macroRoles;
#define MACRO_ROLE(Name, Description)                   \
    if (req.roles.contains(SourceKit::MacroRole::Name)) \
      macroRoles |= swift::MacroRole::Name;
#include "swift/Basic/MacroRoles.def"

    MacroDefinition definition = [&] {
      if (auto *expanded =
              std::get_if<MacroExpansionInfo::ExpandedMacroDefinition>(
                  &req.macroDefinition)) {
        SmallVector<ExpandedMacroReplacement, 2> replacements;
        for (auto &reqReplacement : expanded->replacements) {
          replacements.push_back(
              {/*startOffset=*/reqReplacement.range.Offset,
               /*endOffset=*/reqReplacement.range.Offset +
                   reqReplacement.range.Length,
               /*parameterIndex=*/reqReplacement.parameterIndex});
        }
        SmallVector<ExpandedMacroReplacement, 2> genericReplacements;
        for (auto &genReqReplacement : expanded->genericReplacements) {
          genericReplacements.push_back(
              {/*startOffset=*/genReqReplacement.range.Offset,
               /*endOffset=*/genReqReplacement.range.Offset +
                   genReqReplacement.range.Length,
               /*parameterIndex=*/genReqReplacement.parameterIndex});
        }
        return MacroDefinition::forExpanded(ctx, expanded->expansionText,
                                            replacements, genericReplacements);
      } else if (auto *externalRef =
                     std::get_if<MacroExpansionInfo::ExternalMacroReference>(
                         &req.macroDefinition)) {
        return MacroDefinition::forExternal(
            ctx.getIdentifier(externalRef->moduleName),
            ctx.getIdentifier(externalRef->typeName));
      } else {
        return MacroDefinition::forUndefined();
      }
    }();

    expansions.push_back({offset, macroRoles, definition});
  }

  RequestRefactoringEditConsumer consumer(receiver);
  instance->expandAll(expansions, consumer);
  // consumer automatically send the results on destruction.
}
