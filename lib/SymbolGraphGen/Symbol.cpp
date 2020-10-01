//===--- Symbol.cpp - Symbol Graph Node -----------------------------------===//
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

#include "swift/AST/ASTContext.h"
#include "swift/AST/Comment.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/SourceManager.h"
#include "AvailabilityMixin.h"
#include "JSON.h"
#include "Symbol.h"
#include "SymbolGraph.h"
#include "SymbolGraphASTWalker.h"

using namespace swift;
using namespace symbolgraphgen;

Symbol::Symbol(SymbolGraph *Graph, const ValueDecl *VD,
               const NominalTypeDecl *SynthesizedBaseTypeDecl)
: Graph(Graph),
  VD(VD),
  SynthesizedBaseTypeDecl(SynthesizedBaseTypeDecl) {}

void Symbol::serializeKind(StringRef Identifier, StringRef DisplayName,
                           llvm::json::OStream &OS) const {
  OS.object([&](){
    OS.attribute("identifier", Identifier);
    OS.attribute("displayName", DisplayName);
  });
}

void Symbol::serializeKind(llvm::json::OStream &OS) const {
  AttributeRAII A("kind", OS);
  switch (VD->getKind()) {
  case swift::DeclKind::Class:
    serializeKind("swift.class", "Class", OS);
    break;
  case swift::DeclKind::Struct:
    serializeKind("swift.struct", "Structure", OS);
    break;
  case swift::DeclKind::Enum:
    serializeKind("swift.enum", "Enumeration", OS);
    break;
  case swift::DeclKind::EnumElement:
    serializeKind("swift.enum.case", "Case", OS);
    break;
  case swift::DeclKind::Protocol:
    serializeKind("swift.protocol", "Protocol", OS);
    break;
  case swift::DeclKind::Constructor:
    serializeKind("swift.init", "Initializer", OS);
    break;
  case swift::DeclKind::Destructor:
    serializeKind("swift.deinit", "Deinitializer", OS);
    break;
  case swift::DeclKind::Func:
    if (VD->isOperator()) {
      serializeKind("swift.func.op", "Operator", OS);
    } else if (VD->isStatic()) {
      serializeKind("swift.type.method", "Type Method", OS);
    } else if (VD->getDeclContext()->getSelfNominalTypeDecl()){
      serializeKind("swift.method", "Instance Method", OS);
    } else {
      serializeKind("swift.func", "Function", OS);
    }
    break;
  case swift::DeclKind::Var:
    if (VD->isStatic()) {
      serializeKind("swift.type.property", "Type Property", OS);
    } else if (VD->getDeclContext()->getSelfNominalTypeDecl()) {
      serializeKind("swift.property", "Instance Property", OS);
    } else {
      serializeKind("swift.var", "Global Variable", OS);
    }
    break;
  case swift::DeclKind::Subscript:
    if (VD->isStatic()) {
      serializeKind("swift.type.subscript", "Type Subscript", OS);
    } else {
      serializeKind("swift.subscript", "Instance Subscript", OS);
    }
    break;
  case swift::DeclKind::TypeAlias:
    serializeKind("swift.typealias", "Type Alias", OS);
    break;
  case swift::DeclKind::AssociatedType:
    serializeKind("swift.associatedtype", "Associated Type", OS);
    break;
  default:
    llvm::errs() << "Unsupported kind: " << VD->getKindName(VD->getKind());
    llvm_unreachable("Unsupported declaration kind for symbol graph");
  }
}

void Symbol::serializeIdentifier(llvm::json::OStream &OS) const {
  OS.attributeObject("identifier", [&](){
    SmallString<256> USR;
    getUSR(USR);
    OS.attribute("precise", USR.str());
    OS.attribute("interfaceLanguage", "swift");
  });
}

void Symbol::serializePathComponents(llvm::json::OStream &OS) const {
  OS.attributeArray("pathComponents", [&](){
    SmallVector<SmallString<32>, 8> PathComponents;
    getPathComponents(PathComponents);
    for (auto Component : PathComponents) {
      OS.value(Component);
    }
  });
}

void Symbol::serializeNames(llvm::json::OStream &OS) const {
  OS.attributeObject("names", [&](){
    SmallVector<SmallString<32>, 8> PathComponents;
    getPathComponents(PathComponents);

    if (isa<GenericTypeDecl>(VD)) {    
      SmallString<64> FullyQualifiedTitle;

      for (const auto *It = PathComponents.begin(); It != PathComponents.end(); ++It) {
        if (It != PathComponents.begin()) {
          FullyQualifiedTitle.push_back('.');
        }
        FullyQualifiedTitle.append(*It);
      }
      
      OS.attribute("title", FullyQualifiedTitle.str());
    } else {
      OS.attribute("title", PathComponents.back());
    }

    Graph->serializeNavigatorDeclarationFragments("navigator", *this, OS);
    Graph->serializeSubheadingDeclarationFragments("subHeading", *this, OS);
    // "prose": null
  });
}

void Symbol::serializePosition(StringRef Key, SourceLoc Loc,
                               SourceManager &SourceMgr,
                               llvm::json::OStream &OS) const {
  // Note: Line and columns are zero-based in this serialized format.
  auto LineAndColumn = SourceMgr.getPresumedLineAndColumnForLoc(Loc);
  auto Line = LineAndColumn.first - 1;
  auto Column = LineAndColumn.second - 1;

  OS.attributeObject(Key, [&](){
    OS.attribute("line", Line);
    OS.attribute("character", Column);
  });
}

void Symbol::serializeRange(size_t InitialIndentation,
                            SourceRange Range, SourceManager &SourceMgr,
                            llvm::json::OStream &OS) const {
  OS.attributeObject("range", [&](){
    // Note: Line and columns in the serialized format are zero-based.
    auto Start = Range.Start.getAdvancedLoc(InitialIndentation);
    serializePosition("start", Start, SourceMgr, OS);

    auto End = SourceMgr.isBeforeInBuffer(Range.End, Start)
      ? Start
      : Range.End;
    serializePosition("end", End, SourceMgr, OS);
  });
}

void Symbol::serializeDocComment(llvm::json::OStream &OS) const {
  const auto *DocCommentProvidingDecl =
      dyn_cast_or_null<ValueDecl>(
          getDocCommentProvidingDecl(VD, /*AllowSerialized=*/true));
  if (!DocCommentProvidingDecl) {
    DocCommentProvidingDecl = VD;
  }
  auto RC = DocCommentProvidingDecl->getRawComment(/*SerializedOK=*/true);
  if (RC.isEmpty()) {
    return;
  }

  OS.attributeObject("docComment", [&](){
    auto LL = Graph->Ctx.getLineList(RC);
    StringRef FirstNonBlankLine;
    for (const auto &Line : LL.getLines()) {
      if (!Line.Text.empty()) {
        FirstNonBlankLine = Line.Text;
        break;
      }
    }
    size_t InitialIndentation = FirstNonBlankLine.empty()
      ? 0
      : markup::measureIndentation(FirstNonBlankLine);
    OS.attributeArray("lines", [&](){
      for (const auto &Line : LL.getLines()) {
        // Line object
        OS.object([&](){
          // Trim off any initial indentation from the line's
          // text and start of its source range, if it has one.
          if (Line.Range.isValid()) {
            serializeRange(std::min(InitialIndentation,
                                    Line.FirstNonspaceOffset),
                           Line.Range, Graph->M.getASTContext().SourceMgr, OS);
          }
          auto TrimmedLine = Line.Text.drop_front(std::min(InitialIndentation,
                                                  Line.FirstNonspaceOffset));
          OS.attribute("text", TrimmedLine);
        });
      }
    }); // end lines: []
  }); // end docComment:
}

void Symbol::serializeFunctionSignature(llvm::json::OStream &OS) const {
  if (const auto *FD = dyn_cast_or_null<FuncDecl>(VD)) {
    OS.attributeObject("functionSignature", [&](){

      // Parameters
      if (const auto *ParamList = FD->getParameters()) {
        if (ParamList->size()) {
          OS.attributeArray("parameters", [&](){
            for (const auto *Param : *ParamList) {
              auto ExternalName = Param->getArgumentName().str();
              auto InternalName = Param->getParameterName().str();

              OS.object([&](){
                if (ExternalName.empty()) {
                  OS.attribute("name", InternalName);
                } else {
                  OS.attribute("name", ExternalName);
                  if (ExternalName != InternalName &&
                      !InternalName.empty()) {
                    OS.attribute("internalName", InternalName);
                  }
                }
                Graph->serializeDeclarationFragments("declarationFragments",
                                                     Symbol(Graph, Param,
                                                            nullptr), OS);
              }); // end parameter object
            }
          }); // end parameters:
        }
      }

      // Returns
      if (const auto ReturnType = FD->getResultInterfaceType()) {
        Graph->serializeDeclarationFragments("returns", ReturnType, OS);
      }
    });
  }
}

void Symbol::serializeSwiftGenericMixin(llvm::json::OStream &OS) const {
  if (const auto *GC = VD->getAsGenericContext()) {
    if (const auto Generics = GC->getGenericSignature()) {

      SmallVector<const GenericTypeParamType *, 4> FilteredParams;
      SmallVector<Requirement, 4> FilteredRequirements;
      for (const auto Param : Generics->getGenericParams()) {
        if (const auto *D = Param->getDecl()) {
          if (D->isImplicit()) {
            continue;
          }
          FilteredParams.push_back(Param);
        }
      }

      const auto *Self = dyn_cast<NominalTypeDecl>(VD);
      if (!Self) {
        Self = VD->getDeclContext()->getSelfNominalTypeDecl();
      }

      filterGenericRequirements(Generics->getRequirements(),
                                Self,
                                FilteredRequirements);

      if (FilteredParams.empty() && FilteredRequirements.empty()) {
        return;
      }

      OS.attributeObject("swiftGenerics", [&](){
        if (!FilteredParams.empty()) {
          OS.attributeArray("parameters", [&](){
            for (const auto *Param : FilteredParams) {
              ::serialize(Param, OS);
            }
          }); // end parameters:
        }

        if (!FilteredRequirements.empty()) {
          OS.attributeArray("constraints", [&](){
            for (const auto &Req : FilteredRequirements) {
              ::serialize(Req, OS);
            }
          }); // end constraints:
        }
      }); // end swiftGenerics:
    }
  }
}

void Symbol::serializeSwiftExtensionMixin(llvm::json::OStream &OS) const {
  if (const auto *Extension
          = dyn_cast_or_null<ExtensionDecl>(VD->getDeclContext())) {
    ::serialize(Extension, OS);
  }
}

void Symbol::serializeDeclarationFragmentMixin(llvm::json::OStream &OS) const {
  Graph->serializeDeclarationFragments("declarationFragments", *this, OS);
}

void Symbol::serializeAccessLevelMixin(llvm::json::OStream &OS) const {
  OS.attribute("accessLevel", getAccessLevelSpelling(VD->getFormalAccess()));
}

void Symbol::serializeLocationMixin(llvm::json::OStream &OS) const {
  auto Loc = VD->getLoc(/*SerializedOK=*/true);
  if (Loc.isInvalid()) {
    return;
  }
  auto FileName = VD->getASTContext().SourceMgr.getDisplayNameForLoc(Loc);
  if (FileName.empty()) {
    return;
  }
  OS.attributeObject("location", [&](){
    SmallString<1024> FileURI("file://");
    FileURI.append(FileName);
    OS.attribute("uri", FileURI.str());
    serializePosition("position", Loc, Graph->M.getASTContext().SourceMgr, OS);
  });
}

namespace {
/// Get the availabilities for each domain on a declaration without walking
/// up the parent hierarchy.
///
/// \param D The declaration whose availabilities the method will collect.
/// \param Availabilities The domain -> availability map that will be updated.
/// \param IsParent If \c true\c, will update or fill availabilities for a given
/// domain with different "inheriting" rules rather than filling from
/// duplicate \c \@available attributes on the same declaration.
void getAvailabilities(const Decl *D,
                       llvm::StringMap<Availability> &Availabilities,
                       bool IsParent) {
  // DeclAttributes is a linked list in reverse order from where they
  // appeared in the source. Let's re-reverse them.
  SmallVector<const AvailableAttr *, 4> AvAttrs;
  for (const auto *Attr : D->getAttrs()) {
    if (const auto *AvAttr = dyn_cast<AvailableAttr>(Attr)) {
      AvAttrs.push_back(AvAttr);
    }
  }
  std::reverse(AvAttrs.begin(), AvAttrs.end());

  // Now go through them in source order.
  for (auto *AvAttr : AvAttrs) {
    Availability NewAvailability(*AvAttr);
    if (NewAvailability.empty()) {
      continue;
    }
    auto ExistingAvailability = Availabilities.find(NewAvailability.Domain);
    if (ExistingAvailability != Availabilities.end()) {
      // There are different rules for filling in missing components
      // or replacing existing components from a parent's @available
      // attribute compared to duplicate @available attributes on the
      // same declaration.
      // See the respective methods below for an explanation for the
      // replacement/filling rules.
      if (IsParent) {
        ExistingAvailability->getValue().updateFromParent(NewAvailability);
      } else {
        ExistingAvailability->getValue().updateFromDuplicate(NewAvailability);
      }
    } else {
      // There are no availabilities for this domain yet, so either
      // inherit the parent's in its entirety or set it from this declaration.
      Availabilities.insert(std::make_pair(NewAvailability.Domain,
                                           NewAvailability));
    }
  }
}

/// Get the availabilities of a declaration, considering all of its
/// parent context's except for the module.
void getInheritedAvailabilities(const Decl *D,
llvm::StringMap<Availability> &Availabilities) {
  getAvailabilities(D, Availabilities, /*IsParent*/false);

  auto CurrentContext = D->getDeclContext();
  while (CurrentContext) {
    if (const auto *Parent = CurrentContext->getAsDecl()) {
      if (isa<ModuleDecl>(Parent)) {
        return;
      }
      getAvailabilities(Parent, Availabilities, /*IsParent*/true);
    }
    CurrentContext = CurrentContext->getParent();
  }
}

} // end anonymous namespace

void Symbol::serializeAvailabilityMixin(llvm::json::OStream &OS) const {
  llvm::StringMap<Availability> Availabilities;
  getInheritedAvailabilities(VD, Availabilities);

  if (Availabilities.empty()) {
    return;
  }

  OS.attributeArray("availability", [&]{
    for (const auto &Availability : Availabilities) {
      Availability.getValue().serialize(OS);
    }
  });
}

void Symbol::serialize(llvm::json::OStream &OS) const {
  OS.object([&](){
    serializeKind(OS);
    serializeIdentifier(OS);
    serializePathComponents(OS);
    serializeNames(OS);
    serializeDocComment(OS);

    // "Mixins"
    serializeFunctionSignature(OS);
    serializeSwiftGenericMixin(OS);
    serializeSwiftExtensionMixin(OS);
    serializeDeclarationFragmentMixin(OS);
    serializeAccessLevelMixin(OS);
    serializeAvailabilityMixin(OS);
    serializeLocationMixin(OS);
  });
}

void
Symbol::getPathComponents(SmallVectorImpl<SmallString<32>> &Components) const {

  auto collectPathComponents = [&](const ValueDecl *Decl,
                                   SmallVectorImpl<SmallString<32>> &DeclComponents) {
    // Collect the spellings of the fully qualified identifier components.
    while (Decl && !isa<ModuleDecl>(Decl)) {
      SmallString<32> Scratch;
      Decl->getName().getString(Scratch);
      DeclComponents.push_back(Scratch);
      if (const auto *DC = Decl->getDeclContext()) {
        if (const auto *Nominal = DC->getSelfNominalTypeDecl()) {
          Decl = Nominal;
        } else {
          Decl = dyn_cast_or_null<ValueDecl>(DC->getAsDecl());
        }
      } else {
        Decl = nullptr;
      }
    }
  };

  if (const auto BaseTypeDecl = getSynthesizedBaseTypeDecl()) {
    // This is a synthesized member of some base type declaration, actually
    // existing on another type, such as a default implementation of
    // a protocol. Build a path as if it were defined in the base type.
    SmallString<32> LastPathComponent;
    VD->getName().getString(LastPathComponent);
    Components.push_back(LastPathComponent);
    collectPathComponents(BaseTypeDecl, Components);
  } else {
    // Otherwise, this is just a normal declaration, so we can build
    // its path normally.
    collectPathComponents(VD, Components);
  }

  // The list is leaf-to-root, but we want root-to-leaf, so reverse it.
  std::reverse(Components.begin(), Components.end());
}

void Symbol::printPath(llvm::raw_ostream &OS) const {
  SmallVector<SmallString<32>, 8> Components;
  getPathComponents(Components);
  for (auto it = Components.begin(); it != Components.end(); ++it) {
    if (it != Components.begin()) {
      OS << '.';
    }
    OS << it->str();
  }
}

void Symbol::getUSR(SmallVectorImpl<char> &USR) const {
  llvm::raw_svector_ostream OS(USR);
  ide::printDeclUSR(VD, OS);
  if (SynthesizedBaseTypeDecl) {
    OS << "::SYNTHESIZED::";
    ide::printDeclUSR(SynthesizedBaseTypeDecl, OS);
  }
}
