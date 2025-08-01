//===-------- ConstExtract.cpp -- Gather Compile-Time-Known Values --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/ConstExtract/ConstExtract.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/TypeID.h"
#include "swift/ConstExtract/ConstExtractRequests.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"

#include <set>
#include <sstream>
#include <string>

using namespace swift;

namespace {
/// A helper class to collect all nominal type declarations that conform to
/// specific protocols provided as input.
class NominalTypeConformanceCollector : public ASTWalker {
  const std::unordered_set<std::string> &Protocols;
  std::vector<NominalTypeDecl *> &ConformanceTypeDecls;

public:
  NominalTypeConformanceCollector(
      const std::unordered_set<std::string> &Protocols,
      std::vector<NominalTypeDecl *> &ConformanceDecls)
      : Protocols(Protocols), ConformanceTypeDecls(ConformanceDecls) {}

  MacroWalking getMacroWalkingBehavior() const override {
    return MacroWalking::ArgumentsAndExpansion;
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    auto *NTD = llvm::dyn_cast<NominalTypeDecl>(D);
    if (!NTD)
      if (auto *ETD = dyn_cast<ExtensionDecl>(D))
        NTD = ETD->getExtendedNominal();
    if (NTD)
      if (!isa<ProtocolDecl>(NTD) && CheckedDecls.insert(NTD).second) {
        if (NTD->getAttrs().hasAttribute<ExtractConstantsFromMembersAttr>()) {
          ConformanceTypeDecls.push_back(NTD);
          goto visitAuxiliaryDecls;
        }

        for (auto &Protocol : NTD->getAllProtocols())
          if (Protocol->getAttrs()
                  .hasAttribute<ExtractConstantsFromMembersAttr>() ||
              Protocols.count(Protocol->getName().str().str()) != 0) {
            ConformanceTypeDecls.push_back(NTD);
            goto visitAuxiliaryDecls;
          }
      }
  visitAuxiliaryDecls:
    // Visit peers expanded from macros
    D->visitAuxiliaryDecls([&](Decl *decl) { decl->walk(*this); },
                           /*visitFreestandingExpanded=*/false);
    return Action::Continue();
  }

private:
  std::unordered_set<NominalTypeDecl *> CheckedDecls;
};

std::string toFullyQualifiedTypeNameString(const swift::Type &Type) {
  std::string TypeNameOutput;
  llvm::raw_string_ostream OutputStream(TypeNameOutput);
  swift::PrintOptions Options;
  Options.FullyQualifiedTypes = true;
  Options.PreferTypeRepr = true;
  Options.AlwaysDesugarArraySliceTypes = true;
  Options.AlwaysDesugarInlineArrayTypes = true;
  Options.AlwaysDesugarDictionaryTypes = true;
  Options.AlwaysDesugarOptionalTypes = true;
  Options.PrintTypeAliasUnderlyingType = true;
  Options.OpaqueReturnTypePrinting =
    PrintOptions::OpaqueReturnTypePrintingMode::WithOpaqueKeyword;
  Type.print(OutputStream, Options);
  OutputStream.flush();
  return TypeNameOutput;
}

std::string toFullyQualifiedProtocolNameString(const swift::ProtocolDecl &Protocol) {
  // Protocols cannot be nested in other declarations, so the only fully-qualified
  // context is the declaring module name.
  return Protocol.getParentModule()->getNameStr().str() + "." + Protocol.getNameStr().str();
}

std::string toMangledTypeNameString(const swift::Type &Type) {
  auto PrintingType = Type;
  if (Type->hasArchetype())
    PrintingType = Type->mapTypeOutOfContext();
  return Mangle::ASTMangler(Type->getASTContext()).mangleTypeWithoutPrefix(PrintingType->getCanonicalType());
}

} // namespace

namespace swift {

bool
parseProtocolListFromFile(StringRef protocolListFilePath,
                          DiagnosticEngine &diags,
                          std::unordered_set<std::string> &protocols) {
  // Load the input file.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
      llvm::MemoryBuffer::getFile(protocolListFilePath);
  if (!FileBufOrErr) {
    diags.diagnose(SourceLoc(),
                   diag::const_extract_protocol_list_input_file_missing,
                   protocolListFilePath);
    return false;
  }

  // Parse a JSON file containing a list of the format:
  // [proto1, proto2, proto3]
  bool ParseFailed = false;
  {
    StringRef Buffer = FileBufOrErr->get()->getBuffer();
    llvm::SourceMgr SM;
    llvm::yaml::Stream S(Buffer, SM);
    llvm::yaml::SequenceNode *Sequence =
        dyn_cast<llvm::yaml::SequenceNode>(S.begin()->getRoot());
    if (Sequence) {
      for (auto &ProtocolNameNode : *Sequence) {
        auto *ScalarNode = dyn_cast<llvm::yaml::ScalarNode>(&ProtocolNameNode);
        if (!ScalarNode) {
          ParseFailed = true;
          break;
        }
        auto protocolNameStr = ScalarNode->getRawValue().str();
        if (protocolNameStr.front() == '"' && protocolNameStr.back() == '"')
          protocolNameStr = protocolNameStr.substr(1, protocolNameStr.size() - 2);
        protocols.insert(protocolNameStr);
      }
    } else
      ParseFailed = true;
  }

  if (ParseFailed) {
    diags.diagnose(SourceLoc(),
                   diag::const_extract_protocol_list_input_file_corrupted,
                   protocolListFilePath);
    return false;
  }
  return true;
}

std::vector<std::shared_ptr<BuilderValue::BuilderMember>>
getResultBuilderMembersFromBraceStmt(BraceStmt *braceStmt,
                                     const DeclContext *declContext);

static std::shared_ptr<CompileTimeValue>
extractCompileTimeValue(Expr *expr, const DeclContext *declContext);

static std::vector<FunctionParameter>
extractFunctionArguments(const ArgumentList *args,
                         const DeclContext *declContext) {
  std::vector<FunctionParameter> parameters;

  for (auto arg : *args) {
    auto argExpr = arg.getExpr();
    const auto label = arg.getLabel().str().str();
    const auto type = argExpr->getType();
    if (auto defaultArgument = dyn_cast<DefaultArgumentExpr>(argExpr)) {
      auto *decl = defaultArgument->getParamDecl();
      if (decl->hasDefaultExpr()) {
        argExpr = decl->getTypeCheckedDefaultExpr();
      }
    } else if (auto optionalInject = dyn_cast<InjectIntoOptionalExpr>(argExpr)) {
      argExpr = optionalInject->getSubExpr();
    }
    parameters.push_back(
        {label, type, extractCompileTimeValue(argExpr, declContext)});
  }

  return parameters;
}

static std::optional<std::string> extractRawLiteral(Expr *expr) {
  if (expr) {
    switch (expr->getKind()) {
    case ExprKind::BooleanLiteral:
    case ExprKind::FloatLiteral:
    case ExprKind::IntegerLiteral: {
      std::string literalOutput;
      llvm::raw_string_ostream OutputStream(literalOutput);
      expr->printConstExprValue(&OutputStream, nullptr);
      if (!literalOutput.empty()) {
        return literalOutput;
      }
      break;
    }

    case ExprKind::StringLiteral: {
      auto stringLiteralExpression = cast<StringLiteralExpr>(expr);
      std::string literalOutput;
      llvm::raw_string_ostream OutputStream(literalOutput);
      OutputStream << stringLiteralExpression->getValue();
      return literalOutput;
    }

    default:
      break;
    }
  }
  return std::nullopt;
}

static std::shared_ptr<CompileTimeValue>
extractCompileTimeValue(Expr *expr, const DeclContext *declContext) {
  if (expr) {
    switch (expr->getKind()) {
    case ExprKind::BooleanLiteral:
    case ExprKind::FloatLiteral:
    case ExprKind::IntegerLiteral:
    case ExprKind::StringLiteral: {
      auto rawLiteral = extractRawLiteral(expr);
      if (rawLiteral.has_value()) {
        return std::make_shared<RawLiteralValue>(rawLiteral.value());
      }

      break;
    }

    case ExprKind::NilLiteral: {
      return std::make_shared<NilLiteralValue>();
    }

    case ExprKind::Array: {
      auto arrayExpr = cast<ArrayExpr>(expr);
      std::vector<std::shared_ptr<CompileTimeValue>> elementValues;
      for (const auto elementExpr : arrayExpr->getElements()) {
        elementValues.push_back(
            extractCompileTimeValue(elementExpr, declContext));
      }
      return std::make_shared<ArrayValue>(elementValues);
    }

    case ExprKind::Dictionary: {
      auto dictionaryExpr = cast<DictionaryExpr>(expr);
      std::vector<std::shared_ptr<TupleValue>> tuples;
      for (auto elementExpr : dictionaryExpr->getElements()) {
        auto elementValue = extractCompileTimeValue(elementExpr, declContext);
        if (isa<TupleValue>(elementValue.get())) {
          tuples.push_back(std::static_pointer_cast<TupleValue>(elementValue));
        }
      }
      return std::make_shared<DictionaryValue>(tuples);
    }

    case ExprKind::Tuple: {
      auto tupleExpr = cast<TupleExpr>(expr);

      std::vector<TupleElement> elements;
      if (tupleExpr->hasElementNames()) {
        for (auto pair : llvm::zip(tupleExpr->getElements(),
                                   tupleExpr->getElementNames())) {
          auto elementExpr = std::get<0>(pair);
          auto elementName = std::get<1>(pair);

          std::optional<std::string> label =
              elementName.empty()
                  ? std::nullopt
                  : std::optional<std::string>(elementName.str().str());

          elements.push_back(
              {label, elementExpr->getType(),
               extractCompileTimeValue(elementExpr, declContext)});
        }
      } else {
        for (auto elementExpr : tupleExpr->getElements()) {
          elements.push_back(
              {std::nullopt, elementExpr->getType(),
               extractCompileTimeValue(elementExpr, declContext)});
        }
      }
      return std::make_shared<TupleValue>(elements);
    }

    case ExprKind::Call: {
      auto callExpr = cast<CallExpr>(expr);
      auto functionKind = callExpr->getFn()->getKind();

      if (functionKind == ExprKind::DeclRef) {
        auto declRefExpr = cast<DeclRefExpr>(callExpr->getFn());
        auto identifier =
            declRefExpr->getDecl()->getName().getBaseIdentifier().str().str();

        std::vector<FunctionParameter> parameters =
            extractFunctionArguments(callExpr->getArgs(), declContext);
        return std::make_shared<FunctionCallValue>(identifier, parameters);
      }

      if (functionKind == ExprKind::ConstructorRefCall) {
        std::vector<FunctionParameter> parameters =
            extractFunctionArguments(callExpr->getArgs(), declContext);
        return std::make_shared<InitCallValue>(callExpr->getType(), parameters);
      }

      if (functionKind == ExprKind::DotSyntaxCall) {
        auto dotSyntaxCallExpr = cast<DotSyntaxCallExpr>(callExpr->getFn());
        auto fn = dotSyntaxCallExpr->getFn();
        if (fn->getKind() == ExprKind::DeclRef) {
          auto declRefExpr = cast<DeclRefExpr>(fn);
          auto baseIdentifierName =
              declRefExpr->getDecl()->getName().getBaseIdentifier().str().str();

          std::vector<FunctionParameter> parameters =
              extractFunctionArguments(callExpr->getArgs(), declContext);

          auto declRef = dotSyntaxCallExpr->getFn()->getReferencedDecl();
          switch (declRef.getDecl()->getKind()) {
          case DeclKind::EnumElement: {
            return std::make_shared<EnumValue>(baseIdentifierName, parameters);
          }

          case DeclKind::Func: {
            auto identifier = declRefExpr->getDecl()
                                  ->getName()
                                  .getBaseIdentifier()
                                  .str()
                                  .str();

            return std::make_shared<StaticFunctionCallValue>(
                identifier, callExpr->getType(), parameters);
          }

          default: {
            break;
          }
          }
        }
      }

      break;
    }

    case ExprKind::DotSyntaxCall: {
      auto dotSyntaxCallExpr = cast<DotSyntaxCallExpr>(expr);
      auto fn = dotSyntaxCallExpr->getFn();
      if (fn->getKind() == ExprKind::DeclRef) {
        auto declRefExpr = cast<DeclRefExpr>(fn);
        auto caseName =
            declRefExpr->getDecl()->getName().getBaseIdentifier().str().str();
        return std::make_shared<EnumValue>(caseName, std::nullopt);
      }

      break;
    }

    case ExprKind::Erasure: {
      auto erasureExpr = cast<ErasureExpr>(expr);
      return extractCompileTimeValue(erasureExpr->getSubExpr(), declContext);
    }

    case ExprKind::Paren: {
      auto parenExpr = cast<ParenExpr>(expr);
      return extractCompileTimeValue(parenExpr->getSubExpr(), declContext);
    }

    case ExprKind::PropertyWrapperValuePlaceholder: {
      auto placeholderExpr = cast<PropertyWrapperValuePlaceholderExpr>(expr);
      return extractCompileTimeValue(placeholderExpr->getOriginalWrappedValue(),
                                     declContext);
    }

    case ExprKind::Coerce: {
      auto coerceExpr = cast<CoerceExpr>(expr);
      return extractCompileTimeValue(coerceExpr->getSubExpr(), declContext);
    }

    case ExprKind::DotSelf: {
      auto dotSelfExpr = cast<DotSelfExpr>(expr);
      auto dotSelfMetaType = dotSelfExpr->getType()->getAs<AnyMetatypeType>();
      if (dotSelfMetaType)
        return std::make_shared<TypeValue>(dotSelfMetaType->getInstanceType());
      else
        break;
    }

    case ExprKind::UnderlyingToOpaque: {
      auto underlyingToOpaque = cast<UnderlyingToOpaqueExpr>(expr);
      return extractCompileTimeValue(underlyingToOpaque->getSubExpr(),
                                     declContext);
    }

    case ExprKind::DefaultArgument: {
      auto defaultArgExpr = cast<DefaultArgumentExpr>(expr);
      auto *decl = defaultArgExpr->getParamDecl();
      // If there is a default expr, we should have looked through to it
      assert(!decl->hasDefaultExpr());
      switch (decl->getDefaultArgumentKind()) {
      case DefaultArgumentKind::NilLiteral:
        return std::make_shared<NilLiteralValue>();
      case DefaultArgumentKind::EmptyArray:
        return std::make_shared<ArrayValue>(
            std::vector<std::shared_ptr<CompileTimeValue>>());
      case DefaultArgumentKind::EmptyDictionary:
        return std::make_shared<DictionaryValue>(
            std::vector<std::shared_ptr<TupleValue>>());
      default:
        break;
      }
    } break;

    case ExprKind::KeyPath: {
        auto keyPathExpr = cast<KeyPathExpr>(expr);

        auto rootType = keyPathExpr->getRootType();
        std::vector<KeyPathValue::Component> components;

        for (auto component: keyPathExpr->getComponents()) {
            if (component.isResolved()) {
                auto declRef = component.getDeclRef();
                auto identifier = declRef.getDecl()->getBaseIdentifier().str();
                auto type = component.getComponentType()->getRValueType();
                components.push_back({identifier.str(), type});
            }
        }

        std::string path = "";
        auto numberOfComponents = static_cast<int>(components.size());
        for (int i = 0; i < numberOfComponents; i++) {
            if (i != 0) {
                path += ".";
            }
            path += components[i].Label;
        }

        return std::make_shared<KeyPathValue>(path, rootType, components);
    }

    case ExprKind::InjectIntoOptional: {
      auto injectIntoOptionalExpr = cast<InjectIntoOptionalExpr>(expr);
      return extractCompileTimeValue(injectIntoOptionalExpr->getSubExpr(),
                                     declContext);
    }

    case ExprKind::Load: {
      auto loadExpr = cast<LoadExpr>(expr);
      return extractCompileTimeValue(loadExpr->getSubExpr(), declContext);
    }

    case ExprKind::MemberRef: {
      auto memberExpr = cast<MemberRefExpr>(expr);
      if (isa<TypeExpr>(memberExpr->getBase())) {
        auto baseTypeExpr = cast<TypeExpr>(memberExpr->getBase());
        auto label = memberExpr->getDecl().getDecl()->getBaseIdentifier().str();
        return std::make_shared<MemberReferenceValue>(
            baseTypeExpr->getInstanceType(), label.str());
      }
      break;
    }

    case ExprKind::InterpolatedStringLiteral: {
      auto interpolatedStringExpr = cast<InterpolatedStringLiteralExpr>(expr);
      auto tapExpr = interpolatedStringExpr->getAppendingExpr();
      auto &Ctx = tapExpr->getVar()->getASTContext();

      std::vector<std::shared_ptr<CompileTimeValue>> segments;
      interpolatedStringExpr->forEachSegment(
          Ctx, [&](bool isInterpolation, CallExpr *segment) -> void {
            auto arg = segment->getArgs()->get(0);
            auto expr = arg.getExpr();
            segments.push_back(extractCompileTimeValue(expr, declContext));
          });

      return std::make_shared<InterpolatedStringLiteralValue>(segments);
    }

    case ExprKind::Closure: {
      auto closureExpr = cast<ClosureExpr>(expr);
      auto body = closureExpr->getBody();
      auto resultBuilderMembers =
          getResultBuilderMembersFromBraceStmt(body, declContext);

      if (!resultBuilderMembers.empty()) {
        return std::make_shared<BuilderValue>(resultBuilderMembers);
      }
      break;
    }

    case ExprKind::DerivedToBase: {
      auto derivedExpr = cast<DerivedToBaseExpr>(expr);
      return extractCompileTimeValue(derivedExpr->getSubExpr(), declContext);
    }
    default: {
      break;
    }
    }
  }

  return std::make_shared<RuntimeValue>();
}

static CustomAttrValue extractAttributeValue(const CustomAttr *attr,
                                             const DeclContext *declContext) {
  std::vector<FunctionParameter> parameters;
  if (const auto *args = attr->getArgs()) {
    for (auto arg : *args) {
      const auto label = arg.getLabel().str().str();
      auto argExpr = arg.getExpr();

      if (auto defaultArgument = dyn_cast<DefaultArgumentExpr>(argExpr)) {
        auto *decl = defaultArgument->getParamDecl();
        if (decl->hasDefaultExpr()) {
          argExpr = decl->getTypeCheckedDefaultExpr();
        }
      }
      parameters.push_back({label, argExpr->getType(),
                            extractCompileTimeValue(argExpr, declContext)});
    }
  }
  return {attr, parameters};
}

static AttrValueVector
extractPropertyWrapperAttrValues(VarDecl *propertyDecl) {
  AttrValueVector customAttrValues;
  for (auto *propertyWrapper : propertyDecl->getAttachedPropertyWrappers())
    customAttrValues.push_back(
        extractAttributeValue(propertyWrapper, propertyDecl->getDeclContext()));
  return customAttrValues;
}

static ConstValueTypePropertyInfo
extractTypePropertyInfo(VarDecl *propertyDecl) {
  std::optional<AttrValueVector> propertyWrapperValues;
  if (propertyDecl->hasAttachedPropertyWrapper())
    propertyWrapperValues = extractPropertyWrapperAttrValues(propertyDecl);

  if (const auto binding = propertyDecl->getParentPatternBinding()) {
    if (const auto originalInit = binding->getInit(0)) {
      return {propertyDecl,
              extractCompileTimeValue(originalInit,
                                      propertyDecl->getInnermostDeclContext()),
              propertyWrapperValues};
    }
  }

  if (auto accessorDecl = propertyDecl->getAccessor(AccessorKind::Get)) {
    if (auto body = accessorDecl->getTypecheckedBody()) {
      auto node = body->getFirstElement();
      if (auto *stmt = node.dyn_cast<Stmt *>()) {
        if (stmt->getKind() == StmtKind::Return) {
          return {
              propertyDecl,
              extractCompileTimeValue(cast<ReturnStmt>(stmt)->getResult(),
                                      accessorDecl->getInnermostDeclContext()),
              propertyWrapperValues};
        }
      }
    }
  }

  return {propertyDecl, std::make_shared<RuntimeValue>()};
}

std::optional<std::vector<EnumElementDeclValue>>
extractEnumCases(NominalTypeDecl *Decl) {
  if (Decl->getKind() == DeclKind::Enum) {
    std::vector<EnumElementDeclValue> Elements;
    for (EnumCaseDecl *ECD : cast<EnumDecl>(Decl)->getAllCases()) {
      for (EnumElementDecl *EED : ECD->getElements()) {
        std::string Name = EED->getNameStr().str();
        std::optional<std::string> RawValue =
            extractRawLiteral(EED->getRawValueExpr());

        std::vector<EnumElementParameterValue> Parameters;
        if (const ParameterList *Params = EED->getParameterList()) {
          for (const ParamDecl *Parameter : Params->getArray()) {
            std::optional<std::string> Label =
                Parameter->getParameterName().empty()
                    ? std::nullopt
                    : std::optional<std::string>(
                          Parameter->getParameterName().str().str());

            Parameters.push_back({Label, Parameter->getInterfaceType()});
          }
        }

        if (Parameters.empty()) {
          Elements.push_back({Name, RawValue, std::nullopt});
        } else {
          Elements.push_back({Name, RawValue, Parameters});
        }
      }
    }
    return Elements;
  }

  return std::nullopt;
}

ConstValueTypeInfo ConstantValueInfoRequest::evaluate(
    Evaluator &Evaluator, NominalTypeDecl *Decl,
    llvm::PointerUnion<const SourceFile *, ModuleDecl *> extractionScope)
    const {

  auto shouldExtract = [&](DeclContext *decl) {
    if (auto SF = extractionScope.dyn_cast<const SourceFile *>())
      return decl->getOutermostParentSourceFile() == SF;
    return decl->getParentModule() == cast<ModuleDecl *>(extractionScope);
  };

  std::vector<ConstValueTypePropertyInfo> Properties;
  std::optional<std::vector<EnumElementDeclValue>> EnumCases;

  // Use 'getStoredProperties' to get lowered lazy and wrapped properties.
  // @_objcImplementation extensions might contain stored properties.
  auto StoredProperties = Decl->getStoredProperties();
  std::unordered_set<VarDecl *> StoredPropertiesSet(StoredProperties.begin(),
                                                    StoredProperties.end());
  for (auto Property : StoredProperties) {
    if (shouldExtract(Property->getDeclContext())) {
      Properties.push_back(extractTypePropertyInfo(Property));
    }
  }

  auto extract = [&](class Decl *Member) {
    // Ignore plain stored properties collected above,
    // instead gather up remaining static and computed properties.
    if (auto *VD = dyn_cast<VarDecl>(Member))
      if (!StoredPropertiesSet.count(VD))
        Properties.push_back(extractTypePropertyInfo(VD));
  };

  if (shouldExtract(Decl)) {
    for (auto Member : Decl->getAllMembers()) {
      extract(Member);
    }
    EnumCases = extractEnumCases(Decl);
  }

  for (auto Extension: Decl->getExtensions()) {
    if (shouldExtract(Extension)) {
      for (auto Member : Extension->getAllMembers()) {
        extract(Member);
      }
    }
  }

  return ConstValueTypeInfo{Decl, Properties, EnumCases};
}

std::vector<ConstValueTypeInfo>
gatherConstValuesForModule(const std::unordered_set<std::string> &Protocols,
                           ModuleDecl *Module) {
  std::vector<ConstValueTypeInfo> Result;

  std::vector<NominalTypeDecl *> ConformanceDecls;
  NominalTypeConformanceCollector ConformanceCollector(Protocols,
                                                       ConformanceDecls);
  Module->walk(ConformanceCollector);
  // Visit macro expanded extensions
  for (auto *FU : Module->getFiles())
    if (auto *synthesizedSF = FU->getSynthesizedFile())
      for (auto D : synthesizedSF->getTopLevelDecls())
        if (isa<ExtensionDecl>(D))
          D->walk(ConformanceCollector);

  for (auto *CD : ConformanceDecls)
    Result.emplace_back(evaluateOrDefault(CD->getASTContext().evaluator,
                                          ConstantValueInfoRequest{CD, Module},
                                          {}));
  return Result;
}

std::vector<ConstValueTypeInfo>
gatherConstValuesForPrimary(const std::unordered_set<std::string> &Protocols,
                            const SourceFile *SF) {
  std::vector<ConstValueTypeInfo> Result;

  std::vector<NominalTypeDecl *> ConformanceDecls;
  NominalTypeConformanceCollector ConformanceCollector(Protocols,
                                                       ConformanceDecls);
  for (auto D : SF->getTopLevelDecls())
    D->walk(ConformanceCollector);
  // Visit macro expanded extensions
  if (auto *synthesizedSF = SF->getSynthesizedFile())
    for (auto D : synthesizedSF->getTopLevelDecls())
      if (isa<ExtensionDecl>(D))
        D->walk(ConformanceCollector);

  for (auto *CD : ConformanceDecls)
    Result.emplace_back(evaluateOrDefault(
        CD->getASTContext().evaluator, ConstantValueInfoRequest{CD, SF}, {}));
  return Result;
}

void writeLocationInformation(llvm::json::OStream &JSON, SourceLoc Loc,
                              const ASTContext &ctx) {
  if (Loc.isInvalid())
    return;

  JSON.attribute("file", ctx.SourceMgr.getDisplayNameForLoc(Loc));
  JSON.attribute("line",
                 ctx.SourceMgr.getPresumedLineAndColumnForLoc(Loc).first);
}

// Take BuilderValue, which is a representation of a result builder
// and write the values
void writeBuilderValue(llvm::json::OStream &JSON, BuilderValue *Value);

void writeValue(llvm::json::OStream &JSON,
                std::shared_ptr<CompileTimeValue> Value) {
  auto value = Value.get();
  switch (value->getKind()) {
  case CompileTimeValue::ValueKind::RawLiteral: {
    JSON.attribute("valueKind", "RawLiteral");
    JSON.attribute("value", cast<RawLiteralValue>(value)->getValue());
    break;
  }

  case CompileTimeValue::ValueKind::NilLiteral: {
    JSON.attribute("valueKind", "NilLiteral");
    break;
  }

  case CompileTimeValue::ValueKind::InitCall: {
    auto initCallValue = cast<InitCallValue>(value);

    JSON.attribute("valueKind", "InitCall");
    JSON.attributeObject("value", [&]() {
      JSON.attribute("type",
                     toFullyQualifiedTypeNameString(initCallValue->getType()));
      JSON.attributeArray("arguments", [&] {
        for (auto FP : initCallValue->getParameters()) {
          JSON.object([&] {
            JSON.attribute("label", FP.Label);
            JSON.attribute("type", toFullyQualifiedTypeNameString(FP.Type));
            writeValue(JSON, FP.Value);
          });
        }
      });
    });
    break;
  }

  case CompileTimeValue::ValueKind::Tuple: {
    auto tupleValue = cast<TupleValue>(value);

    JSON.attribute("valueKind", "Tuple");
    JSON.attributeArray("value", [&] {
      for (auto TV : tupleValue->getElements()) {
        JSON.object([&] {
          if (auto Label = TV.Label) {
            JSON.attribute("label", Label);
          }
          JSON.attribute("type", toFullyQualifiedTypeNameString(TV.Type));
          writeValue(JSON, TV.Value);
        });
      }
    });
    break;
  }

  case CompileTimeValue::ValueKind::Builder: {
    auto builderValue = cast<BuilderValue>(value);
    writeBuilderValue(JSON, builderValue);
    break;
  }

  case CompileTimeValue::ValueKind::Dictionary: {
    JSON.attribute("valueKind", "Dictionary");
    JSON.attributeArray("value", [&] {
      for (auto tupleValue : cast<DictionaryValue>(value)->getElements()) {
        auto tupleElements = tupleValue.get()->getElements();
        JSON.object([&] {
          JSON.attributeObject(
              "key", [&] { writeValue(JSON, tupleElements[0].Value); });
          JSON.attributeObject(
              "value", [&] { writeValue(JSON, tupleElements[1].Value); });
        });
      }
    });
    break;
  }

  case CompileTimeValue::ValueKind::Array: {
    auto arrayValue = cast<ArrayValue>(value);

    JSON.attribute("valueKind", "Array");
    JSON.attributeArray("value", [&] {
      for (auto CTP : arrayValue->getElements()) {
        JSON.object([&] { writeValue(JSON, CTP); });
      }
    });
    break;
  }

  case CompileTimeValue::ValueKind::Enum: {
    auto enumValue = cast<EnumValue>(value);
    JSON.attribute("valueKind", "Enum");
    JSON.attributeObject("value", [&]() {
      JSON.attribute("name", enumValue->getIdentifier());
      if (enumValue->getParameters().has_value()) {
        auto params = enumValue->getParameters().value();
        JSON.attributeArray("arguments", [&] {
          for (auto FP : params) {
            JSON.object([&] {
              JSON.attribute("label", FP.Label);
              JSON.attribute("type", toFullyQualifiedTypeNameString(FP.Type));
              writeValue(JSON, FP.Value);
            });
          }
        });
      }
    });
    break;
  }

  case CompileTimeValue::ValueKind::Type: {
    auto typeValue = cast<TypeValue>(value);
    Type type = typeValue->getType();
    JSON.attribute("valueKind", "Type");
    JSON.attributeObject("value", [&]() {
      JSON.attribute("type",
                     toFullyQualifiedTypeNameString(type));
      JSON.attribute("mangledName",
                     toMangledTypeNameString(type));
    });
    break;
  }

  case CompileTimeValue::ValueKind::KeyPath: {
    auto keyPathValue = cast<KeyPathValue>(value);
    JSON.attribute("valueKind", "KeyPath");
    JSON.attributeObject("value", [&]() {
      JSON.attribute("path", keyPathValue->getPath());
      JSON.attribute("rootType", toFullyQualifiedTypeNameString(
                                     keyPathValue->getRootType()));
      JSON.attributeArray("components", [&] {
        auto components = keyPathValue->getComponents();
        for (auto c : components) {
          JSON.object([&] {
            JSON.attribute("label", c.Label);
            JSON.attribute("type", toFullyQualifiedTypeNameString(c.Type));
          });
        }
      });
    });
    break;
  }

  case CompileTimeValue::ValueKind::FunctionCall: {
    auto functionCallValue = cast<FunctionCallValue>(value);
    JSON.attribute("valueKind", "FunctionCall");
    JSON.attributeObject("value", [&]() {
      JSON.attribute("name", functionCallValue->getIdentifier());
      if (functionCallValue->getParameters().has_value()) {
        auto params = functionCallValue->getParameters().value();
        JSON.attributeArray("arguments", [&] {
          for (auto FP : params) {
            JSON.object([&] {
              JSON.attribute("label", FP.Label);
              JSON.attribute("type", toFullyQualifiedTypeNameString(FP.Type));
              writeValue(JSON, FP.Value);
            });
          }
        });
      }
    });
    break;
  }

  case CompileTimeValue::ValueKind::StaticFunctionCall: {
    auto staticFunctionCallValue = cast<StaticFunctionCallValue>(value);

    JSON.attribute("valueKind", "StaticFunctionCall");
    JSON.attributeObject("value", [&]() {
      JSON.attribute("type", toFullyQualifiedTypeNameString(
                                 staticFunctionCallValue->getType()));
      JSON.attribute("memberLabel", staticFunctionCallValue->getLabel());
      JSON.attributeArray("arguments", [&] {
        for (auto FP : staticFunctionCallValue->getParameters()) {
          JSON.object([&] {
            JSON.attribute("label", FP.Label);
            JSON.attribute("type", toFullyQualifiedTypeNameString(FP.Type));
            writeValue(JSON, FP.Value);
          });
        }
      });
    });
    break;
  }

  case CompileTimeValue::ValueKind::MemberReference: {
    auto memberReferenceValue = cast<MemberReferenceValue>(value);
    JSON.attribute("valueKind", "MemberReference");
    JSON.attributeObject("value", [&]() {
      JSON.attribute("baseType", toFullyQualifiedTypeNameString(
                                     memberReferenceValue->getBaseType()));
      JSON.attribute("memberLabel", memberReferenceValue->getMemberLabel());
    });
    break;
  }

  case CompileTimeValue::ValueKind::InterpolatedString: {
    auto interpolatedStringValue = cast<InterpolatedStringLiteralValue>(value);
    JSON.attribute("valueKind", "InterpolatedStringLiteral");
    JSON.attributeObject("value", [&]() {
      JSON.attributeArray("segments", [&] {
        auto segments = interpolatedStringValue->getSegments();
        for (auto s : segments) {
          JSON.object([&] { writeValue(JSON, s); });
        }
      });
    });
    break;
  }

  case CompileTimeValue::ValueKind::Runtime: {
    JSON.attribute("valueKind", "Runtime");
    break;
  }
  }
}

void writeAttributeInfo(llvm::json::OStream &JSON,
                        const CustomAttrValue &AttrVal,
                        const ASTContext &ctx) {
  JSON.object([&] {
    JSON.attribute("type",
                   toFullyQualifiedTypeNameString(AttrVal.Attr->getType()));
    writeLocationInformation(JSON, AttrVal.Attr->getLocation(), ctx);
    JSON.attributeArray("arguments", [&] {
      for (auto FP : AttrVal.Parameters) {
        JSON.object([&] {
          JSON.attribute("label", FP.Label);
          JSON.attribute("type", toFullyQualifiedTypeNameString(FP.Type));
          writeValue(JSON, FP.Value);
        });
      }
    });
  });
}

void writePropertyWrapperAttributes(
    llvm::json::OStream &JSON, std::optional<AttrValueVector> PropertyWrappers,
    const ASTContext &ctx) {
  if (!PropertyWrappers.has_value()) {
    return;
  }

  JSON.attributeArray("propertyWrappers", [&] {
    for (auto PW : PropertyWrappers.value())
      writeAttributeInfo(JSON, PW, ctx);
  });
}

void writeEnumCases(
    llvm::json::OStream &JSON,
    std::optional<std::vector<EnumElementDeclValue>> EnumElements) {
  if (!EnumElements.has_value()) {
    return;
  }

  JSON.attributeArray("cases", [&] {
    for (const auto &Case : EnumElements.value()) {
      JSON.object([&] {
        JSON.attribute("name", Case.Name);
        if (Case.RawValue.has_value()) {
          JSON.attribute("rawValue", Case.RawValue.value());
        }
        if (Case.Parameters.has_value()) {
          JSON.attributeArray("parameters", [&] {
            for (const auto &Parameter : Case.Parameters.value()) {
              JSON.object([&] {
                if (auto Label = Parameter.Label) {
                  JSON.attribute("label", Label);
                }
                JSON.attribute("type",
                               toFullyQualifiedTypeNameString(Parameter.Type));
              });
            }
          });
        }
      });
    }
  });
}

std::optional<std::shared_ptr<CompileTimeValue>>
getResultBuilderElementFromASTNode(const ASTNode node) {
  if (auto *D = node.dyn_cast<Decl *>()) {
    if (auto *patternBinding = dyn_cast<PatternBindingDecl>(D)) {
      if (auto originalInit = patternBinding->getOriginalInit(0)) {
        return extractCompileTimeValue(
            originalInit, patternBinding->getInnermostDeclContext());
      }
    }
  }
  return std::nullopt;
}

BuilderValue::ConditionalMember
getConditionalMemberFromIfStmt(const IfStmt *ifStmt,
                               const DeclContext *declContext) {
  std::vector<BuilderValue::ConditionalMember::AvailabilitySpec>
      AvailabilitySpecs;
  std::vector<std::shared_ptr<BuilderValue::BuilderMember>> IfElements;
  std::vector<std::shared_ptr<BuilderValue::BuilderMember>> ElseElements;
  if (auto thenBraceStmt = ifStmt->getThenStmt()) {
    for (auto elem : thenBraceStmt->getElements()) {
      if (auto memberElement = getResultBuilderElementFromASTNode(elem)) {
        IfElements.push_back(std::make_shared<BuilderValue::SingleMember>(
            memberElement.value()));
      }
    }
  }

  if (auto elseStmt = ifStmt->getElseStmt()) {
    if (auto *elseIfStmt = dyn_cast<IfStmt>(elseStmt)) {
      ElseElements.push_back(std::make_shared<BuilderValue::ConditionalMember>(
          getConditionalMemberFromIfStmt(elseIfStmt, declContext)));
    } else if (auto *elseBraceStmt = dyn_cast<BraceStmt>(elseStmt)) {
      for (auto elem : elseBraceStmt->getElements()) {
        if (auto memberElement = getResultBuilderElementFromASTNode(elem)) {
          ElseElements.push_back(std::make_shared<BuilderValue::SingleMember>(
              memberElement.value()));
        }
      }
    }
  }
  BuilderValue::MemberKind memberKind = BuilderValue::Either;

  if (ElseElements.size() == 0) {
    memberKind = BuilderValue::Optional;
  }
  for (auto elt : ifStmt->getCond()) {
    if (elt.getKind() == StmtConditionElement::CK_Availability) {
      for (auto spec :
           elt.getAvailability()->getSemanticAvailabilitySpecs(declContext)) {
        if (spec.getDomain().isPlatform()) {
          AvailabilitySpecs.push_back(
              BuilderValue::ConditionalMember::AvailabilitySpec(
                  spec.getDomain(), spec.getVersion()));
        }
      }
      memberKind = BuilderValue::LimitedAvailability;
      break;
    }
  }

  if (AvailabilitySpecs.empty()) {
    return BuilderValue::ConditionalMember(memberKind, IfElements,
                                           ElseElements);
  }

  return BuilderValue::ConditionalMember(memberKind, AvailabilitySpecs,
                                         IfElements, ElseElements);
}

BuilderValue::ArrayMember
getBuildArrayMemberFromForEachStmt(const ForEachStmt *forEachStmt) {
  std::vector<std::shared_ptr<BuilderValue::BuilderMember>> MemberElements;
  if (auto braceStmt = forEachStmt->getBody()) {
    for (auto elem : braceStmt->getElements()) {
      if (auto memberElement = getResultBuilderElementFromASTNode(elem)) {
        MemberElements.push_back(std::make_shared<BuilderValue::SingleMember>(
            memberElement.value()));
      }
    }
  }
  return BuilderValue::ArrayMember(MemberElements);
}

std::vector<std::shared_ptr<BuilderValue::BuilderMember>>
getResultBuilderMembersFromBraceStmt(BraceStmt *braceStmt,
                                     const DeclContext *declContext) {
  std::vector<std::shared_ptr<BuilderValue::BuilderMember>>
      ResultBuilderMembers;
  for (auto elem : braceStmt->getElements()) {
    if (auto resultBuilderElement = getResultBuilderElementFromASTNode(elem)) {
      ResultBuilderMembers.push_back(
          std::make_shared<BuilderValue::SingleMember>(
              resultBuilderElement.value()));
    } else if (auto *stmt = elem.dyn_cast<Stmt *>()) {
      if (auto *ifStmt = dyn_cast<IfStmt>(stmt)) {
        ResultBuilderMembers.push_back(
            std::make_shared<BuilderValue::ConditionalMember>(
                getConditionalMemberFromIfStmt(ifStmt, declContext)));
      } else if (auto *doStmt = dyn_cast<DoStmt>(stmt)) {
        if (auto body = doStmt->getBody()) {
          for (auto elem : body->getElements()) {
            if (auto *stmt = elem.dyn_cast<Stmt *>()) {
              if (auto *forEachStmt = dyn_cast<ForEachStmt>(stmt)) {
                ResultBuilderMembers.push_back(
                    std::make_shared<BuilderValue::ArrayMember>(
                        getBuildArrayMemberFromForEachStmt(forEachStmt)));
              }
            }
          }
        }
      }
    }
  }
  return ResultBuilderMembers;
}

std::shared_ptr<BuilderValue>
createBuilderCompileTimeValue(CustomAttr *AttachedResultBuilder,
                              const swift::VarDecl *VarDecl) {
  std::vector<std::shared_ptr<BuilderValue::BuilderMember>>
      ResultBuilderMembers;
  if (!VarDecl->getAllAccessors().empty()) {
    if (auto accessor = VarDecl->getAllAccessors()[0]) {
      if (auto braceStmt = accessor->getTypecheckedBody()) {
        ResultBuilderMembers = getResultBuilderMembersFromBraceStmt(
            braceStmt, accessor->getDeclContext());
      }
    }
  }
  return std::make_shared<BuilderValue>(AttachedResultBuilder,
                                        ResultBuilderMembers);
}

void writeSingleBuilderMemberElement(
    llvm::json::OStream &JSON, std::shared_ptr<CompileTimeValue> Element) {
  switch (Element.get()->getKind()) {
  case CompileTimeValue::ValueKind::StaticFunctionCall: {
    auto staticFunctionCallValue = cast<StaticFunctionCallValue>(Element.get());
    if (staticFunctionCallValue->getLabel() == "buildExpression") {
      for (auto FP : staticFunctionCallValue->getParameters()) {
        writeValue(JSON, FP.Value);
      }
    }
    break;
  }
  default: {
    writeValue(JSON, Element);
    break;
  }
  }
}

void writeBuilderMember(
    llvm::json::OStream &JSON,
    std::shared_ptr<BuilderValue::BuilderMember> BuilderMember) {
  auto Member = BuilderMember.get();
  switch (Member->getKind()) {
  case BuilderValue::Expression: {
    auto member = cast<BuilderValue::SingleMember>(Member);
    JSON.attributeObject("element", [&] {
      writeSingleBuilderMemberElement(JSON, member->getElement());
    });

    break;
  }

  case BuilderValue::Array: {
    auto member = cast<BuilderValue::ArrayMember>(Member);
    JSON.attributeArray("elements", [&] {
      for (auto elem : member->getElements()) {
        JSON.object([&] { writeBuilderMember(JSON, elem); });
      }
    });
    break;
  }

  default: {
    auto member = cast<BuilderValue::ConditionalMember>(Member);
    if (auto availabilitySpecs = member->getAvailabilitySpecs()) {
      JSON.attributeArray("availabilityAttributes", [&] {
        for (auto elem : *availabilitySpecs) {
          JSON.object([&] {
            JSON.attribute(
                "platform",
                platformString(elem.getDomain().getPlatformKind()).str());
            JSON.attribute("minVersion", elem.getVersion().getAsString());
          });
        }
      });
    }
    JSON.attributeArray("ifElements", [&] {
      for (auto elem : member->getIfElements()) {
        JSON.object([&] { writeBuilderMember(JSON, elem); });
      }
    });
    JSON.attributeArray("elseElements", [&] {
      for (auto elem : member->getElseElements()) {
        JSON.object([&] { writeBuilderMember(JSON, elem); });
      }
    });
    break;
  }
  }
}

void writeBuilderValue(llvm::json::OStream &JSON, BuilderValue *Value) {
  JSON.attribute("valueKind", "Builder");
  JSON.attributeObject("value", [&] {
    if (auto resultBuilderType = Value->getResultBuilderType()) {
      JSON.attribute("type", toFullyQualifiedTypeNameString(
                                 resultBuilderType.value()->getType()));
    } else {
      JSON.attribute("type", "");
    }

    JSON.attributeArray("members", [&] {
      for (auto member : Value->getMembers()) {
        JSON.object([&] {
          switch (member->getKind()) {
          case BuilderValue::Expression:
            JSON.attribute("kind", "buildExpression");
            break;
          case BuilderValue::Either:
            JSON.attribute("kind", "buildEither");
            break;
          case BuilderValue::Optional:
            JSON.attribute("kind", "buildOptional");
            break;
          case BuilderValue::LimitedAvailability:
            JSON.attribute("kind", "buildLimitedAvailability");
            break;
          case BuilderValue::Array:
            JSON.attribute("kind", "buildArray");
            break;
          case BuilderValue::Unknown:
            JSON.attribute("kind", "Unknown");
            break;
          }

          writeBuilderMember(JSON, member);
        });
      }
    });
  });
}

std::optional<std::shared_ptr<BuilderValue>>
extractBuilderValueIfExists(const swift::NominalTypeDecl *TypeDecl,
                            const swift::VarDecl *VarDecl) {
  if (auto *attr = VarDecl->getAttachedResultBuilder()) {
    return createBuilderCompileTimeValue(attr, VarDecl);
  }

  for (ProtocolDecl *Decl :
       TypeDecl->getLocalProtocols(ConformanceLookupKind::All)) {
    // FIXME(noncopyable_generics): Should these be included?
    if (Decl->getInvertibleProtocolKind())
      continue;

    for (auto Member : Decl->getMembers()) {
      if (auto *VD = dyn_cast<swift::VarDecl>(Member)) {
        if (VD->getName() != VarDecl->getName())
          continue;

        if (auto *attr = VD->getAttachedResultBuilder()) {
          return createBuilderCompileTimeValue(attr, VarDecl);
        }
      }
    }
  }
  return std::nullopt;
  ;
}

void writeAvailabilityAttributes(llvm::json::OStream &JSON, const Decl &decl) {
  auto attrs = decl.getSemanticAvailableAttrs();
  if (attrs.empty())
    return;

  JSON.attributeArray("availabilityAttributes", [&] {
    for (auto attr : attrs) {
      JSON.object([&] {
        auto domainName = attr.getDomain().getNameForAttributePrinting();
        if (!domainName.empty())
          JSON.attribute("platform", domainName);

        if (!attr.getMessage().empty())
          JSON.attribute("message", attr.getMessage());

        if (!attr.getRename().empty())
          JSON.attribute("rename", attr.getRename());

        if (attr.getIntroduced().has_value())
          JSON.attribute("introducedVersion",
                         attr.getIntroduced().value().getAsString());

        if (attr.getDeprecated().has_value())
          JSON.attribute("deprecatedVersion",
                         attr.getDeprecated().value().getAsString());

        if (attr.getObsoleted().has_value())
          JSON.attribute("obsoletedVersion",
                         attr.getObsoleted().value().getAsString());

        JSON.attribute("isUnavailable", attr.isUnconditionallyUnavailable());
        JSON.attribute("isDeprecated", attr.isUnconditionallyDeprecated());
      });
    }
  });
}

void writeSubstitutedOpaqueTypeAliasDetails(
    llvm::json::OStream &JSON, const OpaqueTypeArchetypeType &OpaqueTy) {
  auto Signature = OpaqueTy.getDecl()->getOpaqueInterfaceGenericSignature();

  JSON.attributeArray("opaqueTypeProtocolRequirements", [&] {
    for (const auto Requirement : Signature.getRequirements()) {
      // Ignore requirements whose subject type is that of the owner decl
      if (!Requirement.getFirstType()->isEqual(OpaqueTy.getInterfaceType()))
        continue;

      if (Requirement.getKind() != RequirementKind::Conformance)
        continue;

      // FIXME(noncopyable_generics): Should these be included?
      if (Requirement.getProtocolDecl()->getInvertibleProtocolKind())
        continue;

      JSON.value(
          toFullyQualifiedProtocolNameString(*Requirement.getProtocolDecl()));
    }
  });

  JSON.attributeArray("opaqueTypeSameTypeRequirements", [&] {
    for (const auto Requirement : Signature.getRequirements()) {
      if (Requirement.getKind() == RequirementKind::SameType) {
        auto TypeAliasType = Requirement.getFirstType();
        auto TypeWitness = Requirement.getSecondType();
        JSON.object([&] {
          auto TypeAliasName = toFullyQualifiedTypeNameString(TypeAliasType);
          if (auto DependentMemberTy =
                  TypeAliasType->getAs<DependentMemberType>())
            if (const auto *Assoc = DependentMemberTy->getAssocType())
              TypeAliasName =
                  toFullyQualifiedProtocolNameString(*Assoc->getProtocol()) +
                  "." + DependentMemberTy->getName().str().str();
          JSON.attribute("typeAliasName", TypeAliasName);
          JSON.attribute("substitutedTypeName",
                         toFullyQualifiedTypeNameString(TypeWitness));
          JSON.attribute("substitutedMangledTypeName",
                         toMangledTypeNameString(TypeWitness));
        });
      }
    }
  });
}

void writeAssociatedTypeAliases(llvm::json::OStream &JSON,
                                const NominalTypeDecl &NomTypeDecl) {
  JSON.attributeArray("associatedTypeAliases", [&] {
    for (auto &Conformance : NomTypeDecl.getAllConformances()) {
      Conformance->forEachTypeWitness(
          [&](AssociatedTypeDecl *assoc, Type type, TypeDecl *typeDecl) {
            JSON.object([&] {
              JSON.attribute("typeAliasName", assoc->getName().str().str());
              JSON.attribute("substitutedTypeName",
                             toFullyQualifiedTypeNameString(type));
              JSON.attribute("substitutedMangledTypeName",
                             toMangledTypeNameString(type));
              if (auto OpaqueTy = dyn_cast<OpaqueTypeArchetypeType>(type)) {
                writeSubstitutedOpaqueTypeAliasDetails(JSON, *OpaqueTy);
              }
            });
            return false;
          });
    }
  });
}

void writeProperties(llvm::json::OStream &JSON,
                     const ConstValueTypeInfo &TypeInfo,
                     const NominalTypeDecl &NomTypeDecl) {
  JSON.attributeArray("properties", [&] {
    for (const auto &PropertyInfo : TypeInfo.Properties) {
      JSON.object([&] {
        const auto *decl = PropertyInfo.VarDecl;
        std::shared_ptr<CompileTimeValue> value = PropertyInfo.Value;
        JSON.attribute("label", decl->getName().str().str());
        JSON.attribute("type", toFullyQualifiedTypeNameString(
            decl->getInterfaceType()));
        JSON.attribute("mangledTypeName", "n/a - deprecated");
        JSON.attribute("isStatic", decl->isStatic() ? "true" : "false");
        JSON.attribute("isComputed", !decl->hasStorage() ? "true" : "false");
        writeLocationInformation(JSON, decl->getLoc(),
                                 decl->getDeclContext()->getASTContext());

        if (value.get()->getKind() == CompileTimeValue::ValueKind::Runtime) {
          // Extract result builder information only if the variable has not
          // used a different kind of initializer
          if (auto builderValue =
                  extractBuilderValueIfExists(&NomTypeDecl, decl)) {
            value = builderValue.value();
          }
        }

        writeValue(JSON, value);
        writePropertyWrapperAttributes(JSON, PropertyInfo.PropertyWrappers,
                                       decl->getASTContext());
        writeAvailabilityAttributes(JSON, *decl);
      });
    }
  });
}

void writeConformances(llvm::json::OStream &JSON,
                       const NominalTypeDecl &NomTypeDecl) {
  JSON.attributeArray("conformances", [&] {
    for (auto *Conformance : NomTypeDecl.getAllConformances()) {
      auto Proto = Conformance->getProtocol();
      // FIXME(noncopyable_generics): Should these be included?
      if (Proto->getInvertibleProtocolKind())
        continue;

      JSON.value(toFullyQualifiedProtocolNameString(*Proto));
    }
  });
}

void writeAllConformances(llvm::json::OStream &JSON,
                          const NominalTypeDecl &NomTypeDecl) {
  JSON.attributeArray("allConformances", [&] {
    for (auto *Conformance : NomTypeDecl.getAllConformances()) {
      auto Proto = Conformance->getProtocol();
      // FIXME(noncopyable_generics): Should these be included?
      if (Proto->getInvertibleProtocolKind())
        continue;

      JSON.object([&] {
        JSON.attribute("protocolName",
                       toFullyQualifiedProtocolNameString(*Proto));
        JSON.attribute(
            "conformanceDefiningModule",
            Conformance->getDeclContext()->getParentModule()->getName().str());
      });
    }
  });
}

void writeTypeName(llvm::json::OStream &JSON, const TypeDecl &TypeDecl) {
  JSON.attribute("typeName",
                 toFullyQualifiedTypeNameString(
                                 TypeDecl.getDeclaredInterfaceType()));
  JSON.attribute("mangledTypeName",
                 toMangledTypeNameString(TypeDecl.getDeclaredInterfaceType()));
}

void writeNominalTypeKind(llvm::json::OStream &JSON,
                          const NominalTypeDecl &NomTypeDecl) {
  JSON.attribute(
      "kind",
      NomTypeDecl.getDescriptiveKindName(NomTypeDecl.getDescriptiveKind())
          .str());
}

bool writeAsJSONToFile(const std::vector<ConstValueTypeInfo> &ConstValueInfos,
                       llvm::raw_ostream &OS) {
  llvm::json::OStream JSON(OS, 2);
  JSON.array([&] {
    for (const auto &TypeInfo : ConstValueInfos) {
      assert(isa<NominalTypeDecl>(TypeInfo.TypeDecl) &&
             "Expected Nominal Type Decl for a conformance");
      const auto *NomTypeDecl = cast<NominalTypeDecl>(TypeInfo.TypeDecl);
      const auto SourceLoc =
          extractNearestSourceLoc(NomTypeDecl->getInnermostDeclContext());
      const auto &Ctx = NomTypeDecl->getInnermostDeclContext()->getASTContext();

      JSON.object([&] {
        writeTypeName(JSON, *NomTypeDecl);
        writeNominalTypeKind(JSON, *NomTypeDecl);
        writeLocationInformation(JSON, SourceLoc, Ctx);
        writeConformances(JSON, *NomTypeDecl);

        // "conformances" will be removed once all clients move to
        // "allConformances"
        writeAllConformances(JSON, *NomTypeDecl);
        writeAssociatedTypeAliases(JSON, *NomTypeDecl);
        writeProperties(JSON, TypeInfo, *NomTypeDecl);
        writeEnumCases(JSON, TypeInfo.EnumElements);
        writeAvailabilityAttributes(JSON, *NomTypeDecl);
      });
    }
  });
  JSON.flush();
  return false;
}

} // namespace swift

#define SWIFT_TYPEID_ZONE ConstExtract
#define SWIFT_TYPEID_HEADER "swift/ConstExtract/ConstExtractTypeIDZone.def"
#include "swift/Basic/ImplementTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER

// Define request evaluation functions for each of the name lookup requests.
static AbstractRequestFunction *constExtractRequestFunctions[] = {
#define SWIFT_REQUEST(Zone, Name, Sig, Caching, LocOptions)                    \
  reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "swift/ConstExtract/ConstExtractTypeIDZone.def"
#undef SWIFT_REQUEST
};

void swift::registerConstExtractRequestFunctions(Evaluator &evaluator) {
  evaluator.registerRequestFunctions(Zone::ConstExtract,
                                     constExtractRequestFunctions);
}
