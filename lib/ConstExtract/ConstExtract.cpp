//===-------- ConstExtract.pp -- Gather Compile-Time-Known Values --------===//
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

#include "swift/ConstExtract/ConstExtract.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/JSON.h"

#include <set>
#include <sstream>
#include <string>

namespace {
/// A helper class to collect all nominal type declarations that conform to
/// specific protocols provided as input.
class NominalTypeConformanceCollector : public swift::ASTWalker {
  const std::unordered_set<std::string> &Protocols;
  std::vector<swift::NominalTypeDecl *> &ConformanceTypeDecls;

public:
  NominalTypeConformanceCollector(
      const std::unordered_set<std::string> &Protocols,
      std::vector<swift::NominalTypeDecl *> &ConformanceDecls)
      : Protocols(Protocols), ConformanceTypeDecls(ConformanceDecls) {}

  bool walkToDeclPre(swift::Decl *D) override {
    if (auto *NTD = llvm::dyn_cast<swift::NominalTypeDecl>(D))
      for (auto &Protocol : NTD->getAllProtocols())
        if (Protocols.count(Protocol->getName().str().str()) != 0)
          ConformanceTypeDecls.push_back(NTD);
    return true;
  }
};

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
        protocols.insert(ScalarNode->getRawValue().str());
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

static std::shared_ptr<CompileTimeValue>
extractPropertyInitializationValue(VarDecl *propertyDecl) {
  auto binding = propertyDecl->getParentPatternBinding();
  if (binding) {
    auto originalInit = binding->getOriginalInit(0);
    if (originalInit) {
      std::string LiteralOutput;
      llvm::raw_string_ostream OutputStream(LiteralOutput);
      originalInit->printConstExprValue(&OutputStream, nullptr);
      if (!LiteralOutput.empty())
        return std::make_shared<RawLiteralValue>(LiteralOutput);
    }
  }

  return std::make_shared<RuntimeValue>();
}

ConstValueTypeInfo
ConstantValueInfoRequest::evaluate(Evaluator &Evaluator,
                                   NominalTypeDecl *Decl) const {
  // Use 'getStoredProperties' to get lowered lazy and wrapped properties
  auto StoredProperties = Decl->getStoredProperties();
  std::unordered_set<VarDecl *> StoredPropertiesSet(StoredProperties.begin(),
                                                    StoredProperties.end());

  std::vector<ConstValueTypePropertyInfo> Properties;
  for (auto Property : StoredProperties) {
    Properties.push_back(
        {Property, extractPropertyInitializationValue(Property)});
  }
  for (auto Member : Decl->getMembers()) {
    auto *VD = dyn_cast<VarDecl>(Member);
    // Ignore plain stored properties collected above,
    // instead gather up remaining static and computed properties.
    if (!VD || StoredPropertiesSet.count(VD))
      continue;
    Properties.push_back({VD, extractPropertyInitializationValue(VD)});
  }

  return ConstValueTypeInfo{Decl, Properties};
}

std::vector<ConstValueTypeInfo>
gatherConstValuesForModule(const std::unordered_set<std::string> &Protocols,
                           ModuleDecl *Module) {
  std::vector<ConstValueTypeInfo> Result;

  std::vector<NominalTypeDecl *> ConformanceDecls;
  NominalTypeConformanceCollector ConformanceCollector(Protocols,
                                                       ConformanceDecls);
  Module->walk(ConformanceCollector);
  for (auto *CD : ConformanceDecls)
    Result.emplace_back(evaluateOrDefault(CD->getASTContext().evaluator,
                                          ConstantValueInfoRequest{CD}, {}));
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

  for (auto *CD : ConformanceDecls)
    Result.emplace_back(evaluateOrDefault(CD->getASTContext().evaluator,
                                          ConstantValueInfoRequest{CD}, {}));
  return Result;
}

static std::string toString(const CompileTimeValue *Value) {
  switch (Value->getKind()) {
    case CompileTimeValue::RawLiteral:
      return cast<RawLiteralValue>(Value)->getValue();
    case CompileTimeValue::InitCall:
      // TODO
    case CompileTimeValue::Builder:
      // TODO
    case CompileTimeValue::Dictionary:
      // TODO
    case CompileTimeValue::Runtime:
      return "Unknown";
  }
}

bool writeAsJSONToFile(const std::vector<ConstValueTypeInfo> &ConstValueInfos,
                       llvm::raw_fd_ostream &OS) {
  llvm::json::OStream JSON(OS, 2);
  JSON.array([&] {
    for (const auto &TypeInfo : ConstValueInfos) {
      JSON.object([&] {
        const auto *TypeDecl = TypeInfo.TypeDecl;
        JSON.attribute("typeName", TypeDecl->getName().str().str());
        JSON.attribute(
            "kind",
            TypeDecl->getDescriptiveKindName(TypeDecl->getDescriptiveKind())
                .str());
        JSON.attributeArray("properties", [&] {
          for (const auto &PropertyInfo : TypeInfo.Properties) {
            JSON.object([&] {
              const auto *PropertyDecl = PropertyInfo.VarDecl;
              JSON.attribute("label", PropertyDecl->getName().str().str());
              JSON.attribute("type", PropertyDecl->getType().getString());
              JSON.attribute("isStatic",
                             PropertyDecl->isStatic() ? "true" : "false");
              JSON.attribute("isComputed",
                             !PropertyDecl->hasStorage() ? "true" : "false");
              JSON.attribute("value", toString(PropertyInfo.Value.get()));
            });
          }
        });
      });
    }
  });
  JSON.flush();
  return false;
}

} // namespace swift
