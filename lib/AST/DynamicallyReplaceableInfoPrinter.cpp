//===-- DynamicallyReplaceableInfoPrinter.cpp - Dynamic info ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DynamicallyReplaceableInfoPrinter.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/Basic/SourceManager.h"

using namespace swift;

DynamicallyReplaceableInfoPrinter::DynamicallyReplaceableInfoPrinter(
    StringRef moduleName) {
  module.name = moduleName;
  module.formatVersion = 1;
}

YAMLDynamicallyReplaceableFile &
DynamicallyReplaceableInfoPrinter::getNodeForFile(StringRef filename) {
  auto existing = fileMap.find(filename);
  if (existing != fileMap.end()) {
    return module.files[existing->second];
  }

  YAMLDynamicallyReplaceableFile newFile;
  newFile.name = filename;
  module.files.push_back(newFile);
  auto last = module.files.size() - 1;
  fileMap[filename] = last;
  return module.files[last];
}

bool DynamicallyReplaceableInfoPrinter::walkToDeclPre(Decl *d) {
  auto *val = dyn_cast<ValueDecl>(d);
  if (!val)
    return true;
  if (!isa<AbstractStorageDecl>(val) && !isa<AbstractFunctionDecl>(val))
    return true;
  if (!val->isNativeDynamic() && !val->isObjCDynamic())
    return true;
  if (val->isImplicit() || isa<AccessorDecl>(val))
    return true;
  auto &SM = val->getASTContext().SourceMgr;

  unsigned BufferID = SM.findBufferContainingLoc(val->getStartLoc());
  auto filename = SM.getIdentifierForBuffer(BufferID);

  auto startLineAndCol = SM.getLineAndColumn(val->getStartLoc(), BufferID);
  auto endLineAndCol = SM.getLineAndColumn(val->getEndLoc(), BufferID);

  YAMLDynamicallyReplaceableNode node;

  std::string declName;
  llvm::raw_string_ostream declNameStream(declName);
  swift::printContext(declNameStream, val->getDeclContext());
  declNameStream << ".";
  val->getFullName().print(declNameStream);

  std::string type;
  llvm::raw_string_ostream typeStream(type);
  val->getInterfaceType()->print(typeStream);

  node.name = declNameStream.str();
  node.type = typeStream.str();
  node.startLocLine = startLineAndCol.first;
  node.startLocCol = startLineAndCol.second;
  node.endLocLine = endLineAndCol.first;
  node.endLocCol = endLineAndCol.second;

  auto &fileNode = getNodeForFile(filename);
  fileNode.decls.push_back(node);
  return true;
}

void DynamicallyReplaceableInfoPrinter::writeToStream(llvm::raw_ostream &os) {
  llvm::yaml::Output yout(os);
  yout << module;
}
