//===--- APIDigesterData.cpp - Code completion implementation --------------===//
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
#include "swift/IDE/APIDigesterData.h"

using namespace swift;
using namespace ide;
using namespace api;

inline raw_ostream &swift::ide::api::
operator<<(raw_ostream &Out, const SDKNodeKind Value) {
  switch (Value) {
#define NODE_KIND(Name) case SDKNodeKind::Name: return Out << #Name;
#include "swift/IDE/DigesterEnums.def"
  }
  llvm_unreachable("Undefined SDK node kind.");
}

inline raw_ostream &swift::ide::api::
operator<<(raw_ostream &Out, const NodeAnnotation Value) {
#define NODE_ANNOTATION(X) if (Value == NodeAnnotation::X) { return Out << #X; }
#include "swift/IDE/DigesterEnums.def"
  llvm_unreachable("Undefined SDK node kind.");
}

swift::ide::api::DiffItem::
DiffItem(SDKNodeKind NodeKind, NodeAnnotation DiffKind, StringRef ChildIndex,
         StringRef LeftUsr, StringRef RightUsr, StringRef LeftComment,
         StringRef RightComment, StringRef ModuleName) : NodeKind(NodeKind),
           DiffKind(DiffKind), ChildIndex(ChildIndex), LeftUsr(LeftUsr),
           RightUsr(RightUsr), LeftComment(LeftComment),
           RightComment(RightComment), ModuleName(ModuleName) {
  assert(!ChildIndex.empty() && "Child index is empty.");
}

StringRef swift::ide::api::DiffItem::head() {
  return "SDK_CHANGE";
}

bool swift::ide::api::DiffItem::operator<(DiffItem Other) const {
  if (auto UsrCompare = LeftUsr.compare(Other.LeftUsr))
      return UsrCompare < 0;
  if (NodeKind != Other.NodeKind)
    return NodeKind < Other.NodeKind;
  if (DiffKind != Other.DiffKind)
    return DiffKind < Other.DiffKind;
  if (auto ChildCompare = ChildIndex.compare(Other.ChildIndex))
    return ChildCompare < 0;
  return false;
}

void swift::ide::api::DiffItem::describe(llvm::raw_ostream &os) {
  os << "#ifndef " << head() << "\n";
  os << "#define " << head() << "(NODE_KIND, DIFF_KIND, CHILD_INDEX, LEFT_USR, "
                                "RIGHT_USR, LEFT_COMMENT, RIGHT_COMMENT, "
                                "MODULENAME)\n";
  os << "#endif\n";
}

void swift::ide::api::DiffItem::undef(llvm::raw_ostream &os) {
  os << "#undef " << head() << "\n";
}

void swift::ide::api::DiffItem::streamDef(llvm::raw_ostream &S) const {
  S << head() << "(" << NodeKind << ", " << DiffKind << ", \"" << ChildIndex
    << "\", \"" << LeftUsr << "\", \"" << RightUsr << "\", \""
    << LeftComment << "\", \"" << RightComment
    << "\", \"" << ModuleName << "\")";
}

StringRef swift::ide::api::TypeMemberDiffItem::head() {
  return "SDK_CHANGE_TYPE_MEMBER";
}

void swift::ide::api::TypeMemberDiffItem::describe(llvm::raw_ostream &os) {
  os << "#ifndef " << head() << "\n";
  os << "#define " << head() << "(USR, NEW_TYPE_NAME, NEW_PRINTED_NAME, "
                                "SELF_INDEX, OLD_PRINTED_NAME)\n";
  os << "#endif\n";
}

void swift::ide::api::TypeMemberDiffItem::undef(llvm::raw_ostream &os) {
  os << "#undef " << head() << "\n";
}

void swift::ide::api::TypeMemberDiffItem::streamDef(llvm::raw_ostream &os) const {
  std::string IndexContent = selfIndex.hasValue() ?
    std::to_string(selfIndex.getValue()) : "";
  os << head() << "("
     << "\"" << usr << "\"" << ", "
     << "\"" << newTypeName << "\"" << ", "
     << "\"" << newPrintedName << "\"" << ", "
     << "\"" << IndexContent << "\"" << ", "
     << "\"" << oldPrintedName << "\""
     << ")";
}

bool swift::ide::api::TypeMemberDiffItem::
operator<(TypeMemberDiffItem Other) const {
  return usr.compare(Other.usr) < 0;
}

StringRef swift::ide::api::NoEscapeFuncParam::head() {
  return "NOESCAPE_FUNC_PARAM";
}

void swift::ide::api::NoEscapeFuncParam::describe(llvm::raw_ostream &os) {
  os << "#ifndef " << head() << "\n";
  os << "#define " << head() << "(USR, Index)\n";
  os << "#endif\n";
}

void swift::ide::api::NoEscapeFuncParam::undef(llvm::raw_ostream &os) {
  os << "#undef " << head() << "\n";
}

void swift::ide::api::NoEscapeFuncParam::
streamDef(llvm::raw_ostream &os) const {
  os << head() << "(" << "\"" << Usr << "\"" << ", "
     << "\"" << Index << "\"" << ")";
}

bool swift::ide::api::NoEscapeFuncParam::
operator<(NoEscapeFuncParam Other) const {
  if (Usr != Other.Usr)
    return Usr.compare(Other.Usr) < 0;
  return Index < Other.Index;
}

StringRef swift::ide::api::OverloadedFuncInfo::head() {
  return "OVERLOAD_FUNC_TRAILING_CLOSURE";
}

void swift::ide::api::OverloadedFuncInfo::describe(llvm::raw_ostream &os) {
  os << "#ifndef " << head() << "\n";
  os << "#define " << head() << "(USR)\n";
  os << "#endif\n";
}

void swift::ide::api::OverloadedFuncInfo::undef(llvm::raw_ostream &os) {
  os << "#undef " << head() << "\n";
}

void swift::ide::api::OverloadedFuncInfo::
streamDef(llvm::raw_ostream &os) const {
  os << head() << "(" << "\"" << Usr << "\"" << ")";
}

bool swift::ide::api::OverloadedFuncInfo::
operator<(OverloadedFuncInfo Other) const {
  return Usr.compare(Other.Usr) < 0;
}
