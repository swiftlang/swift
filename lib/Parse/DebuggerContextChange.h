//===--- DebuggerContextChange.h --------------------------------*- C++ -*-===//
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

#ifndef SWIFT_PARSE_DEBUGGERCONTEXTCHANGE_H
#define SWIFT_PARSE_DEBUGGERCONTEXTCHANGE_H

#include "swift/AST/DebuggerClient.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/SourceFile.h"
#include "swift/Parse/Parser.h"

namespace swift {

/// A RAII object for deciding whether this DeclKind needs special
/// treatment when parsing in the "debugger context", and implementing
/// that treatment.  The problem arises because, when lldb
/// uses swift to parse expressions, it needs to emulate the current
/// frame's scope. We do that, for instance, by making a class extension
/// and running the code in a function in that extension.
///
/// This causes two kinds of issues:
/// 1) Some DeclKinds require to be parsed in TopLevel contexts only.
/// 2) Sometimes the debugger wants a Decl to live beyond the current
/// function invocation, in which case it should be parsed at the
/// file scope level so it will be set up correctly for this purpose.
///
/// Creating an instance of this object will cause it to figure out
/// whether we are in the debugger function, whether it needs to swap
/// the Decl that is currently being parsed.
/// If you have created the object, instead of returning the result
/// with makeParserResult, use the object's fixupParserResult.  If
/// no swap has occurred, these methods will work the same.
/// If the decl has been moved, then Parser::markWasHandled will be
/// called on the Decl, and you should call declWasHandledAlready
/// before you consume the Decl to see if you actually need to
/// consume it.
/// If you are making one of these objects to address issue 1, call
/// the constructor that only takes a DeclKind, and it will be moved
/// unconditionally.  Otherwise pass in the Name and DeclKind and the
/// DebuggerClient will be asked whether to move it or not.
class DebuggerContextChange {
protected:
  Parser &P;
  Identifier Name;
  SourceFile *SF;
  Optional<Parser::ContextChange> CC;

public:
  DebuggerContextChange(Parser &P) : P(P), SF(nullptr) {
    if (!inDebuggerContext())
      return;
    else
      switchContext();
  }

  DebuggerContextChange(Parser &P, Identifier &Name, DeclKind Kind)
      : P(P), Name(Name), SF(nullptr) {
    if (!inDebuggerContext())
      return;
    bool globalize = false;

    DebuggerClient *debug_client = getDebuggerClient();
    if (!debug_client)
      return;

    globalize = debug_client->shouldGlobalize(Name, Kind);

    if (globalize)
      switchContext();
  }

  bool movedToTopLevel() { return CC.hasValue(); }

  template <typename T>
  ParserResult<T> fixupParserResult(ParserResult<T> &Result) {
    ParserStatus Status = Result;
    return fixupParserResult(Status, Result.getPtrOrNull());
  }

  template <typename T> ParserResult<T> fixupParserResult(T *D) {
    if (CC.hasValue()) {
      swapDecl(D);
    }
    return ParserResult<T>(D);
  }

  template <typename T>
  ParserResult<T> fixupParserResult(ParserStatus Status, T *D) {
    if (CC.hasValue() && !Status.isError()) {
      // If there is an error, don't do our splicing trick,
      // just return the Decl and the status for reporting.
      swapDecl(D);
    }
    return makeParserResult(Status, D);
  }

  // The destructor doesn't need to do anything, the CC's destructor will
  // pop the context if we set it.
  ~DebuggerContextChange() {}

protected:
  DebuggerClient *getDebuggerClient() {
    ModuleDecl *PM = P.CurDeclContext->getParentModule();
    if (!PM)
      return nullptr;
    else
      return PM->getDebugClient();
  }

  bool inDebuggerContext() {
    if (!P.Context.LangOpts.DebuggerSupport)
      return false;
    if (!P.CurDeclContext)
      return false;
    auto *func_decl = dyn_cast<FuncDecl>(P.CurDeclContext);
    if (!func_decl)
      return false;

    if (!func_decl->getAttrs().hasAttribute<LLDBDebuggerFunctionAttr>())
      return false;

    return true;
  }

  void switchContext() {
    SF = P.CurDeclContext->getParentSourceFile();
    CC.emplace(P, SF);
  }

  void swapDecl(Decl *D) {
    assert(SF);
    DebuggerClient *debug_client = getDebuggerClient();
    assert(debug_client);
    debug_client->didGlobalize(D);
    SF->Decls.push_back(D);
    P.markWasHandled(D);
  }
};
} // namespace swift

#endif
