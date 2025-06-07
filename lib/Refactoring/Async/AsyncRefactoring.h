//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFACTORING_ASYNCREFACTORING_H
#define SWIFT_REFACTORING_ASYNCREFACTORING_H

#include "ContextFinder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsRefactoring.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ForeignAsyncConvention.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/IDE/Utils.h"
#include "swift/Parse/Lexer.h"

namespace swift {
namespace refactoring {
namespace asyncrefactorings {

using namespace swift::ide;

FuncDecl *getUnderlyingFunc(const Expr *Fn);

/// Describes the expressions to be kept from a call to the handler in a
/// function that has (or will have ) and async alternative. Eg.
/// ```
/// func toBeAsync(completion: (String?, Error?) -> Void) {
///   ...
///   completion("something", nil) // Result = ["something"], IsError = false
///   ...
///   completion(nil, MyError.Bad) // Result = [MyError.Bad], IsError = true
/// }
class HandlerResult {
  SmallVector<Argument, 2> Args;
  bool IsError = false;

public:
  HandlerResult() {}

  HandlerResult(ArrayRef<Argument> ArgsRef)
      : Args(ArgsRef.begin(), ArgsRef.end()) {}

  HandlerResult(Argument Arg, bool IsError) : IsError(IsError) {
    Args.push_back(Arg);
  }

  bool isError() { return IsError; }

  ArrayRef<Argument> args() { return Args; }
};

/// The type of the handler, ie. whether it takes regular parameters or a
/// single parameter of `Result` type.
enum class HandlerType { INVALID, PARAMS, RESULT };

/// A single return type of a refactored async function. If the async function
/// returns a tuple, each element of the tuple (represented by a \c
/// LabeledReturnType) might have a label, otherwise the \p Label is empty.
struct LabeledReturnType {
  Identifier Label;
  swift::Type Ty;

  LabeledReturnType(Identifier Label, swift::Type Ty) : Label(Label), Ty(Ty) {}
};

/// Given a function with an async alternative (or one that *could* have an
/// async alternative), stores information about the completion handler.
/// The completion handler can be either a variable (which includes a parameter)
/// or a function
struct AsyncHandlerDesc {
  PointerUnion<const VarDecl *, const AbstractFunctionDecl *> Handler = nullptr;
  HandlerType Type = HandlerType::INVALID;
  bool HasError = false;

  static AsyncHandlerDesc get(const ValueDecl *Handler, bool RequireName);

  bool isValid() const { return Type != HandlerType::INVALID; }

  /// Return the declaration of the completion handler as a \c ValueDecl.
  /// In practice, the handler will always be a \c VarDecl or \c
  /// AbstractFunctionDecl.
  /// \c getNameStr and \c getType provide access functions that are available
  /// for both variables and functions, but not on \c ValueDecls.
  const ValueDecl *getHandler() const;

  /// Return the name of the completion handler. If it is a variable, the
  /// variable name, if it's a function, the function base name.
  StringRef getNameStr() const;

  HandlerType getHandlerType() const { return Type; }

  /// Get the type of the completion handler.
  swift::Type getType() const;

  ArrayRef<AnyFunctionType::Param> params() const;

  /// Retrieve the parameters relevant to a successful return from the
  /// completion handler. This drops the Error parameter if present.
  ArrayRef<AnyFunctionType::Param> getSuccessParams() const;

  /// If the completion handler has an Error parameter, return it.
  std::optional<AnyFunctionType::Param> getErrorParam() const;

  /// Get the type of the error that will be thrown by the \c async method or \c
  /// None if the completion handler doesn't accept an error parameter.
  /// This may be more specialized than the generic 'Error' type if the
  /// completion handler of the converted function takes a more specialized
  /// error type.
  std::optional<swift::Type> getErrorType() const;

  /// The `CallExpr` if the given node is a call to the `Handler`
  CallExpr *getAsHandlerCall(ASTNode Node) const;

  /// Returns \c true if the call to the completion handler contains possibly
  /// non-nil values for both the success and error parameters, e.g.
  /// \code
  ///   completion(result, error)
  /// \endcode
  /// This can only happen if the completion handler is a params handler.
  bool isAmbiguousCallToParamHandler(const CallExpr *CE) const;

  /// Given a call to the `Handler`, extract the expressions to be returned or
  /// thrown, taking care to remove the `.success`/`.failure` if it's a
  /// `RESULT` handler type.
  /// If the call is ambiguous (contains potentially non-nil arguments to both
  /// the result and the error parameters), the \p ReturnErrorArgsIfAmbiguous
  /// determines whether the success or error parameters are passed.
  HandlerResult extractResultArgs(const CallExpr *CE,
                                  bool ReturnErrorArgsIfAmbiguous) const;

  // Convert the type of a success parameter in the completion handler function
  // to a return type suitable for an async function. If there is an error
  // parameter present e.g (T?, Error?) -> Void, this unwraps a level of
  // optionality from T?. If this is a Result<T, U> type, returns the success
  // type T.
  swift::Type getSuccessParamAsyncReturnType(swift::Type Ty) const;

  /// If the async function returns a tuple, the label of the \p Index -th
  /// element in the returned tuple. If the function doesn't return a tuple or
  /// the element is unlabeled, an empty identifier is returned.
  Identifier getAsyncReturnTypeLabel(size_t Index) const;

  /// Gets the return value types for the async equivalent of this handler.
  ArrayRef<LabeledReturnType>
  getAsyncReturnTypes(SmallVectorImpl<LabeledReturnType> &Scratch) const;

  /// Whether the async equivalent of this handler returns Void.
  bool willAsyncReturnVoid() const;

  // TODO: If we have an async alternative we should check its result types
  //       for whether to unwrap or not
  bool shouldUnwrap(swift::Type Ty) const {
    return HasError && Ty->isOptional();
  }
};

/// Given a completion handler that is part of a function signature, stores
/// information about that completion handler and its index within the function
/// declaration.
struct AsyncHandlerParamDesc : public AsyncHandlerDesc {
  /// Enum to represent the position of the completion handler param within
  /// the parameter list. Given `(A, B, C, D)`:
  ///   - A is `First`
  ///   - B and C are `Middle`
  ///   - D is `Last`
  /// The position is `Only` if there's a single parameter that is the
  /// completion handler and `None` if there is no handler.
  enum class Position { First, Middle, Last, Only, None };

  /// The function the completion handler is a parameter of.
  const FuncDecl *Func = nullptr;
  /// The index of the completion handler in the function that declares it.
  unsigned Index = 0;
  /// The async alternative, if one is found.
  const AbstractFunctionDecl *Alternative = nullptr;

  AsyncHandlerParamDesc() : AsyncHandlerDesc() {}
  AsyncHandlerParamDesc(const AsyncHandlerDesc &Handler, const FuncDecl *Func,
                        unsigned Index, const AbstractFunctionDecl *Alternative)
      : AsyncHandlerDesc(Handler), Func(Func), Index(Index),
        Alternative(Alternative) {}

  static AsyncHandlerParamDesc find(const FuncDecl *FD,
                                    bool RequireAttributeOrName) {
    if (!FD || FD->hasAsync() || FD->hasThrows() ||
        !FD->getResultInterfaceType()->isVoid())
      return AsyncHandlerParamDesc();

    const auto *Alternative = FD->getAsyncAlternative();
    std::optional<unsigned> Index =
        FD->findPotentialCompletionHandlerParam(Alternative);
    if (!Index)
      return AsyncHandlerParamDesc();

    bool RequireName = RequireAttributeOrName && !Alternative;
    return AsyncHandlerParamDesc(
        AsyncHandlerDesc::get(FD->getParameters()->get(*Index), RequireName),
        FD, *Index, Alternative);
  }

  /// Build an @available attribute with the name of the async alternative as
  /// the \c renamed argument, followed by a newline.
  SmallString<128> buildRenamedAttribute() const {
    SmallString<128> AvailabilityAttr;
    llvm::raw_svector_ostream OS(AvailabilityAttr);

    // If there's an alternative then there must already be an attribute,
    // don't add another.
    if (!isValid() || Alternative)
      return AvailabilityAttr;

    DeclName Name = Func->getName();
    OS << "@available(*, renamed: \"" << Name.getBaseName() << "(";
    ArrayRef<Identifier> ArgNames = Name.getArgumentNames();
    for (size_t I = 0; I < ArgNames.size(); ++I) {
      if (I != Index) {
        OS << ArgNames[I] << tok::colon;
      }
    }
    OS << ")\")\n";

    return AvailabilityAttr;
  }

  /// Retrieves the parameter decl for the completion handler parameter, or
  /// \c nullptr if no valid completion parameter is present.
  const ParamDecl *getHandlerParam() const {
    if (!isValid())
      return nullptr;
    return cast<ParamDecl>(getHandler());
  }

  /// See \c Position
  Position handlerParamPosition() const {
    if (!isValid())
      return Position::None;
    const auto *Params = Func->getParameters();
    if (Params->size() == 1)
      return Position::Only;
    if (Index == 0)
      return Position::First;
    if (Index == Params->size() - 1)
      return Position::Last;
    return Position::Middle;
  }

  bool operator==(const AsyncHandlerParamDesc &Other) const {
    return Handler == Other.Handler && Type == Other.Type &&
           HasError == Other.HasError && Index == Other.Index;
  }

  bool alternativeIsAccessor() const {
    return isa_and_nonnull<AccessorDecl>(Alternative);
  }
};

/// The type of a condition in a conditional statement.
enum class ConditionType {
  NIL,             // == nil
  NOT_NIL,         // != nil
  IS_TRUE,         // if b
  IS_FALSE,        // if !b
  SUCCESS_PATTERN, // case .success
  FAILURE_PATTEN   // case .failure
};

/// Indicates whether a condition describes a success or failure path. For
/// example, a check for whether an error parameter is present is a failure
/// path. A check for a nil error parameter is a success path. This is distinct
/// from ConditionType, as it relies on contextual information about what values
/// need to be checked for success or failure.
enum class ConditionPath { SUCCESS, FAILURE };

/// Finds the `Subject` being compared to in various conditions. Also finds any
/// pattern that may have a bound name.
struct CallbackCondition {
  std::optional<ConditionType> Type;
  const Decl *Subject = nullptr;
  const Pattern *BindPattern = nullptr;

  /// Initializes a `CallbackCondition` with a `!=` or `==` comparison of
  /// an `Optional` typed `Subject` to `nil`, or a `Bool` typed `Subject` to a
  /// boolean literal, ie.
  ///   - `<Subject> != nil`
  ///   - `<Subject> == nil`
  ///   - `<Subject> != true`
  ///   - `<Subject> == false`
  CallbackCondition(const BinaryExpr *BE, const FuncDecl *Operator);

  /// A bool condition expression.
  explicit CallbackCondition(const Expr *E);

  /// Initializes a `CallbackCondition` with binding of an `Optional` or
  /// `Result` typed `Subject`, ie.
  ///   - `let bind = <Subject>`
  ///   - `case .success(let bind) = <Subject>`
  ///   - `case .failure(let bind) = <Subject>`
  ///   - `let bind = try? <Subject>.get()`
  CallbackCondition(const Pattern *P, const Expr *Init);

  /// Initializes a `CallbackCondtion` from a case statement inside a switch
  /// on `Subject` with `Result` type, ie.
  /// ```
  /// switch <Subject> {
  /// case .success(let bind):
  /// case .failure(let bind):
  /// }
  /// ```
  CallbackCondition(const Decl *Subject, const CaseLabelItem *CaseItem);

  bool isValid() const { return Type.has_value(); }

private:
  void initFromEnumPattern(const Decl *D, const EnumElementPattern *EEP);

  void initFromOptionalTry(const class Pattern *P, const OptionalTryExpr *OTE);
};

/// A CallbackCondition with additional semantic information about whether it
/// is for a success path or failure path.
struct ClassifiedCondition : public CallbackCondition {
  ConditionPath Path;

  /// Whether this represents an Obj-C style boolean flag check for success.
  bool IsObjCStyleFlagCheck;

  explicit ClassifiedCondition(CallbackCondition Cond, ConditionPath Path,
                               bool IsObjCStyleFlagCheck)
      : CallbackCondition(Cond), Path(Path),
        IsObjCStyleFlagCheck(IsObjCStyleFlagCheck) {}
};

/// A wrapper for a map of parameter decls to their classified conditions, or
/// \c None if they are not present in any conditions.
struct ClassifiedCallbackConditions final
    : llvm::MapVector<const Decl *, ClassifiedCondition> {
  std::optional<ClassifiedCondition> lookup(const Decl *D) const {
    auto Res = find(D);
    if (Res == end())
      return std::nullopt;
    return Res->second;
  }
};

/// A list of nodes to print, along with a list of locations that may have
/// preceding comments attached, which also need printing. For example:
///
/// \code
/// if .random() {
///   // a
///   print("hello")
///   // b
/// }
/// \endcode
///
/// To print out the contents of the if statement body, we'll include the AST
/// node for the \c print call. This will also include the preceding comment
/// \c a, but won't include the comment \c b. To ensure the comment \c b gets
/// printed, the SourceLoc for the closing brace \c } is added as a possible
/// comment loc.
class NodesToPrint {
  SmallVector<ASTNode, 0> Nodes;
  SmallVector<SourceLoc, 2> PossibleCommentLocs;

public:
  NodesToPrint() {}
  NodesToPrint(ArrayRef<ASTNode> Nodes, ArrayRef<SourceLoc> PossibleCommentLocs)
      : Nodes(Nodes.begin(), Nodes.end()),
        PossibleCommentLocs(PossibleCommentLocs.begin(),
                            PossibleCommentLocs.end()) {}

  ArrayRef<ASTNode> getNodes() const { return Nodes; }
  ArrayRef<SourceLoc> getPossibleCommentLocs() const {
    return PossibleCommentLocs;
  }

  /// Add an AST node to print.
  void addNode(ASTNode Node) {
    // Note we skip vars as they'll be printed as a part of their
    // PatternBindingDecl.
    if (!Node.isDecl(DeclKind::Var))
      Nodes.push_back(Node);
  }

  /// Add a SourceLoc which may have a preceding comment attached. If so, the
  /// comment will be printed out at the appropriate location.
  void addPossibleCommentLoc(SourceLoc Loc) {
    if (Loc.isValid())
      PossibleCommentLocs.push_back(Loc);
  }

  /// Add all the nodes in the brace statement to the list of nodes to print.
  /// This should be preferred over adding the nodes manually as it picks up the
  /// end location of the brace statement as a possible comment loc, ensuring
  /// that we print any trailing comments in the brace statement.
  void addNodesInBraceStmt(BraceStmt *Brace) {
    for (auto Node : Brace->getElements())
      addNode(Node);

    // Ignore the end locations of implicit braces, as they're likely bogus.
    // e.g for a case statement, the r-brace loc points to the last token of the
    // last node in the body.
    if (!Brace->isImplicit())
      addPossibleCommentLoc(Brace->getRBraceLoc());
  }

  /// Add the nodes and comment locs from another NodesToPrint.
  void addNodes(NodesToPrint OtherNodes) {
    Nodes.append(OtherNodes.Nodes.begin(), OtherNodes.Nodes.end());
    PossibleCommentLocs.append(OtherNodes.PossibleCommentLocs.begin(),
                               OtherNodes.PossibleCommentLocs.end());
  }

  /// Whether the last recorded node is an explicit return or break statement.
  bool hasTrailingReturnOrBreak() const {
    if (Nodes.empty())
      return false;
    return (Nodes.back().isStmt(StmtKind::Return) ||
            Nodes.back().isStmt(StmtKind::Break)) &&
           !Nodes.back().isImplicit();
  }

  /// If the last recorded node is an explicit return or break statement that
  /// can be safely dropped, drop it from the list.
  void dropTrailingReturnOrBreakIfPossible() {
    if (!hasTrailingReturnOrBreak())
      return;

    auto *Node = Nodes.back().get<Stmt *>();

    // If this is a return statement with return expression, let's preserve it.
    if (auto *RS = dyn_cast<ReturnStmt>(Node)) {
      if (RS->hasResult())
        return;
    }

    // Remove the node from the list, but make sure to add it as a possible
    // comment loc to preserve any of its attached comments.
    Nodes.pop_back();
    addPossibleCommentLoc(Node->getStartLoc());
  }

  /// Returns a list of nodes to print in a brace statement. This picks up the
  /// end location of the brace statement as a possible comment loc, ensuring
  /// that we print any trailing comments in the brace statement.
  static NodesToPrint inBraceStmt(BraceStmt *stmt) {
    NodesToPrint Nodes;
    Nodes.addNodesInBraceStmt(stmt);
    return Nodes;
  }
};

/// The statements within the closure of call to a function taking a callback
/// are split into a `SuccessBlock` and `ErrorBlock` (`ClassifiedBlocks`).
/// This class stores the nodes for each block, as well as a mapping of
/// decls to any patterns they are used in.
class ClassifiedBlock {
  NodesToPrint Nodes;

  // A mapping of closure params to a list of patterns that bind them.
  using ParamPatternBindingsMap =
      llvm::MapVector<const Decl *, TinyPtrVector<const Pattern *>>;
  ParamPatternBindingsMap ParamPatternBindings;

public:
  const NodesToPrint &nodesToPrint() const { return Nodes; }

  /// Attempt to retrieve an existing bound name for a closure parameter, or
  /// an empty string if there's no suitable existing binding.
  StringRef boundName(const Decl *D) const {
    // Adopt the same name as the representative single pattern, if it only
    // binds a single var.
    if (auto *P = getSinglePatternFor(D)) {
      if (P->getSingleVar())
        return P->getBoundName().str();
    }
    return StringRef();
  }

  /// Checks whether a closure parameter can be represented by a single pattern
  /// that binds it. If the param is only bound by a single pattern, that will
  /// be returned. If there's a pattern with a single var that binds it, that
  /// will be returned, preferring a 'let' pattern to prefer out of line
  /// printing of 'var' patterns.
  const Pattern *getSinglePatternFor(const Decl *D) const {
    auto Iter = ParamPatternBindings.find(D);
    if (Iter == ParamPatternBindings.end())
      return nullptr;

    const auto &Patterns = Iter->second;
    if (Patterns.empty())
      return nullptr;
    if (Patterns.size() == 1)
      return Patterns[0];

    // If we have multiple patterns, search for the best single var pattern to
    // use, preferring a 'let' binding.
    const Pattern *FirstSingleVar = nullptr;
    for (auto *P : Patterns) {
      if (!P->getSingleVar())
        continue;

      if (!P->hasAnyMutableBindings())
        return P;

      if (!FirstSingleVar)
        FirstSingleVar = P;
    }
    return FirstSingleVar;
  }

  /// Retrieve any bound vars that are effectively aliases of a given closure
  /// parameter.
  llvm::SmallDenseSet<const Decl *> getAliasesFor(const Decl *D) const {
    auto Iter = ParamPatternBindings.find(D);
    if (Iter == ParamPatternBindings.end())
      return {};

    llvm::SmallDenseSet<const Decl *> Aliases;

    // The single pattern that we replace the decl with is always an alias.
    if (auto *P = getSinglePatternFor(D)) {
      if (auto *SingleVar = P->getSingleVar())
        Aliases.insert(SingleVar);
    }

    // Any other let bindings we have are also aliases.
    for (auto *P : Iter->second) {
      if (auto *SingleVar = P->getSingleVar()) {
        if (!P->hasAnyMutableBindings())
          Aliases.insert(SingleVar);
      }
    }
    return Aliases;
  }

  const ParamPatternBindingsMap &paramPatternBindings() const {
    return ParamPatternBindings;
  }

  void addNodesInBraceStmt(BraceStmt *Brace) {
    Nodes.addNodesInBraceStmt(Brace);
  }
  void addPossibleCommentLoc(SourceLoc Loc) {
    Nodes.addPossibleCommentLoc(Loc);
  }
  void addAllNodes(NodesToPrint OtherNodes) {
    Nodes.addNodes(std::move(OtherNodes));
  }

  void addNode(ASTNode Node) { Nodes.addNode(Node); }

  void addBinding(const ClassifiedCondition &FromCondition) {
    auto *P = FromCondition.BindPattern;
    if (!P)
      return;

    // Patterns that don't bind anything aren't interesting.
    SmallVector<VarDecl *, 2> Vars;
    P->collectVariables(Vars);
    if (Vars.empty())
      return;

    ParamPatternBindings[FromCondition.Subject].push_back(P);
  }

  void addAllBindings(const ClassifiedCallbackConditions &FromConditions) {
    for (auto &Entry : FromConditions)
      addBinding(Entry.second);
  }
};

/// The type of block rewritten code may be placed in.
enum class BlockKind { SUCCESS, ERROR, FALLBACK };

/// A completion handler function parameter that is known to be a Bool flag
/// indicating success or failure.
struct KnownBoolFlagParam {
  const ParamDecl *Param;
  bool IsSuccessFlag;
};

/// A set of parameters for a completion callback closure.
class ClosureCallbackParams final {
  const AsyncHandlerParamDesc &HandlerDesc;
  ArrayRef<const ParamDecl *> AllParams;
  llvm::SetVector<const ParamDecl *> SuccessParams;
  const ParamDecl *ErrParam = nullptr;
  std::optional<KnownBoolFlagParam> BoolFlagParam;

public:
  ClosureCallbackParams(const AsyncHandlerParamDesc &HandlerDesc,
                        const ClosureExpr *Closure)
      : HandlerDesc(HandlerDesc),
        AllParams(Closure->getParameters()->getArray()) {
    assert(AllParams.size() == HandlerDesc.params().size());
    assert(HandlerDesc.Type != HandlerType::RESULT || AllParams.size() == 1);

    SuccessParams.insert(AllParams.begin(), AllParams.end());
    if (HandlerDesc.HasError && HandlerDesc.Type == HandlerType::PARAMS)
      ErrParam = SuccessParams.pop_back_val();

    // Check to see if we have a known bool flag parameter.
    if (auto *AsyncAlt = HandlerDesc.Func->getAsyncAlternative()) {
      if (auto Conv = AsyncAlt->getForeignAsyncConvention()) {
        auto FlagIdx = Conv->completionHandlerFlagParamIndex();
        if (FlagIdx && *FlagIdx >= 0 && *FlagIdx < AllParams.size()) {
          auto IsSuccessFlag = Conv->completionHandlerFlagIsErrorOnZero();
          BoolFlagParam = {AllParams[*FlagIdx], IsSuccessFlag};
        }
      }
    }
  }

  /// Whether the closure has a particular parameter.
  bool hasParam(const ParamDecl *Param) const {
    return Param == ErrParam || SuccessParams.contains(Param);
  }

  /// Whether \p Param is a success param.
  bool isSuccessParam(const ParamDecl *Param) const {
    return SuccessParams.contains(Param);
  }

  /// Whether \p Param is a closure parameter that may be unwrapped. This
  /// includes optional parameters as well as \c Result parameters that may be
  /// unwrapped through e.g 'try? res.get()'.
  bool isUnwrappableParam(const ParamDecl *Param) const {
    if (!hasParam(Param))
      return false;
    if (getResultParam() == Param)
      return true;
    return HandlerDesc.shouldUnwrap(Param->getTypeInContext());
  }

  /// Whether \p Param is the known Bool parameter that indicates success or
  /// failure.
  bool isKnownBoolFlagParam(const ParamDecl *Param) const {
    if (auto BoolFlag = getKnownBoolFlagParam())
      return BoolFlag->Param == Param;
    return false;
  }

  /// Whether \p Param is a closure parameter that has a binding available in
  /// the async variant of the call for a particular \p Block.
  bool hasBinding(const ParamDecl *Param, BlockKind Block) const {
    switch (Block) {
    case BlockKind::SUCCESS:
      // Known bool flags get dropped from the imported async variant.
      if (isKnownBoolFlagParam(Param))
        return false;

      return isSuccessParam(Param);
    case BlockKind::ERROR:
      return Param == ErrParam;
    case BlockKind::FALLBACK:
      // We generally want to bind everything in the fallback case.
      return hasParam(Param);
    }
    llvm_unreachable("Unhandled case in switch");
  }

  /// Retrieve the parameters to bind in a given \p Block.
  TinyPtrVector<const ParamDecl *> getParamsToBind(BlockKind Block) {
    TinyPtrVector<const ParamDecl *> Result;
    for (auto *Param : AllParams) {
      if (hasBinding(Param, Block))
        Result.push_back(Param);
    }
    return Result;
  }

  /// If there is a known Bool flag parameter indicating success or failure,
  /// returns it, \c None otherwise.
  std::optional<KnownBoolFlagParam> getKnownBoolFlagParam() const {
    return BoolFlagParam;
  }

  /// All the parameters of the closure passed as the completion handler.
  ArrayRef<const ParamDecl *> getAllParams() const { return AllParams; }

  /// The success parameters of the closure passed as the completion handler.
  /// Note this includes a \c Result parameter.
  ArrayRef<const ParamDecl *> getSuccessParams() const {
    return SuccessParams.getArrayRef();
  }

  /// The error parameter of the closure passed as the completion handler, or
  /// \c nullptr if there is no error parameter.
  const ParamDecl *getErrParam() const { return ErrParam; }

  /// If the closure has a single \c Result parameter, returns it, \c nullptr
  /// otherwise.
  const ParamDecl *getResultParam() const {
    return HandlerDesc.Type == HandlerType::RESULT ? SuccessParams[0] : nullptr;
  }
};

struct ClassifiedBlocks {
  ClassifiedBlock SuccessBlock;
  ClassifiedBlock ErrorBlock;
};

/// Classifer of callback closure statements that that have either multiple
/// non-Result parameters or a single Result parameter and return Void.
///
/// It performs a (possibly incorrect) best effort and may give up in certain
/// cases. Aims to cover the idiomatic cases of either having no error
/// parameter at all, or having success/error code wrapped in ifs/guards/switch
/// using either pattern binding or nil checks.
///
/// Code outside any clear conditions is assumed to be solely part of the
/// success block for now, though some heuristics could be added to classify
/// these better in the future.
struct CallbackClassifier {
  /// Updates the success and error block of `Blocks` with nodes and bound
  /// names from `Body`. Errors are added through `DiagEngine`, possibly
  /// resulting in partially filled out blocks.
  static void classifyInto(ClassifiedBlocks &Blocks,
                           const ClosureCallbackParams &Params,
                           llvm::DenseSet<SwitchStmt *> &HandledSwitches,
                           DiagnosticEngine &DiagEngine, BraceStmt *Body);

private:
  ClassifiedBlocks &Blocks;
  const ClosureCallbackParams &Params;
  llvm::DenseSet<SwitchStmt *> &HandledSwitches;
  DiagnosticEngine &DiagEngine;
  ClassifiedBlock *CurrentBlock;

  /// This is set to \c true if we're currently classifying on a known condition
  /// path, where \c CurrentBlock is set to the appropriate block. This lets us
  /// be more lenient with unhandled conditions as we already know the block
  /// we're supposed to be in.
  bool IsKnownConditionPath = false;

  CallbackClassifier(ClassifiedBlocks &Blocks,
                     const ClosureCallbackParams &Params,
                     llvm::DenseSet<SwitchStmt *> &HandledSwitches,
                     DiagnosticEngine &DiagEngine)
      : Blocks(Blocks), Params(Params), HandledSwitches(HandledSwitches),
        DiagEngine(DiagEngine), CurrentBlock(&Blocks.SuccessBlock) {}

  /// Attempt to apply custom classification logic to a given node, returning
  /// \c true if the node was classified, otherwise \c false.
  bool tryClassifyNode(ASTNode Node);

  /// Classify a node, or add the node to the block if it cannot be classified.
  /// Returns \c true if there was an error.
  bool classifyNode(ASTNode Node);

  void classifyNodes(ArrayRef<ASTNode> Nodes, SourceLoc EndCommentLoc);

  /// Whether any of the provided ASTNodes have a child expression that force
  /// unwraps the error parameter. Note that this doesn't walk into new scopes.
  bool hasForceUnwrappedErrorParam(ArrayRef<ASTNode> Nodes);

  /// Given a callback condition, classify it as a success or failure path.
  std::optional<ClassifiedCondition>
  classifyCallbackCondition(const CallbackCondition &Cond,
                            const NodesToPrint &SuccessNodes, Stmt *ElseStmt);

  /// Classifies all the conditions present in a given StmtCondition, taking
  /// into account its success body and failure body. Returns \c true if there
  /// were any conditions that couldn't be classified, \c false otherwise.
  bool classifyConditionsOf(StmtCondition Cond,
                            const NodesToPrint &ThenNodesToPrint,
                            Stmt *ElseStmt,
                            ClassifiedCallbackConditions &Conditions);

  /// Classifies the conditions of a conditional statement, and adds the
  /// necessary nodes to either the success or failure block.
  void classifyConditional(Stmt *Statement, StmtCondition Condition,
                           NodesToPrint ThenNodesToPrint, Stmt *ElseStmt);

  /// Adds \p Nodes to \p Block, potentially flipping the current block if we
  /// can determine that the nodes being added will cause control flow to leave
  /// the scope.
  ///
  /// \param Block The block to add the nodes to.
  /// \param OtherBlock The block for the opposing condition path.
  /// \param Nodes The nodes to add.
  /// \param AlwaysExitsScope Whether the nodes being added always exit the
  /// scope, and therefore whether the current block should be flipped.
  void setNodes(ClassifiedBlock *Block, ClassifiedBlock *OtherBlock,
                NodesToPrint Nodes, bool AlwaysExitsScope = false);

  void classifySwitch(SwitchStmt *SS);
};

class DeclCollector : private SourceEntityWalker {
  llvm::DenseSet<const Decl *> &Decls;

public:
  /// Collect all explicit declarations declared in \p Scope (or \p SF if
  /// \p Scope is a nullptr) that are not within their own scope.
  static void collect(BraceStmt *Scope, SourceFile &SF,
                      llvm::DenseSet<const Decl *> &Decls);

private:
  DeclCollector(llvm::DenseSet<const Decl *> &Decls) : Decls(Decls) {}

  bool walkToDeclPre(Decl *D, CharSourceRange Range) override;

  bool walkToExprPre(Expr *E) override;

  bool walkToStmtPre(Stmt *S) override;
};

class ReferenceCollector : private SourceEntityWalker {
  SourceManager *SM;
  llvm::DenseSet<const Decl *> DeclaredDecls;
  llvm::DenseSet<const Decl *> &ReferencedDecls;

  ASTNode Target;
  bool AfterTarget;

public:
  /// Collect all explicit references in \p Scope (or \p SF if \p Scope is
  /// a nullptr) that are after \p Target and not first declared. That is,
  /// references that we don't want to shadow with hoisted declarations.
  ///
  /// Also collect all declarations that are \c DeclContexts, which is an
  /// over-appoximation but let's us ignore them elsewhere.
  static void collect(ASTNode Target, BraceStmt *Scope, SourceFile &SF,
                      llvm::DenseSet<const Decl *> &Decls);

private:
  ReferenceCollector(ASTNode Target, SourceManager *SM,
                     llvm::DenseSet<const Decl *> &Decls)
      : SM(SM), DeclaredDecls(), ReferencedDecls(Decls), Target(Target),
        AfterTarget(false) {}

  bool walkToDeclPre(Decl *D, CharSourceRange Range) override;

  bool walkToExprPre(Expr *E) override;

  bool walkToStmtPre(Stmt *S) override;

  bool walkToPatternPre(Pattern *P) override;

  bool shouldWalkInto(SourceRange Range);
};

/// Similar to the \c ReferenceCollector but collects references in all scopes
/// without any starting point in each scope. In addition, it tracks the number
/// of references to a decl in a given scope.
class ScopedDeclCollector : private SourceEntityWalker {
public:
  using DeclsTy = llvm::DenseSet<const Decl *>;
  using RefDeclsTy = llvm::DenseMap<const Decl *, /*numRefs*/ unsigned>;

private:
  using ScopedDeclsTy = llvm::DenseMap<const Stmt *, RefDeclsTy>;

  struct Scope {
    DeclsTy DeclaredDecls;
    RefDeclsTy *ReferencedDecls;
    Scope(RefDeclsTy *ReferencedDecls)
        : DeclaredDecls(), ReferencedDecls(ReferencedDecls) {}
  };

  ScopedDeclsTy ReferencedDecls;
  llvm::SmallVector<Scope, 4> ScopeStack;

public:
  /// Starting at \c Scope, collect all explicit references in every scope
  /// within (including the initial) that are not first declared, ie. those that
  /// could end up shadowed. Also include all \c DeclContext declarations as
  /// we'd like to avoid renaming functions and types completely.
  void collect(ASTNode Node) { walk(Node); }

  const RefDeclsTy *getReferencedDecls(Stmt *Scope) const;

private:
  bool walkToDeclPre(Decl *D, CharSourceRange Range) override;

  bool walkToExprPre(Expr *E) override;

  bool walkToStmtPre(Stmt *S) override;

  bool walkToStmtPost(Stmt *S) override;
};

/// Checks whether an ASTNode contains a reference to a given declaration.
class DeclReferenceFinder : private SourceEntityWalker {
  bool HasFoundReference = false;
  const Decl *Search;

  bool walkToExprPre(Expr *E) override;

  DeclReferenceFinder(const Decl *Search) : Search(Search) {}

public:
  /// Returns \c true if \p node contains a reference to \p Search, \c false
  /// otherwise.
  static bool containsReference(ASTNode Node, const ValueDecl *Search);
};

/// Builds up async-converted code for an AST node.
///
/// If it is a function, its declaration will have `async` added. If a
/// completion handler is present, it will be removed and the return type of
/// the function will reflect the parameters of the handler, including an
/// added `throws` if necessary.
///
/// Calls to the completion handler are replaced with either a `return` or
/// `throws` depending on the arguments.
///
/// Calls to functions with an async alternative will be replaced with a call
/// to the alternative, possibly wrapped in a do/catch. The do/catch is skipped
/// if the closure either:
///   1. Has no error
///   2. Has an error but no error handling (eg. just ignores)
///   3. Has error handling that only calls the containing function's handler
///      with an error matching the error argument
///
/// (2) is technically not the correct translation, but in practice it's likely
/// the code a user would actually want.
///
/// If the success vs error handling split inside the closure cannot be
/// determined and the closure takes regular parameters (ie. not a Result), a
/// fallback translation is used that keeps all the same variable names and
/// simply moves the code within the closure out.
///
/// The fallback is generally avoided, however, since it's quite unlikely to be
/// the code the user intended. In most cases the refactoring will continue,
/// with any unhandled decls wrapped in placeholders instead.
class AsyncConverter : private SourceEntityWalker {
  struct Scope {
    llvm::DenseSet<DeclBaseName> Names;

    /// If this scope is wrapped in a \c withChecked(Throwing)Continuation, the
    /// name of the continuation that must be resumed where there previously was
    /// a call to the function's completion handler.
    /// Otherwise an empty identifier.
    Identifier ContinuationName;

    Scope(Identifier ContinuationName)
        : Names(), ContinuationName(ContinuationName) {}

    /// Whether this scope is wrapped in a \c withChecked(Throwing)Continuation.
    bool isWrappedInContination() const { return !ContinuationName.empty(); }
  };
  SourceFile *SF;
  SourceManager &SM;
  DiagnosticEngine &DiagEngine;

  // Node to convert
  ASTNode StartNode;

  // Completion handler of `StartNode` (if it's a function with an async
  // alternative)
  AsyncHandlerParamDesc TopHandler;

  SmallString<0> Buffer;
  llvm::raw_svector_ostream OS;

  // Decls where any force unwrap or optional chain of that decl should be
  // elided, e.g for a previously optional closure parameter that has become a
  // non-optional local.
  llvm::DenseSet<const Decl *> Unwraps;

  // Decls whose references should be replaced with, either because they no
  // longer exist or are a different type. Any replaced code should ideally be
  // handled by the refactoring properly, but that's not possible in all cases
  llvm::DenseSet<const Decl *> Placeholders;

  // Mapping from decl -> name, used as the name of possible new local
  // declarations of old completion handler parametes, as well as the
  // replacement for other hoisted declarations and their references
  llvm::DenseMap<const Decl *, Identifier> Names;

  /// The scopes (containing all name decls and whether the scope is wrapped in
  /// a continuation) as the AST is being walked. The first element is the
  /// initial scope and the last is the current scope.
  llvm::SmallVector<Scope, 4> Scopes;

  // Mapping of \c BraceStmt -> declarations referenced in that statement
  // without first being declared. These are used to fill the \c ScopeNames
  // map on entering that scope.
  ScopedDeclCollector ScopedDecls;

  /// The switch statements that have been re-written by this transform.
  llvm::DenseSet<SwitchStmt *> HandledSwitches;

  // The last source location that has been output. Used to output the source
  // between handled nodes
  SourceLoc LastAddedLoc;

  // Number of expressions (or pattern binding decl) currently nested in, taking
  // into account hoisting and the possible removal of ifs/switches
  int NestedExprCount = 0;

  // Whether a completion handler body is currently being hoisted out of its
  // call
  bool Hoisting = false;

  /// Whether a pattern is currently being converted.
  bool ConvertingPattern = false;

  /// A mapping of inline patterns to print for closure parameters.
  using InlinePatternsToPrint = llvm::DenseMap<const Decl *, const Pattern *>;

public:
  /// Convert a function
  AsyncConverter(SourceFile *SF, SourceManager &SM,
                 DiagnosticEngine &DiagEngine, AbstractFunctionDecl *FD,
                 const AsyncHandlerParamDesc &TopHandler)
      : SF(SF), SM(SM), DiagEngine(DiagEngine), StartNode(FD),
        TopHandler(TopHandler), OS(Buffer) {
    Placeholders.insert(TopHandler.getHandler());
    ScopedDecls.collect(FD);

    // Shouldn't strictly be necessary, but prefer possible shadowing over
    // crashes caused by a missing scope
    addNewScope({});
  }

  /// Convert a call
  AsyncConverter(SourceFile *SF, SourceManager &SM,
                 DiagnosticEngine &DiagEngine, CallExpr *CE, BraceStmt *Scope)
      : SF(SF), SM(SM), DiagEngine(DiagEngine), StartNode(CE), OS(Buffer) {
    ScopedDecls.collect(CE);

    // Create the initial scope, can be more accurate than the general
    // \c ScopedDeclCollector as there is a starting point.
    llvm::DenseSet<const Decl *> UsedDecls;
    DeclCollector::collect(Scope, *SF, UsedDecls);
    ReferenceCollector::collect(StartNode, Scope, *SF, UsedDecls);
    addNewScope(UsedDecls);
  }

  ASTContext &getASTContext() const { return SF->getASTContext(); }

  bool convert();

  /// When adding an async alternative method for the function declaration \c
  /// FD, this function tries to create a function body for the legacy function
  /// (the one with a completion handler), which calls the newly converted async
  /// function. There are certain situations in which we fail to create such a
  /// body, e.g. if the completion handler has the signature `(String, Error?)
  /// -> Void` in which case we can't synthesize the result of type \c String in
  /// the error case.
  bool createLegacyBody();

  /// Creates an async alternative function that forwards onto the completion
  /// handler function through
  /// withCheckedContinuation/withCheckedThrowingContinuation.
  bool createAsyncWrapper();

  void replace(ASTNode Node, SourceEditConsumer &EditConsumer,
               SourceLoc StartOverride = SourceLoc());

  void insertAfter(ASTNode Node, SourceEditConsumer &EditConsumer);

private:
  bool canCreateLegacyBody();

  /// Prints a tuple of elements, or a lone single element if only one is
  /// present, using the provided printing function.
  template <typename Container, typename PrintFn>
  void addTupleOf(const Container &Elements, llvm::raw_ostream &OS,
                  PrintFn PrintElt) {
    if (Elements.size() == 1) {
      PrintElt(Elements[0]);
      return;
    }
    OS << tok::l_paren;
    llvm::interleave(Elements, PrintElt, [&]() { OS << tok::comma << " "; });
    OS << tok::r_paren;
  }

  /// Retrieve the completion handler closure argument for an async wrapper
  /// function.
  std::string
  getAsyncWrapperCompletionClosure(StringRef ContName,
                                   const AsyncHandlerParamDesc &HandlerDesc);

  /// Retrieves the SourceRange of the preceding comment, or an invalid range if
  /// there is no preceding comment.
  CharSourceRange getPrecedingCommentRange(SourceLoc Loc);

  /// Retrieves the location for the start of a comment attached to the token
  /// at the provided location, or the location itself if there is no comment.
  SourceLoc getLocIncludingPrecedingComment(SourceLoc Loc);

  /// If the provided SourceLoc has a preceding comment, print it out.
  void printCommentIfNeeded(SourceLoc Loc);

  void convertNodes(const NodesToPrint &ToPrint);

  void convertNode(ASTNode Node, SourceLoc StartOverride = {},
                   bool ConvertCalls = true,
                   bool IncludePrecedingComment = true);

  void convertPattern(const Pattern *P);

  /// Check whether \p Node requires the remainder of this scope to be wrapped
  /// in a \c withChecked(Throwing)Continuation. If it is necessary, add
  /// a call to \c withChecked(Throwing)Continuation and modify the current
  /// scope (\c Scopes.back() ) so that it knows it's wrapped in a continuation.
  ///
  /// Wrapping a node in a continuation is necessary if the following conditions
  /// are satisfied:
  ///  - It contains a reference to the \c TopHandler's completion hander,
  ///    because these completion handler calls need to be promoted to \c return
  ///    statements in the refactored method, but
  ///  - We cannot hoist the completion handler of \p Node, because it doesn't
  ///    have an async alternative by our heuristics (e.g. because of a
  ///    completion handler name mismatch or because it also returns a value
  ///    synchronously).
  void wrapScopeInContinationIfNecessary(ASTNode Node);

  bool walkToPatternPre(Pattern *P) override;

  bool walkToDeclPre(Decl *D, CharSourceRange Range) override;

  bool walkToDeclPost(Decl *D) override;

  bool walkToExprPre(Expr *E) override;

  bool replaceRangeWithPlaceholder(SourceRange range);

  bool walkToExprPost(Expr *E) override;

  bool walkToStmtPre(Stmt *S) override;

  bool walkToStmtPost(Stmt *S) override;

  bool addCustom(SourceRange Range, llvm::function_ref<void()> Custom = {});

  /// Insert custom text at the given \p Loc that shouldn't replace any existing
  /// source code.
  bool insertCustom(SourceLoc Loc, llvm::function_ref<void()> Custom = {});

  void addRange(SourceLoc Start, SourceLoc End, bool ToEndOfToken = false);

  void addRange(SourceRange Range, bool ToEndOfToken = false);

  void addFuncDecl(const FuncDecl *FD);

  void addFallbackVars(ArrayRef<const ParamDecl *> FallbackParams,
                       const ClosureCallbackParams &AllParams);

  void addDo();

  /// Assuming that \p Result represents an error result to completion handler,
  /// returns \c true if the error has already been handled through a
  /// 'try await'.
  bool isErrorAlreadyHandled(HandlerResult Result);

  /// Returns \c true if the source representation of \p E can be interpreted
  /// as an expression returning an Optional value.
  bool isExpressionOptional(Expr *E);

  /// Converts a call \p CE to a completion handler. Depending on the call it
  /// will be interpreted as a call that's returning a success result, an error
  /// or, if the call is completely ambiguous, adds an if-let that checks if the
  /// error is \c nil at runtime and dispatches to the success or error case
  /// depending on it.
  /// \p AddConvertedHandlerCall needs to add the converted version of the
  /// completion handler. Depending on the given \c HandlerResult, it must be
  /// intepreted as a success or error call.
  /// \p AddConvertedErrorCall must add the converted equivalent of returning an
  /// error. The passed \c StringRef contains the name of a variable that is of
  /// type 'Error'.
  void convertHandlerCall(
      const CallExpr *CE,
      llvm::function_ref<void(HandlerResult)> AddConvertedHandlerCall,
      llvm::function_ref<void(StringRef)> AddConvertedErrorCall);

  /// Convert a call \p CE to a completion handler to its 'return' or 'throws'
  /// equivalent.
  void convertHandlerToReturnOrThrows(const CallExpr *CE);

  /// Convert the call \p CE to a completion handler to its 'return' or 'throws'
  /// equivalent, where \p Result determines whether the call should be
  /// interpreted as an error or success call.
  void convertHandlerToReturnOrThrowsImpl(const CallExpr *CE,
                                          HandlerResult Result);

  /// Convert a call \p CE to a completion handler to resumes of the
  /// continuation that's currently on top of the stack.
  void convertHandlerToContinuationResume(const CallExpr *CE);

  /// Convert a call \p CE to a completion handler to resumes of the
  /// continuation that's currently on top of the stack.
  /// \p Result determines whether the call should be interpreted as a success
  /// or error call.
  void convertHandlerToContinuationResumeImpl(const CallExpr *CE,
                                              HandlerResult Result);

  /// From the given expression \p E, which is an argument to a function call,
  /// extract the passed closure if there is one. Otherwise return \c nullptr.
  ClosureExpr *extractCallback(Expr *E);

  /// Callback arguments marked as e.g. `@convention(block)` produce arguments
  /// that are `FunctionConversionExpr`.
  /// We don't care about the conversions and want to shave them off.
  Expr *lookThroughFunctionConversionExpr(Expr *E);

  void addHoistedCallback(const CallExpr *CE,
                          const AsyncHandlerParamDesc &HandlerDesc);

  /// Add a binding to a known bool flag that indicates success or failure.
  void addBoolFlagParamBindingIfNeeded(std::optional<KnownBoolFlagParam> Flag,
                                       BlockKind Block);

  /// Add a call to the async alternative of \p CE and convert the \p Callback
  /// to be executed after the async call. \p HandlerDesc describes the
  /// completion handler in the function that's called by \p CE and \p ArgList
  /// are the arguments being passed in \p CE.
  void addHoistedClosureCallback(const CallExpr *CE,
                                 const AsyncHandlerParamDesc &HandlerDesc,
                                 const ClosureExpr *Callback);

  /// Add a call to the async alternative of \p FD. Afterwards, pass the results
  /// of the async call to the completion handler, named \p HandlerName and
  /// described by \p HandlerDesc.
  /// \p AddAwaitCall adds the call to the refactored async method to the output
  /// stream without storing the result to any variables.
  /// This is used when the user didn't use a closure for the callback, but
  /// passed in a variable or function name for the completion handler.
  void addHoistedNamedCallback(const FuncDecl *FD,
                               const AsyncHandlerDesc &HandlerDesc,
                               StringRef HandlerName,
                               std::function<void(void)> AddAwaitCall);

  /// Checks whether a binding pattern for a given decl can be printed inline in
  /// an await call, e.g 'let ((x, y), z) = await foo()', where '(x, y)' is the
  /// inline pattern.
  const Pattern *
  bindingPatternToPrintInline(const Decl *D, const ClassifiedBlock &Block,
                              const ClosureExpr *CallbackClosure);

  /// Retrieve a map of patterns to print inline for an array of param decls.
  InlinePatternsToPrint
  getInlinePatternsToPrint(const ClassifiedBlock &Block,
                           ArrayRef<const ParamDecl *> Params,
                           const ClosureExpr *CallbackClosure);

  /// Print any out of line binding patterns that could not be printed as inline
  /// patterns. These typically appear directly after an await call, e.g:
  /// \code
  /// let x = await foo()
  /// let (y, z) = x
  /// \endcode
  void
  printOutOfLineBindingPatterns(const ClassifiedBlock &Block,
                                const InlinePatternsToPrint &InlinePatterns);

  /// Prints an \c await call to an \c async function, binding any return values
  /// into variables.
  ///
  /// \param CE The call expr to convert.
  /// \param SuccessBlock The nodes present in the success block following the
  /// call.
  /// \param SuccessParams The success parameters, which will be printed as
  /// return values.
  /// \param InlinePatterns A map of patterns that can be printed inline for
  /// a given param.
  /// \param HandlerDesc A description of the completion handler.
  /// \param AddDeclarations Whether or not to add \c let or \c var keywords to
  /// the return value bindings.
  void addAwaitCall(const CallExpr *CE, const ClassifiedBlock &SuccessBlock,
                    ArrayRef<const ParamDecl *> SuccessParams,
                    const InlinePatternsToPrint &InlinePatterns,
                    const AsyncHandlerParamDesc &HandlerDesc,
                    bool AddDeclarations);

  void addFallbackCatch(const ClosureCallbackParams &Params);

  void addCatch(const ParamDecl *ErrParam);

  void preparePlaceholdersAndUnwraps(AsyncHandlerDesc HandlerDesc,
                                     const ClosureCallbackParams &Params,
                                     BlockKind Block);

  /// Add a mapping from each passed parameter to a new name, possibly
  /// synthesizing a new one if hoisting it would cause a redeclaration or
  /// shadowing. If there's no bound name and \c AddIfMissing is false, no
  /// name will be added.
  void prepareNames(const ClassifiedBlock &Block,
                    ArrayRef<const ParamDecl *> Params,
                    const InlinePatternsToPrint &InlinePatterns,
                    bool AddIfMissing = true);

  /// Returns a unique name using \c Name as base that doesn't clash with any
  /// other names in the current scope.
  Identifier createUniqueName(StringRef Name);

  /// Create a unique name for the variable declared by \p D that doesn't
  /// clash with any other names in scope, using \p BoundName as the base name
  /// if not empty and the name of \p D otherwise. Adds this name to both
  /// \c Names and the current scope's names (\c Scopes.Names).
  Identifier assignUniqueName(const Decl *D, StringRef BoundName);

  StringRef newNameFor(const Decl *D, bool Required = true);

  void addNewScope(const llvm::DenseSet<const Decl *> &Decls);

  void clearNames(ArrayRef<const ParamDecl *> Params);

  /// Adds a forwarding call to the old completion handler function, with
  /// \p HandlerReplacement that allows for a custom replacement or, if empty,
  /// removal of the completion handler closure.
  void addForwardingCallTo(const FuncDecl *FD, StringRef HandlerReplacement);

  /// Adds a forwarded error argument to a completion handler call. If the error
  /// type of \p HandlerDesc is more specialized than \c Error, an
  /// 'as! CustomError' cast to the more specialized error type will be added to
  /// the output stream.
  void addForwardedErrorArgument(StringRef ErrorName,
                                 const AsyncHandlerDesc &HandlerDesc);

  /// If \p T has a natural default value like \c nil for \c Optional or \c ()
  /// for \c Void, add that default value to the output. Otherwise, add a
  /// placeholder that contains \p T's name as the hint.
  void addDefaultValueOrPlaceholder(Type T);

  /// Adds the \c Index -th parameter to the completion handler described by \p
  /// HanderDesc.
  /// If \p ResultName is not empty, it is assumed that a variable with that
  /// name contains the result returned from the async alternative. If the
  /// callback also takes an error parameter, \c nil passed to the completion
  /// handler for the error. If \p ResultName is empty, it is a assumed that a
  /// variable named 'error' contains the error thrown from the async method and
  /// 'nil' will be passed to the completion handler for all result parameters.
  void addCompletionHandlerArgument(size_t Index, StringRef ResultName,
                                    const AsyncHandlerDesc &HandlerDesc);

  /// Add a call to the completion handler named \p HandlerName and described by
  /// \p HandlerDesc, passing all the required arguments. See \c
  /// getCompletionHandlerArgument for how the arguments are synthesized.
  void addCallToCompletionHandler(StringRef ResultName,
                                  const AsyncHandlerDesc &HandlerDesc,
                                  StringRef HandlerName);

  /// Adds the result type of a refactored async function that previously
  /// returned results via a completion handler described by \p HandlerDesc.
  void addAsyncFuncReturnType(const AsyncHandlerDesc &HandlerDesc);

  /// If \p FD is generic, adds a type annotation with the return type of the
  /// converted async function. This is used when creating a legacy function,
  /// calling the converted 'async' function so that the generic parameters of
  /// the legacy function are passed to the generic function. For example for
  /// \code
  /// func foo<GenericParam>() async -> GenericParam {}
  /// \endcode
  /// we generate
  /// \code
  /// func foo<GenericParam>(completion: (GenericParam) -> Void) {
  ///   Task {
  ///     let result: GenericParam = await foo()
  ///               <------------>
  ///     completion(result)
  ///   }
  /// }
  /// \endcode
  /// This function adds the range marked by \c <----->
  void addResultTypeAnnotationIfNecessary(const FuncDecl *FD,
                                          const AsyncHandlerDesc &HandlerDesc);
};

} // namespace asyncrefactorings
} // namespace refactoring
} // namespace swift

#endif
