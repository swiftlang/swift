//===--- DiagnoseStaticExclusivity.cpp - Find violations of exclusivity ---===//
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
// This file implements a diagnostic pass that finds violations of the
// "Law of Exclusivity" at compile time. The Law of Exclusivity requires
// that the access duration of any access to an address not overlap
// with an access to the same address unless both accesses are reads.
//
// This pass relies on 'begin_access' and 'end_access' SIL instruction
// markers inserted during SILGen to determine when an access to an address
// begins and ends. It models the in-progress accesses with a map from
// storage locations to the counts of read and write-like accesses in progress
// for that location.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "static-exclusivity"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Parse/Lexer.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/Projection.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Support/Debug.h"
using namespace swift;

template <typename... T, typename... U>
static InFlightDiagnostic diagnose(ASTContext &Context, SourceLoc loc,
                                   Diag<T...> diag, U &&... args) {
  return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

namespace {

enum class AccessedStorageKind : unsigned {
  /// The access is to a location represented by a SIL value
  /// (for example, an 'alloc_box' instruction for a local variable).
  /// Two accesses accessing the exact same SILValue are considered to be
  /// accessing the same storage location.
  Value,

  /// The access is to a global variable.
  GlobalVar,

  /// The access is to a stored class property.
  ClassProperty
};

/// Represents the identity of a stored class property as a combination
/// of a base and a single projection. Eventually the goal is to make this
/// more precise and consider, casts, etc.
class ObjectProjection {
public:
  ObjectProjection(SILValue Object, const Projection &Proj)
      : Object(Object), Proj(Proj) {
    assert(Object->getType().isObject());
  }

  SILValue getObject() const { return Object; }
  const Projection &getProjection() const { return Proj; }

  bool operator==(const ObjectProjection &Other) const {
    return Object == Other.Object && Proj == Other.Proj;
  }

  bool operator!=(const ObjectProjection &Other) const {
    return Object != Other.Object || Proj != Other.Proj;
  }

private:
  SILValue Object;
  Projection Proj;
};

/// Represents the identity of a storage location being accessed.
/// This is used to determine when two 'begin_access' instructions
/// definitely access the same underlying location.
///
/// The key invariant that this class must maintain is that if it says
/// two storage locations are the same then they must be the same at run time.
/// It is allowed to err on the other side: it may imprecisely fail to
/// recognize that two storage locations that represent the same run-time
/// location are in fact the same.
class AccessedStorage {

private:
  AccessedStorageKind Kind;

  union {
    SILValue Value;
    SILGlobalVariable *Global;
    ObjectProjection ObjProj;
  };

public:
  AccessedStorage(SILValue V)
      : Kind(AccessedStorageKind::Value), Value(V) { }

  AccessedStorage(SILGlobalVariable *Global)
      : Kind(AccessedStorageKind::GlobalVar), Global(Global) {}

  AccessedStorage(AccessedStorageKind Kind,
                  const ObjectProjection &ObjProj)
      : Kind(Kind), ObjProj(ObjProj) {}

  AccessedStorageKind getKind() const { return Kind; }

  SILValue getValue() const {
    assert(Kind == AccessedStorageKind::Value);
    return Value;
  }

  SILGlobalVariable *getGlobal() const {
    assert(Kind == AccessedStorageKind::GlobalVar);
    return Global;
  }

  const ObjectProjection &getObjectProjection() const {
    assert(Kind == AccessedStorageKind::ClassProperty);
    return ObjProj;
  }

  /// Returns the ValueDecl for the underlying storage, if it can be
  /// determined. Otherwise returns null. For diagnostic purposes.
  const ValueDecl *getStorageDecl() const {
    switch(Kind) {
    case AccessedStorageKind::GlobalVar:
      return getGlobal()->getDecl();
    case AccessedStorageKind::Value:
      if (auto *Box = dyn_cast<AllocBoxInst>(getValue())) {
        return Box->getLoc().getAsASTNode<VarDecl>();
      }
      if (auto *Arg = dyn_cast<SILFunctionArgument>(getValue())) {
        return Arg->getDecl();
      }
      break;
    case AccessedStorageKind::ClassProperty: {
      const ObjectProjection &OP = getObjectProjection();
      const Projection &P = OP.getProjection();
      return P.getVarDecl(OP.getObject()->getType());
    }
    }
    return nullptr;
  }
};

/// Records an access to an address and the single subpath of projections
/// that was performed on the address, if such a single subpath exists.
class RecordedAccess {
private:
  BeginAccessInst *Inst;
  ProjectionPath SubPath;

public:
  RecordedAccess(BeginAccessInst *BAI, const ProjectionPath &SubPath)
      : Inst(BAI), SubPath(SubPath) {}

  BeginAccessInst *getInstruction() const { return Inst; }

  SILAccessKind getAccessKind() const { return Inst->getAccessKind(); }

  SILLocation getAccessLoc() const { return Inst->getLoc(); }

  const ProjectionPath &getSubPath() const { return SubPath; }
};

/// Records the in-progress accesses to a given sub path.
class SubAccessInfo {
public:
  SubAccessInfo(const ProjectionPath &P) : Path(P) {}

  ProjectionPath Path;

  /// The number of in-progress 'read' accesses (that is 'begin_access [read]'
  /// instructions that have not yet had the corresponding 'end_access').
  unsigned Reads = 0;

  /// The number of in-progress write-like accesses.
  unsigned NonReads = 0;

  /// The instruction that began the first in-progress access to the storage
  /// location. Used for diagnostic purposes.
  Optional<RecordedAccess> FirstAccess = None;

public:
  /// Increment the count for given access.
  void beginAccess(BeginAccessInst *BAI, const ProjectionPath &SubPath) {
    if (!FirstAccess) {
      assert(Reads == 0 && NonReads == 0);
      FirstAccess = RecordedAccess(BAI, SubPath);
    }

    if (BAI->getAccessKind() == SILAccessKind::Read)
      Reads++;
    else
      NonReads++;
  }

  /// Decrement the count for given access.
  void endAccess(EndAccessInst *EAI) {
    if (EAI->getBeginAccess()->getAccessKind() == SILAccessKind::Read)
      Reads--;
    else
      NonReads--;

    // If all open accesses are now ended, forget the location of the
    // first access.
    if (Reads == 0 && NonReads == 0)
      FirstAccess = None;
  }

  /// Returns true when there are any accesses to this location in progress.
  bool hasAccessesInProgress() const { return Reads > 0 || NonReads > 0; }

  /// Returns true when there must have already been a conflict diagnosed
  /// for an in-progress access. Used to suppress multiple diagnostics for
  /// the same underlying access violation.
  bool alreadyHadConflict() const {
    return (NonReads > 0 && Reads > 0) || (NonReads > 1);
  }

  // Returns true when beginning an access of the given Kind can
  // result in a conflict with a previous access.
  bool canConflictWithAccessOfKind(SILAccessKind Kind) const {
    if (Kind == SILAccessKind::Read) {
      // A read conflicts with any non-read accesses.
      return NonReads > 0;
    }

    // A non-read access conflicts with any other access.
    return NonReads > 0 || Reads > 0;
  }

  bool conflictsWithAccess(SILAccessKind Kind,
                           const ProjectionPath &SubPath) const {
    if (!canConflictWithAccessOfKind(Kind))
      return false;

    SubSeqRelation_t Relation = Path.computeSubSeqRelation(SubPath);
    // If the one path is a subsequence of the other (or they are the same)
    // then access the same storage.
    return (Relation != SubSeqRelation_t::Unknown);
  }
};

/// Models the in-progress accesses for an address on which access has begun
/// with a begin_access instruction. For a given address, tracks the
/// count and kinds of accesses as well as the subpaths (i.e., projections) that
/// were accessed.
class AccessInfo {
  using SubAccessVector = SmallVector<SubAccessInfo, 4>;

  SubAccessVector SubAccesses;

  /// Returns the SubAccess info for accessing at the given SubPath.
  SubAccessInfo &findOrCreateSubAccessInfo(const ProjectionPath &SubPath) {
    for (auto &Info : SubAccesses) {
      if (Info.Path == SubPath)
        return Info;
    }

    SubAccesses.emplace_back(SubPath);
    return SubAccesses.back();
  }

  SubAccessVector::const_iterator
  findFirstSubPathWithConflict(SILAccessKind OtherKind,
                               const ProjectionPath &OtherSubPath) const {
    // Note this iteration requires deterministic ordering for repeatable
    // diagnostics.
    for (auto I = SubAccesses.begin(), E = SubAccesses.end(); I != E; ++I) {
      const SubAccessInfo &Access = *I;
      if (Access.conflictsWithAccess(OtherKind, OtherSubPath))
        return I;
    }

    return SubAccesses.end();
  }

public:
  // Returns the previous access when beginning an access of the given Kind will
  // result in a conflict with a previous access.
  Optional<RecordedAccess>
  conflictsWithAccess(SILAccessKind Kind, const ProjectionPath &SubPath) const {
    auto I = findFirstSubPathWithConflict(Kind, SubPath);
    if (I == SubAccesses.end())
      return None;

    return I->FirstAccess;
  }

  /// Returns true if any subpath of has already had a conflict.
  bool alreadyHadConflict() const {
    for (const auto &SubAccess : SubAccesses) {
      if (SubAccess.alreadyHadConflict())
        return true;
    }
    return false;
  }

  /// Returns true when there are any accesses to this location in progress.
  bool hasAccessesInProgress() const {
    for (const auto &SubAccess : SubAccesses) {
      if (SubAccess.hasAccessesInProgress())
        return true;
    }
    return false;
  }

  /// Increment the count for given access.
  void beginAccess(BeginAccessInst *BAI, const ProjectionPath &SubPath) {
    SubAccessInfo &SubAccess = findOrCreateSubAccessInfo(SubPath);
    SubAccess.beginAccess(BAI, SubPath);
  }

  /// Decrement the count for given access.
  void endAccess(EndAccessInst *EAI, const ProjectionPath &SubPath) {
    SubAccessInfo &SubAccess = findOrCreateSubAccessInfo(SubPath);
    SubAccess.endAccess(EAI);
  }
};

/// Indicates whether a 'begin_access' requires exclusive access
/// or allows shared access. This needs to be kept in sync with
/// diag::exclusivity_access_required, exclusivity_access_required_swift3,
/// and diag::exclusivity_conflicting_access.
enum class ExclusiveOrShared_t : unsigned {
  ExclusiveAccess = 0,
  SharedAccess = 1
};


/// Tracks the in-progress accesses on per-storage-location basis.
using StorageMap = llvm::SmallDenseMap<AccessedStorage, AccessInfo, 4>;

/// Represents two accesses that conflict and their underlying storage.
struct ConflictingAccess {
public:
  /// Create a conflict for two begin_access instructions in the same function.
  ConflictingAccess(const AccessedStorage &Storage, const RecordedAccess &First,
                    const RecordedAccess &Second)
      : Storage(Storage), FirstAccess(First), SecondAccess(Second) {}

  const AccessedStorage Storage;
  const RecordedAccess FirstAccess;
  const RecordedAccess SecondAccess;
};

} // end anonymous namespace

namespace llvm {
/// Enable using AccessedStorage as a key in DenseMap.
template <> struct DenseMapInfo<AccessedStorage> {
  static AccessedStorage getEmptyKey() {
    return AccessedStorage(swift::SILValue::getFromOpaqueValue(
        llvm::DenseMapInfo<void *>::getEmptyKey()));
  }

  static AccessedStorage getTombstoneKey() {
    return AccessedStorage(swift::SILValue::getFromOpaqueValue(
        llvm::DenseMapInfo<void *>::getTombstoneKey()));
  }

  static unsigned getHashValue(AccessedStorage Storage) {
    switch (Storage.getKind()) {
    case AccessedStorageKind::Value:
      return DenseMapInfo<swift::SILValue>::getHashValue(Storage.getValue());
    case AccessedStorageKind::GlobalVar:
      return DenseMapInfo<void *>::getHashValue(Storage.getGlobal());
    case AccessedStorageKind::ClassProperty: {
      const ObjectProjection &P = Storage.getObjectProjection();
      return llvm::hash_combine(P.getObject(), P.getProjection());
    }
    }
    llvm_unreachable("Unhandled AccessedStorageKind");
  }

  static bool isEqual(AccessedStorage LHS, AccessedStorage RHS) {
    if (LHS.getKind() != RHS.getKind())
      return false;

    switch (LHS.getKind()) {
    case AccessedStorageKind::Value:
      return LHS.getValue() == RHS.getValue();
    case AccessedStorageKind::GlobalVar:
      return LHS.getGlobal() == RHS.getGlobal();
    case AccessedStorageKind::ClassProperty:
        return LHS.getObjectProjection() == RHS.getObjectProjection();
    }
    llvm_unreachable("Unhandled AccessedStorageKind");
  }
};

} // end namespace llvm

/// Returns whether an access of the given kind requires exclusive or shared
/// access to its storage.
static ExclusiveOrShared_t getRequiredAccess(SILAccessKind Kind) {
  if (Kind == SILAccessKind::Read)
    return ExclusiveOrShared_t::SharedAccess;

  return ExclusiveOrShared_t::ExclusiveAccess;
}

/// Extract the text for the given expression.
static StringRef extractExprText(const Expr *E, SourceManager &SM) {
  const auto CSR = Lexer::getCharSourceRangeFromSourceRange(SM,
      E->getSourceRange());
  return SM.extractText(CSR);
}

/// Returns true when the call expression is a call to swap() in the Standard
/// Library.
/// This is a helper function that is only used in an assertion, which is why
/// it is in the ifndef.
#ifndef NDEBUG
static bool isCallToStandardLibrarySwap(CallExpr *CE, ASTContext &Ctx) {
  if (CE->getCalledValue() == Ctx.getSwap(nullptr))
    return true;

  // Is the call module qualified, i.e. Swift.swap(&a[i], &[j)?
  if (auto *DSBIE = dyn_cast<DotSyntaxBaseIgnoredExpr>(CE->getFn())) {
    if (auto *DRE = dyn_cast<DeclRefExpr>(DSBIE->getRHS())) {
      return DRE->getDecl() == Ctx.getSwap(nullptr);
    }
  }

  return false;
}
#endif

/// Do a sytactic pattern match to try to safely suggest a Fix-It to rewrite
/// calls like swap(&collection[index1], &collection[index2]) to
///
/// This method takes an array of all the ApplyInsts for calls to swap()
/// in the function to avoid need to construct a parent map over the AST
/// to find the CallExpr for the inout accesses.
static void
tryFixItWithCallToCollectionSwapAt(const BeginAccessInst *Access1,
                                  const BeginAccessInst *Access2,
                                  ArrayRef<ApplyInst *> CallsToSwap,
                                  ASTContext &Ctx,
                                  InFlightDiagnostic &Diag) {
  if (CallsToSwap.empty())
    return;

  // Inout arguments must be modifications.
  if (Access1->getAccessKind() != SILAccessKind::Modify ||
      Access2->getAccessKind() != SILAccessKind::Modify) {
    return;
  }

  SILLocation Loc1 = Access1->getLoc();
  SILLocation Loc2 = Access2->getLoc();
  if (Loc1.isNull() || Loc2.isNull())
    return;

  auto *InOut1 = Loc1.getAsASTNode<InOutExpr>();
  auto *InOut2 = Loc2.getAsASTNode<InOutExpr>();
  if (!InOut1 || !InOut2)
    return;

  CallExpr *FoundCall = nullptr;
  // Look through all the calls to swap() recorded in the function to find
  // which one we're diagnosing.
  for (ApplyInst *AI : CallsToSwap) {
    SILLocation CallLoc = AI->getLoc();
    if (CallLoc.isNull())
      continue;

    auto *CE = CallLoc.getAsASTNode<CallExpr>();
    if (!CE)
      continue;

    assert(isCallToStandardLibrarySwap(CE, Ctx));
    // swap() takes two arguments.
    auto *ArgTuple = cast<TupleExpr>(CE->getArg());
    const Expr *Arg1 = ArgTuple->getElement(0);
    const Expr *Arg2 = ArgTuple->getElement(1);
    if ((Arg1 == InOut1 && Arg2 == InOut2)) {
        FoundCall = CE;
      break;
    }
  }
  if (!FoundCall)
    return;

  // We found a call to swap(&e1, &e2). Now check to see whether it
  // matches the form swap(&someCollection[index1], &someCollection[index2]).
  auto *SE1 = dyn_cast<SubscriptExpr>(InOut1->getSubExpr());
  if (!SE1)
    return;
  auto *SE2 = dyn_cast<SubscriptExpr>(InOut2->getSubExpr());
  if (!SE2)
    return;

  // Do the two subscripts refer to the same subscript declaration?
  auto *Decl1 = cast<SubscriptDecl>(SE1->getDecl().getDecl());
  auto *Decl2 = cast<SubscriptDecl>(SE2->getDecl().getDecl());
  if (Decl1 != Decl2)
    return;

  ProtocolDecl *MutableCollectionDecl = Ctx.getMutableCollectionDecl();

  // Is the subcript either (1) on MutableCollection itself or (2) a
  // a witness for a subscript on MutableCollection?
  bool IsSubscriptOnMutableCollection = false;
  ProtocolDecl *ProtocolForDecl =
      Decl1->getDeclContext()->getAsProtocolOrProtocolExtensionContext();
  if (ProtocolForDecl) {
    IsSubscriptOnMutableCollection = (ProtocolForDecl == MutableCollectionDecl);
  } else {
    for (ValueDecl *Req : Decl1->getSatisfiedProtocolRequirements()) {
      DeclContext *ReqDC = Req->getDeclContext();
      ProtocolDecl *ReqProto = ReqDC->getAsProtocolOrProtocolExtensionContext();
      assert(ReqProto && "Protocol requirement not in a protocol?");

      if (ReqProto == MutableCollectionDecl) {
        IsSubscriptOnMutableCollection = true;
        break;
      }
    }
  }

  if (!IsSubscriptOnMutableCollection)
    return;

  // We're swapping two subscripts on mutable collections -- but are they
  // the same collection? Approximate this by checking for textual
  // equality on the base expressions. This is just an approximation,
  // but is fine for a best-effort Fix-It.
  SourceManager &SM = Ctx.SourceMgr;
  StringRef Base1Text = extractExprText(SE1->getBase(), SM);
  StringRef Base2Text = extractExprText(SE2->getBase(), SM);

  if (Base1Text != Base2Text)
    return;

  auto *Index1 = dyn_cast<ParenExpr>(SE1->getIndex());
  if (!Index1)
    return;

  auto *Index2 = dyn_cast<ParenExpr>(SE2->getIndex());
  if (!Index2)
    return;

  StringRef Index1Text = extractExprText(Index1->getSubExpr(), SM);
  StringRef Index2Text = extractExprText(Index2->getSubExpr(), SM);

  // Suggest replacing with call with a call to swapAt().
  SmallString<64> FixItText;
  {
    llvm::raw_svector_ostream Out(FixItText);
    Out << Base1Text << ".swapAt(" << Index1Text << ", " << Index2Text << ")";
  }

  Diag.fixItReplace(FoundCall->getSourceRange(), FixItText);
}

/// Returns a string representation of the BaseName and the SubPath
/// suitable for use in diagnostic text. Only supports the Projections
/// that stored-property relaxation supports: struct stored properties
/// and tuple elements.
static std::string getPathDescription(DeclName BaseName, SILType BaseType,
                                      ProjectionPath SubPath, SILModule &M) {
  std::string sbuf;
  llvm::raw_string_ostream os(sbuf);

  os << "'" << BaseName;

  SILType ContainingType = BaseType;
  for (auto &P : SubPath) {
    os << ".";
    switch (P.getKind()) {
    case ProjectionKind::Struct:
      os << P.getVarDecl(ContainingType)->getBaseName();
      break;
    case ProjectionKind::Tuple: {
      // Use the tuple element's name if present, otherwise use its index.
      Type SwiftTy = ContainingType.getSwiftRValueType();
      TupleType *TupleTy = SwiftTy->getAs<TupleType>();
      assert(TupleTy && "Tuple projection on non-tuple type!?");

      Identifier ElementName = TupleTy->getElement(P.getIndex()).getName();
      if (ElementName.empty())
        os << P.getIndex();
      else
        os << ElementName;
      break;
    }
    default:
      llvm_unreachable("Unexpected projection kind in SubPath!");
    }
    ContainingType = P.getType(ContainingType, M);
  }

  os << "'";

  return os.str();
}

/// Emits a diagnostic if beginning an access with the given in-progress
/// accesses violates the law of exclusivity. Returns true when a
/// diagnostic was emitted.
static void diagnoseExclusivityViolation(const ConflictingAccess &Violation,
                                         ArrayRef<ApplyInst *> CallsToSwap,
                                         ASTContext &Ctx) {

  const AccessedStorage &Storage = Violation.Storage;
  const RecordedAccess &FirstAccess = Violation.FirstAccess;
  const RecordedAccess &SecondAccess = Violation.SecondAccess;

  DEBUG(llvm::dbgs() << "Conflict on " << *FirstAccess.getInstruction()
                     << "\n  vs " << *SecondAccess.getInstruction()
                     << "\n  in function "
                     << *FirstAccess.getInstruction()->getFunction());

  // Can't have a conflict if both accesses are reads.
  assert(!(FirstAccess.getAccessKind() == SILAccessKind::Read &&
           SecondAccess.getAccessKind() == SILAccessKind::Read));

  ExclusiveOrShared_t FirstRequires =
      getRequiredAccess(FirstAccess.getAccessKind());

  // Diagnose on the first access that requires exclusivity.
  bool FirstIsMain = (FirstRequires == ExclusiveOrShared_t::ExclusiveAccess);
  const RecordedAccess &MainAccess = (FirstIsMain ? FirstAccess : SecondAccess);
  const RecordedAccess &NoteAccess = (FirstIsMain ? SecondAccess : FirstAccess);

  SourceRange RangeForMain = MainAccess.getAccessLoc().getSourceRange();
  unsigned AccessKindForMain =
      static_cast<unsigned>(MainAccess.getAccessKind());

  if (const ValueDecl *VD = Storage.getStorageDecl()) {
    // We have a declaration, so mention the identifier in the diagnostic.
    auto DiagnosticID = (Ctx.LangOpts.isSwiftVersion3() ?
                         diag::exclusivity_access_required_swift3 :
                         diag::exclusivity_access_required);
    SILType BaseType = FirstAccess.getInstruction()->getType().getAddressType();
    SILModule &M = FirstAccess.getInstruction()->getModule();
    std::string PathDescription = getPathDescription(
        VD->getBaseName(), BaseType, MainAccess.getSubPath(), M);
    auto D =
        diagnose(Ctx, MainAccess.getAccessLoc().getSourceLoc(), DiagnosticID,
                 PathDescription, AccessKindForMain);
    D.highlight(RangeForMain);
    tryFixItWithCallToCollectionSwapAt(FirstAccess.getInstruction(),
                                       SecondAccess.getInstruction(),
                                       CallsToSwap, Ctx, D);
  } else {
    auto DiagnosticID = (Ctx.LangOpts.isSwiftVersion3() ?
                         diag::exclusivity_access_required_unknown_decl_swift3 :
                         diag::exclusivity_access_required_unknown_decl);
    diagnose(Ctx, MainAccess.getAccessLoc().getSourceLoc(), DiagnosticID,
             AccessKindForMain)
        .highlight(RangeForMain);
  }
  diagnose(Ctx, NoteAccess.getAccessLoc().getSourceLoc(),
           diag::exclusivity_conflicting_access)
      .highlight(NoteAccess.getAccessLoc().getSourceRange());
}

/// Make a best effort to find the underlying object for the purpose
/// of identifying the base of a 'ref_element_addr'.
static SILValue findUnderlyingObject(SILValue Value) {
  assert(Value->getType().isObject());
  SILValue Iter = Value;

  while (true) {
    // For now just look through begin_borrow instructions; we can likely
    // make this more precise in the future.
    if (auto *BBI = dyn_cast<BeginBorrowInst>(Iter)) {
      Iter = BBI->getOperand();
      continue;
    }
    break;
  }

  assert(Iter->getType().isObject());
  return Iter;
}

/// Look through a value to find the underlying storage accessed.
static AccessedStorage findAccessedStorage(SILValue Source) {
  SILValue Iter = Source;
  while (true) {
    // Base case for globals: make sure ultimate source is recognized.
    if (auto *GAI = dyn_cast<GlobalAddrInst>(Iter)) {
      return AccessedStorage(GAI->getReferencedGlobal());
    }

    // Base case for class objects.
    if (auto *REA = dyn_cast<RefElementAddrInst>(Iter)) {
      // Do a best-effort to find the identity of the object being projected
      // from. It is OK to be unsound here (i.e. miss when two ref_element_addrs
      // actually refer the same address) because these will be dynamically
      // checked.
      SILValue Object = findUnderlyingObject(REA->getOperand());
      const ObjectProjection &OP = ObjectProjection(Object,
                                                    Projection(REA));
      return AccessedStorage(AccessedStorageKind::ClassProperty, OP);
    }

    switch (Iter->getKind()) {
    // Inductive cases: look through operand to find ultimate source.
    case ValueKind::ProjectBoxInst:
    case ValueKind::CopyValueInst:
    case ValueKind::MarkUninitializedInst:
    case ValueKind::UncheckedAddrCastInst:
    // Inlined access to subobjects.
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
    case ValueKind::UncheckedTakeEnumDataAddrInst:
    case ValueKind::RefTailAddrInst:
    case ValueKind::TailAddrInst:
    case ValueKind::IndexAddrInst:
      Iter = cast<SILInstruction>(Iter)->getOperand(0);
      continue;

    // Base address producers.
    case ValueKind::AllocBoxInst:
      // An AllocBox is a fully identified memory location.
    case ValueKind::AllocStackInst:
      // An AllocStack is a fully identified memory location, which may occur
      // after inlining code already subjected to stack promotion.
    case ValueKind::BeginAccessInst:
      // The current access is nested within another access.
      // View the outer access as a separate location because nested accesses do
      // not conflict with each other.
    case ValueKind::SILFunctionArgument:
      // A function argument is effectively a nested access, enforced
      // independently in the caller and callee.
    case ValueKind::PointerToAddressInst:
      // An addressor provides access to a global or class property via a
      // RawPointer. Calling the addressor casts that raw pointer to an address.
      return AccessedStorage(Iter);

    // Unsupported address producers.
    // Initialization is always local.
    case ValueKind::InitEnumDataAddrInst:
    case ValueKind::InitExistentialAddrInst:
    // Accessing an existential value requires a cast.
    case ValueKind::OpenExistentialAddrInst:
    default:
      DEBUG(llvm::dbgs() << "Bad memory access source: " << Iter);
      llvm_unreachable("Unexpected access source.");
    }
  }
}

/// Returns true when the apply calls the Standard Library swap().
/// Used for fix-its to suggest replacing with Collection.swapAt()
/// on exclusivity violations.
static bool isCallToStandardLibrarySwap(ApplyInst *AI, ASTContext &Ctx) {
  SILFunction *SF = AI->getReferencedFunction();
  if (!SF)
    return false;

  if (!SF->hasLocation())
    return false;

  auto *FD = SF->getLocation().getAsASTNode<FuncDecl>();
  if (!FD)
    return false;

  return FD == Ctx.getSwap(nullptr);
}

static SILInstruction *getSingleAddressProjectionUser(SILInstruction *I) {
  SILInstruction *SingleUser = nullptr;

  for (Operand *Use : I->getUses()) {
    SILInstruction *User = Use->getUser();
    if (isa<BeginAccessInst>(I) && isa<EndAccessInst>(User))
      continue;

    // We have more than a single user so bail.
    if (SingleUser)
      return nullptr;

    switch (User->getKind()) {
    case ValueKind::StructElementAddrInst:
    case ValueKind::TupleElementAddrInst:
      SingleUser = User;
      break;
    default:
      return nullptr;
    }
  }

  return SingleUser;
}

static ProjectionPath findSubPathAccessed(BeginAccessInst *BAI) {
  ProjectionPath SubPath(BAI->getType(), BAI->getType());

  SILInstruction *Iter = BAI;

  while ((Iter = getSingleAddressProjectionUser(Iter))) {
    SubPath.push_back(Projection(Iter));
  }

  return SubPath;
}

/// If making an access of the given kind at the given subpath would
/// would conflict, returns the first recorded access it would conflict
/// with. Otherwise, returns None.
Optional<RecordedAccess> shouldReportAccess(const AccessInfo &Info,
                                            swift::SILAccessKind Kind,
                                            const ProjectionPath &SubPath) {
  if (Info.alreadyHadConflict())
    return None;

  return Info.conflictsWithAccess(Kind, SubPath);
}

static void checkStaticExclusivity(SILFunction &Fn, PostOrderFunctionInfo *PO) {
  // The implementation relies on the following SIL invariants:
  //    - All incoming edges to a block must have the same in-progress
  //      accesses. This enables the analysis to not perform a data flow merge
  //      on incoming edges.
  //    - Further, for a given address each of the in-progress
  //      accesses must have begun in the same order on all edges. This ensures
  //      consistent diagnostics across changes to the exploration of the CFG.
  //    - On return from a function there are no in-progress accesses. This
  //      enables a sanity check for lean analysis state at function exit.
  //    - Each end_access instruction corresponds to exactly one begin access
  //      instruction. (This is encoded in the EndAccessInst itself)
  //    - begin_access arguments cannot be basic block arguments.
  //      This enables the analysis to look back to find the *single* storage
  //      storage location accessed.

  if (Fn.empty())
    return;

  // Collects calls the Standard Library swap() for Fix-Its.
  llvm::SmallVector<ApplyInst *, 8> CallsToSwap;

  // Stores the accesses that have been found to conflict. Used to defer
  // emitting diagnostics until we can determine whether they should
  // be suppressed.
  llvm::SmallVector<ConflictingAccess, 4> ConflictingAccesses;

  // For each basic block, track the stack of current accesses on
  // exit from that block.
  llvm::SmallDenseMap<SILBasicBlock *, Optional<StorageMap>, 32>
      BlockOutAccesses;

  BlockOutAccesses[Fn.getEntryBlock()] = StorageMap();

  for (auto *BB : PO->getReversePostOrder()) {
    Optional<StorageMap> &BBState = BlockOutAccesses[BB];

    // Because we use a reverse post-order traversal, unless this is the entry
    // at least one of its predecessors must have been reached. Use the out
    // state for that predecessor as our in state. The SIL verifier guarantees
    // that all incoming edges must have the same current accesses.
    for (auto *Pred : BB->getPredecessorBlocks()) {
      auto it = BlockOutAccesses.find(Pred);
      if (it == BlockOutAccesses.end())
        continue;

      const Optional<StorageMap> &PredAccesses = it->getSecond();
      if (PredAccesses) {
        BBState = PredAccesses;
        break;
      }
    }

    // The in-progress accesses for the current program point, represented
    // as map from storage locations to the accesses in progress for the
    // location.
    StorageMap &Accesses = *BBState;

    for (auto &I : *BB) {
      // Apply transfer functions. Beginning an access
      // increments the read or write count for the storage location;
      // Ending onr decrements the count.
      if (auto *BAI = dyn_cast<BeginAccessInst>(&I)) {
        SILAccessKind Kind = BAI->getAccessKind();
        const AccessedStorage &Storage = findAccessedStorage(BAI->getSource());
        AccessInfo &Info = Accesses[Storage];
        ProjectionPath SubPath = findSubPathAccessed(BAI);
        if (auto Conflict = shouldReportAccess(Info, Kind, SubPath)) {
          ConflictingAccesses.emplace_back(Storage, *Conflict,
                                           RecordedAccess(BAI, SubPath));
        }

        Info.beginAccess(BAI, SubPath);
        continue;
      }

      if (auto *EAI = dyn_cast<EndAccessInst>(&I)) {
        auto It = Accesses.find(findAccessedStorage(EAI->getSource()));
        AccessInfo &Info = It->getSecond();

        BeginAccessInst *BAI = EAI->getBeginAccess();
        const ProjectionPath &SubPath = findSubPathAccessed(BAI);
        Info.endAccess(EAI, SubPath);

        // If the storage location has no more in-progress accesses, remove
        // it to keep the StorageMap lean.
        if (!Info.hasAccessesInProgress())
          Accesses.erase(It);
        continue;
      }

      if (auto *AI = dyn_cast<ApplyInst>(&I)) {
        // Record calls to swap() for potential Fix-Its.
        if (isCallToStandardLibrarySwap(AI, Fn.getASTContext()))
          CallsToSwap.push_back(AI);
      }

      // Sanity check to make sure entries are properly removed.
      assert((!isa<ReturnInst>(&I) || Accesses.size() == 0) &&
             "Entries were not properly removed?!");
    }
  }

  // Now that we've collected violations and suppressed calls, emit
  // diagnostics.
  for (auto &Violation : ConflictingAccesses) {
    diagnoseExclusivityViolation(Violation, CallsToSwap, Fn.getASTContext());
  }
}

namespace {

class DiagnoseStaticExclusivity : public SILFunctionTransform {
public:
  DiagnoseStaticExclusivity() {}

private:
  void run() override {
    SILFunction *Fn = getFunction();
    // This is a staging flag. Eventually the ability to turn off static
    // enforcement will be removed.
    if (!Fn->getModule().getOptions().EnforceExclusivityStatic)
      return;

    PostOrderFunctionInfo *PO = getAnalysis<PostOrderAnalysis>()->get(Fn);
    checkStaticExclusivity(*Fn, PO);
  }
};

} // end anonymous namespace

SILTransform *swift::createDiagnoseStaticExclusivity() {
  return new DiagnoseStaticExclusivity();
}
