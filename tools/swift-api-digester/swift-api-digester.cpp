//===--- swift-api-digester.cpp - API change detector ---------------------===//
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

// swift-api-digester is a test utility to detect source-breaking API changes
// during the evolution of a Swift library. The tool works on two phases:
// (1) dumping library contents as a JSON file, and (2) comparing two JSON
// files textually to report interesting changes.
//
// During phase (1), the api-digester looks up every declarations inside
// a module and outputs a singly-rooted tree that encloses interesting
// details of the API level.
//
// During phase (2), api-digester applies structure-information comparison
// algorithms on two given singly root trees, trying to figure out, as
// precise as possible, the branches/leaves in the trees that differ from
// each other. Further analysis decides whether the changed leaves/branches
// can be reflected as source-breaking changes for API users. If they are,
// the output of api-digester will include such changes.

#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/AST/DiagnosticsModuleDiffer.h"
#include "swift/IDE/APIDigesterData.h"
#include <functional>
#include "ModuleAnalyzerNodes.h"
#include "ModuleDiagsConsumer.h"

using namespace swift;
using namespace ide;
using namespace api;

namespace  {
  enum class ActionType {
    None,
    DumpSDK,
    MigratorGen,
    DiagnoseSDKs,
    // The following two are for testing purposes
    DeserializeDiffItems,
    DeserializeSDK,
    GenerateNameCorrectionTemplate,
    FindUsr,
    GenerateEmptyBaseline,
  };
} // end anonymous namespace

namespace options {

static llvm::cl::OptionCategory Category("swift-api-digester Options");

static llvm::cl::opt<bool>
IncludeAllModules("include-all",
                  llvm::cl::desc("Include all modules from the SDK"),
                  llvm::cl::cat(Category));

static llvm::cl::list<std::string>
ModuleNames("module", llvm::cl::ZeroOrMore, llvm::cl::desc("Names of modules"),
            llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
ModuleList("module-list-file",
           llvm::cl::desc("File containing a new-line separated list of modules"),
           llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
ProtReqWhiteList("protocol-requirement-white-list",
           llvm::cl::desc("File containing a new-line separated list of protocol names"),
           llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
OutputFile("o", llvm::cl::desc("Output file"),
           llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
SDK("sdk", llvm::cl::desc("path to the SDK to build against"),
    llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
BaselineSDK("bsdk", llvm::cl::desc("path to the baseline SDK to import frameworks"),
    llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
Triple("target", llvm::cl::desc("target triple"),
       llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
ModuleCachePath("module-cache-path", llvm::cl::desc("Clang module cache path"),
                llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
ResourceDir("resource-dir",
            llvm::cl::desc("The directory that holds the compiler resource files"),
            llvm::cl::cat(Category));

static llvm::cl::list<std::string>
FrameworkPaths("F", llvm::cl::desc("add a directory to the framework search path"),
               llvm::cl::cat(Category));

static llvm::cl::list<std::string>
BaselineFrameworkPaths("BF", llvm::cl::desc("add a directory to the baseline framework search path"),
                       llvm::cl::cat(Category));

static llvm::cl::list<std::string>
BaselineModuleInputPaths("BI", llvm::cl::desc("add a module for baseline input"),
                         llvm::cl::cat(Category));

static llvm::cl::list<std::string>
ModuleInputPaths("I", llvm::cl::desc("add a module for input"),
                 llvm::cl::cat(Category));

static llvm::cl::list<std::string>
CCSystemFrameworkPaths("iframework",
  llvm::cl::desc("add a directory to the clang importer system framework search path"),
  llvm::cl::cat(Category));

static llvm::cl::opt<bool>
AbortOnModuleLoadFailure("abort-on-module-fail",
                        llvm::cl::desc("Abort if a module failed to load"),
                        llvm::cl::cat(Category));

static llvm::cl::opt<bool>
Verbose("v", llvm::cl::desc("Verbose"),
        llvm::cl::cat(Category));

static llvm::cl::opt<bool>
DebugMapping("debug-mapping", llvm::cl::desc("Dumping information for debug purposes"),
             llvm::cl::cat(Category));

static llvm::cl::opt<bool>
Abi("abi", llvm::cl::desc("Dumping ABI interface"),  llvm::cl::init(false),
    llvm::cl::cat(Category));

static llvm::cl::opt<bool>
SwiftOnly("swift-only",
          llvm::cl::desc("Only include APIs defined from Swift source"),
          llvm::cl::init(false),
          llvm::cl::cat(Category));

static llvm::cl::opt<bool>
DisableOSChecks("disable-os-checks",
                llvm::cl::desc("Skip OS related diagnostics"),
                llvm::cl::init(false),
                llvm::cl::cat(Category));

static llvm::cl::opt<bool>
PrintModule("print-module", llvm::cl::desc("Print module names in diagnostics"),
            llvm::cl::cat(Category));

static llvm::cl::opt<ActionType>
Action(llvm::cl::desc("Mode:"), llvm::cl::init(ActionType::None),
      llvm::cl::cat(Category),
      llvm::cl::values(
          clEnumValN(ActionType::DumpSDK,
                     "dump-sdk",
                     "Dump SDK content to JSON file"),
          clEnumValN(ActionType::MigratorGen,
                     "generate-migration-script",
                     "Compare SDK content in JSON file and generate migration script"),
          clEnumValN(ActionType::DiagnoseSDKs,
                     "diagnose-sdk",
                     "Diagnose SDK content in JSON file"),
          clEnumValN(ActionType::DeserializeDiffItems,
                     "deserialize-diff",
                     "Deserialize diff items in a JSON file"),
          clEnumValN(ActionType::DeserializeSDK,
                     "deserialize-sdk",
                     "Deserialize sdk digester in a JSON file"),
          clEnumValN(ActionType::FindUsr,
                     "find-usr",
                     "Find USR for decls by given condition"),
          clEnumValN(ActionType::GenerateNameCorrectionTemplate,
                     "generate-name-correction",
                     "Generate name correction template"),
          clEnumValN(ActionType::GenerateEmptyBaseline,
                     "generate-empty-baseline",
                     "Generate an empty baseline")));

static llvm::cl::list<std::string>
SDKJsonPaths("input-paths",
            llvm::cl::desc("The SDK contents under comparison"),
            llvm::cl::cat(Category));

static llvm::cl::list<std::string>
ApisPrintUsrs("api-usrs",
              llvm::cl::desc("The name of APIs to print their usrs, "
                             "e.g. Type::Function"),
              llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
IgnoreRemovedDeclUSRs("ignored-usrs",
                      llvm::cl::desc("the file containing USRs of removed decls "
                                     "that the digester should ignore"),
                      llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
SwiftVersion("swift-version",
             llvm::cl::desc("The Swift compiler version to invoke"),
             llvm::cl::cat(Category));

static llvm::cl::opt<bool>
OutputInJson("json", llvm::cl::desc("Print output in JSON format."),
             llvm::cl::cat(Category));

static llvm::cl::opt<bool>
AvoidLocation("avoid-location",
              llvm::cl::desc("Avoid serializing the file paths of SDK nodes."),
              llvm::cl::cat(Category));

static llvm::cl::opt<bool>
AvoidToolArgs("avoid-tool-args",
              llvm::cl::desc("Avoid serializing the arguments for invoking the tool."),
              llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
LocationFilter("location",
              llvm::cl::desc("Filter nodes with the given location."),
              llvm::cl::cat(Category));

static llvm::cl::opt<bool>
CompilerStyleDiags("compiler-style-diags",
                   llvm::cl::desc("Print compiler style diagnostics to stderr."),
                   llvm::cl::cat(Category));

static llvm::cl::opt<bool>
Migrator("migrator",
         llvm::cl::desc("Dump Json suitable for generating migration script"),
         llvm::cl::cat(Category));

static llvm::cl::list<std::string>
PreferInterfaceForModules("use-interface-for-module", llvm::cl::ZeroOrMore,
                          llvm::cl::desc("Prefer loading these modules via interface"),
                          llvm::cl::cat(Category));

static llvm::cl::opt<std::string>
BaselineFilePath("baseline-path",
                 llvm::cl::desc("The path to the Json file that we should use as the baseline"),
                 llvm::cl::cat(Category));

static llvm::cl::opt<bool>
UseEmptyBaseline("empty-baseline",
                llvm::cl::desc("Use empty baseline for diagnostics"),
                llvm::cl::cat(Category));
} // namespace options

namespace {

using swift::ide::api::KnownProtocolKind;

// A node matcher will traverse two trees of SDKNode and find matched nodes
struct NodeMatcher {
  virtual void match() = 0;
  virtual ~NodeMatcher() = default;
};

// During the matching phase, any matched node will be reported using this API.
// For update Node left = {Node before change} Right = {Node after change};
// For added Node left = {NilNode} Right = {Node after change};
// For removed Node left = {Node before change} Right = {NilNode}
struct MatchedNodeListener {
  virtual void foundMatch(NodePtr Left, NodePtr Right, NodeMatchReason Reason) = 0;
  virtual ~MatchedNodeListener() = default;
};

template<typename T>
bool contains(std::vector<T*> &container, T *instance) {
  return std::find(container.begin(), container.end(), instance) != container.end();
}

template<typename T>
bool contains(ArrayRef<T> container, T instance) {
  return std::find(container.begin(), container.end(), instance) != container.end();
}

static
void singleMatch(SDKNode* Left, SDKNode *Right, MatchedNodeListener &Listener) {

  // Both null, be forgiving.
  if (!Left && !Right)
    return;
  // If both are valid and identical to each other, we don't need to match them.
  if (Left && Right && *Left == *Right)
    return;
  if (!Left || !Right)
    Listener.foundMatch(Left, Right,
                        Left ? NodeMatchReason::Removed : NodeMatchReason::Added);
  else
    Listener.foundMatch(Left, Right, NodeMatchReason::Sequential);
}

// Given two NodeVector, this matches SDKNode by the order of their appearance
// in the respective NodeVector. We use this in the order-sensitive cases, such
// as parameters in a function decl.
class SequentialNodeMatcher : public NodeMatcher {
  ArrayRef<SDKNode*> Left;
  ArrayRef<SDKNode*> Right;
  MatchedNodeListener &Listener;
public:
  SequentialNodeMatcher(ArrayRef<SDKNode*> Left,
                        ArrayRef<SDKNode*> Right,
                        MatchedNodeListener &Listener) :
                          Left(Left), Right(Right), Listener(Listener) {}

  void match() override {
    for (unsigned long i = 0; i < std::max(Left.size(), Right.size()); i ++) {
      auto L = i < Left.size() ? Left[i] : nullptr;
      auto R = i < Right.size() ? Right[i] : nullptr;
      singleMatch(L, R, Listener);
    }
  }
};

struct NodeMatch {
  NodePtr Left;
  NodePtr Right;
};

class BestMatchMatcher : public NodeMatcher {
  NodeVector &Left;
  NodeVector &Right;
  llvm::function_ref<bool(NodePtr, NodePtr)> CanMatch;
  llvm::function_ref<bool(NodeMatch, NodeMatch)> IsFirstMatchBetter;
  NodeMatchReason Reason;
  MatchedNodeListener &Listener;
  llvm::SmallPtrSet<NodePtr, 16> MatchedRight;

  bool internalCanMatch(NodePtr L, NodePtr R) {
    return MatchedRight.count(R) == 0 && CanMatch(L, R);
  }

  Optional<NodePtr> findBestMatch(NodePtr Pin, NodeVector& Candidates) {
    Optional<NodePtr> Best;
    for (auto Can : Candidates) {
      if (!internalCanMatch(Pin, Can))
        continue;
      if (!Best.hasValue() ||
          IsFirstMatchBetter({Pin, Can}, {Pin, Best.getValue()}))
        Best = Can;
    }
    return Best;
  }

public:
  BestMatchMatcher(NodeVector &Left, NodeVector &Right,
                   llvm::function_ref<bool(NodePtr, NodePtr)> CanMatch,
                   llvm::function_ref<bool(NodeMatch, NodeMatch)> IsFirstMatchBetter,
                   NodeMatchReason Reason,
                   MatchedNodeListener &Listener) : Left(Left), Right(Right),
  CanMatch(CanMatch),
  IsFirstMatchBetter(IsFirstMatchBetter), Reason(Reason),
  Listener(Listener){}

  void match() override {
    for (auto L : Left) {
      if (auto Best = findBestMatch(L, Right)) {
        MatchedRight.insert(Best.getValue());
        Listener.foundMatch(L, Best.getValue(), Reason);
      }
    }
  }
};

class RemovedAddedNodeMatcher : public NodeMatcher, public MatchedNodeListener {
  NodeVector &Removed;
  NodeVector &Added;
  MatchedNodeListener &Listener;

  NodeVector RemovedMatched;
  NodeVector AddedMatched;

  void handleUnmatch(NodeVector &Matched, NodeVector &All, bool Left) {
    for (auto A : All) {
      if (contains(Matched, A))
        continue;
      if (Left)
        Listener.foundMatch(A, nullptr, NodeMatchReason::Removed);
      else
        Listener.foundMatch(nullptr, A, NodeMatchReason::Added);
    }
  }

  bool detectFuncToProperty(SDKNode *R, SDKNode *A) {
    if (R->getKind() == SDKNodeKind::DeclFunction) {
      if (A->getKind() == SDKNodeKind::DeclVar) {
        if (A->getName().compare_lower(R->getName()) == 0) {
          R->annotate(NodeAnnotation::GetterToProperty);
        } else if (R->getName().startswith("get") &&
                   R->getName().substr(3).compare_lower(A->getName()) == 0) {
          R->annotate(NodeAnnotation::GetterToProperty);
        } else if (R->getName().startswith("set") &&
                   R->getName().substr(3).compare_lower(A->getName()) == 0) {
          R->annotate(NodeAnnotation::SetterToProperty);
        } else {
          return false;
        }
        R->annotate(NodeAnnotation::PropertyName, A->getPrintedName());
        foundMatch(R, A, NodeMatchReason::FuncToProperty);
        return true;
      }
    }
    return false;
  }

  static bool isAnonymousEnum(SDKNodeDecl *N) {
    return N->getKind() == SDKNodeKind::DeclVar &&
      N->getUsr().startswith("c:@Ea@");
  }

  static bool isNominalEnum(SDKNodeDecl *N) {
    return N->getKind() == SDKNodeKind::DeclType &&
    N->getUsr().startswith("c:@E@");
  }

  static Optional<StringRef> getLastPartOfUsr(SDKNodeDecl *N) {
    auto LastPartIndex = N->getUsr().find_last_of('@');
    if (LastPartIndex == StringRef::npos)
      return None;
    return N->getUsr().substr(LastPartIndex + 1);
  }

  bool detectTypeAliasChange(SDKNodeDecl *R, SDKNodeDecl *A) {
    if (R->getPrintedName() != A->getPrintedName())
      return false;
    if (R->getKind() == SDKNodeKind::DeclType &&
        A->getKind() == SDKNodeKind::DeclTypeAlias) {
      foundMatch(R, A, NodeMatchReason::TypeToTypeAlias);
      return true;
    } else {
      return false;
    }
  }

  bool detectModernizeEnum(SDKNodeDecl *R, SDKNodeDecl *A) {
    if (!isAnonymousEnum(R) || !isNominalEnum(A))
      return false;

    auto LastPartOfR = getLastPartOfUsr(R);
    if (!LastPartOfR)
      return false;

    for (auto Child : A->getChildren()) {
      if (auto VC = dyn_cast<SDKNodeDeclVar>(Child)) {
      auto LastPartOfA = getLastPartOfUsr(VC);
        if (LastPartOfA && LastPartOfR.getValue() == LastPartOfA.getValue()) {
          std::string FullName = (llvm::Twine(A->getName()) + "." +
            Child->getName()).str();
          R->annotate(NodeAnnotation::ModernizeEnum,
                      R->getSDKContext().buffer(FullName));
          foundMatch(R, A, NodeMatchReason::ModernizeEnum);
          return true;
        }
      }
    }
    return false;
  }

  bool detectSameAnonymousEnum(SDKNodeDecl *R, SDKNodeDecl *A) {
    if (!isAnonymousEnum(R) || !isAnonymousEnum(A))
      return false;
    auto LastR = getLastPartOfUsr(R);
    auto LastA = getLastPartOfUsr(A);
    if (LastR && LastA && LastR.getValue() == LastA.getValue()) {
      foundMatch(R, A, NodeMatchReason::Name);
      return true;
    }
    return false;
  }

  static bool isNameTooSimple(StringRef N) {
    static std::vector<std::string> SimpleNames = {"unit", "data", "log", "coding",
      "url", "name", "date", "datecomponents", "notification", "urlrequest",
      "personnamecomponents", "measurement", "dateinterval", "indexset"};
    return std::find(SimpleNames.begin(), SimpleNames.end(), N) !=
      SimpleNames.end();
  }

  static bool isSimilarName(StringRef L, StringRef R) {
    auto LL = L.lower();
    auto RR = R.lower();
    if (isNameTooSimple(LL) || isNameTooSimple(RR))
      return false;
    if (((StringRef)LL).startswith(RR) || ((StringRef)RR).startswith(LL))
      return true;
    if (((StringRef)LL).startswith((llvm::Twine("ns") + RR).str()) ||
        ((StringRef)RR).startswith((llvm::Twine("ns") + LL).str()))
      return true;
    if (((StringRef)LL).endswith(RR) || ((StringRef)RR).endswith(LL))
      return true;
    return false;
  }

  /// Whether two decls of different decl kinds can be considered as rename.
  static bool isDeclKindCrossable(DeclKind DK1, DeclKind DK2, bool First) {
    if (DK1 == DK2)
      return true;
    if (DK1 == DeclKind::Var && DK2 == DeclKind::EnumElement)
      return true;
    return First && isDeclKindCrossable(DK2, DK1, false);
  }

  static bool isRename(NodePtr L, NodePtr R) {
    if (L->getKind() != R->getKind())
      return false;
    if (isa<SDKNodeDeclConstructor>(L))
      return false;
    if (auto LD = dyn_cast<SDKNodeDecl>(L)) {
      auto *RD = R->getAs<SDKNodeDecl>();
      return isDeclKindCrossable(LD->getDeclKind(), RD->getDeclKind(), true) &&
        isSimilarName(LD->getName(), RD->getName());
    }
    return false;
  }

  static bool isBetterMatch(NodeMatch Match1, NodeMatch Match2) {
    assert(Match1.Left == Match2.Left);
    auto Left = Match1.Left;
    auto *M1Right = Match1.Right->getAs<SDKNodeDecl>();
    auto *M2Right = Match2.Right->getAs<SDKNodeDecl>();

    // Consider non-deprecated nodes better matches.
    auto Dep1 = M1Right->isDeprecated();
    auto Dep2 = M2Right->isDeprecated();
    if (Dep1 ^ Dep2) {
      return Dep2;
    }

    // If two names are identical, measure whose printed names is closer.
    if (M1Right->getName() == M2Right->getName()) {
      return
        M1Right->getPrintedName().edit_distance(Left->getPrintedName()) <
        M2Right->getPrintedName().edit_distance(Left->getPrintedName());
    }

#define DIST(A, B) (std::max(A, B) - std::min(A, B))
    return
      DIST(Left->getName().size(), Match1.Right->getName().size()) <
      DIST(Left->getName().size(), Match2.Right->getName().size());
#undef DIST
  }

  void foundMatch(NodePtr R, NodePtr A, NodeMatchReason Reason) override {
    Listener.foundMatch(R, A, Reason);
    RemovedMatched.push_back(R);
    AddedMatched.push_back(A);
  }

public:
  RemovedAddedNodeMatcher(NodeVector &Removed, NodeVector &Added,
                          MatchedNodeListener &Listener) : Removed(Removed),
                            Added(Added), Listener(Listener) {}

  void match() override {
    auto IsDecl = [](NodePtr P) { return isa<SDKNodeDecl>(P); };
    for (auto R : SDKNodeVectorViewer(Removed, IsDecl)) {
      for (auto A : SDKNodeVectorViewer(Added, IsDecl)) {
        auto RD = R->getAs<SDKNodeDecl>();
        auto AD = A->getAs<SDKNodeDecl>();
        if (detectFuncToProperty(RD, AD) || detectModernizeEnum(RD, AD) ||
            detectSameAnonymousEnum(RD, AD) || detectTypeAliasChange(RD, AD)) {
          break;
        }
      }
    }

    // Rename detection starts.
    NodeVector RenameLeft;
    NodeVector RenameRight;

    for (auto Remain : Removed) {
      if (!contains(RemovedMatched, Remain))
        RenameLeft.push_back(Remain);
    }

    for (auto Remain : Added) {
      if (!contains(AddedMatched, Remain))
        RenameRight.push_back(Remain);
    }

    BestMatchMatcher RenameMatcher(RenameLeft, RenameRight, isRename,
                                   isBetterMatch, NodeMatchReason::Name, *this);
    RenameMatcher.match();
    // Rename detection ends.

    handleUnmatch(RemovedMatched, Removed, true);
    handleUnmatch(AddedMatched, Added, false);
  }
};

// Given two NodeVector, this matches SDKNode by the their names; only Nodes with
// the identical names will be matched. We use this in name-sensitive but
// order-insensitive cases, such as matching types in a module.
class SameNameNodeMatcher : public NodeMatcher {
  ArrayRef<SDKNode*> Left;
  ArrayRef<SDKNode*> Right;
  MatchedNodeListener &Listener;

  enum class NameMatchKind {
    USR,
    PrintedName,
    PrintedNameAndUSR,
  };

  static bool isUSRSame(SDKNode *L, SDKNode *R) {
    auto *LD = dyn_cast<SDKNodeDecl>(L);
    auto *RD = dyn_cast<SDKNodeDecl>(R);
    if (!LD || !RD)
      return false;
    return LD->getUsr() == RD->getUsr();
  }

  // Given two SDK nodes, figure out the reason for why they have the same name.
  Optional<NameMatchKind> getNameMatchKind(SDKNode *L, SDKNode *R) {
    if (L->getKind() != R->getKind())
      return None;
    auto NameEqual = L->getPrintedName() == R->getPrintedName();
    auto UsrEqual = isUSRSame(L, R);
    if (NameEqual && UsrEqual)
      return NameMatchKind::PrintedNameAndUSR;
    else if (NameEqual)
      return NameMatchKind::PrintedName;
    else if (UsrEqual)
      return NameMatchKind::USR;
    else
      return None;
  }

  struct NameMatchCandidate {
    SDKNode *Node;
    NameMatchKind Kind;
  };

  // Get the priority for the favored name match kind. Favored name match kind
  // locats before less favored ones.
  ArrayRef<NameMatchKind> getNameMatchKindPriority(SDKNodeKind Kind) {
    if (Kind == SDKNodeKind::DeclFunction) {
      static NameMatchKind FuncPriority[] = { NameMatchKind::PrintedNameAndUSR,
                                              NameMatchKind::USR,
                                              NameMatchKind::PrintedName };
      return FuncPriority;
    } else {
      static NameMatchKind OtherPriority[] = { NameMatchKind::PrintedNameAndUSR,
                                               NameMatchKind::PrintedName,
                                               NameMatchKind::USR };
      return OtherPriority;
    }
  }

  // Given a list and a priority, find the best matched candidate SDK node.
  SDKNode* findBestNameMatch(ArrayRef<NameMatchCandidate> Candidates,
                             ArrayRef<NameMatchKind> Kinds) {
    for (auto Kind : Kinds)
      for (auto &Can : Candidates)
        if (Kind == Can.Kind)
          return Can.Node;
    return nullptr;
  }

public:
  SameNameNodeMatcher(ArrayRef<SDKNode*> Left, ArrayRef<SDKNode*> Right,
                      MatchedNodeListener &Listener) : Left(Left), Right(Right),
                                                       Listener(Listener) {}
  void match() override ;
};

void SameNameNodeMatcher::match() {
  NodeVector MatchedRight;
  NodeVector Removed;
  NodeVector Added;

  for (auto *LN : Left) {

    // This collects all the candidates that can match with LN.
    std::vector<NameMatchCandidate> Candidates;
    for (auto *RN : Right) {

      // If RN has matched before, ignore it.
      if (contains(MatchedRight, RN))
        continue;

      // If LN and RN have the same name for some reason, keep track of RN.
      if (auto Kind = getNameMatchKind(LN, RN))
        Candidates.push_back({RN, Kind.getValue()});
    }

    // Try to find the best match among all the candidates by the priority name
    // match kind list.
    if (auto Match = findBestNameMatch(Candidates,
                                    getNameMatchKindPriority(LN->getKind()))) {
      Listener.foundMatch(LN, Match, NodeMatchReason::Name);
      MatchedRight.push_back(Match);
    } else {
      Removed.push_back(LN);
    }
  }
  for (auto &R : Right) {
    if (!contains(MatchedRight, R)) {
      Added.push_back(R);
    }
  }
  RemovedAddedNodeMatcher RAMatcher(Removed, Added, Listener);
  RAMatcher.match();
}

// The recursive version of sequential matcher. We do not only match two vectors
// of NodePtr but also their descendents.
class SequentialRecursiveMatcher : public NodeMatcher {
  NodePtr &Left;
  NodePtr &Right;
  MatchedNodeListener &Listener;

  void matchInternal(NodePtr L, NodePtr R) {
    Listener.foundMatch(L, R, NodeMatchReason::Sequential);
    if (!L || !R)
      return;
    for (unsigned I = 0; I < std::max(L->getChildrenCount(),
                                      R->getChildrenCount()); ++ I) {
      auto Left = I < L->getChildrenCount() ? L->childAt(I) : nullptr;
      auto Right = I < R->getChildrenCount() ? R->childAt(I): nullptr;
      matchInternal(Left, Right);
    }
  }

public:
  SequentialRecursiveMatcher(NodePtr &Left, NodePtr &Right,
                             MatchedNodeListener &Listener) : Left(Left),
                              Right(Right), Listener(Listener) {}
  void match() override {
    matchInternal(Left, Right);
  }
};


// This is the interface of all passes on the given trees rooted at Left and Right.
class SDKTreeDiffPass {
public:
  virtual void pass(NodePtr Left, NodePtr Right) = 0;
  virtual ~SDKTreeDiffPass() {}
};

static void detectRename(SDKNode *L, SDKNode *R) {
  if (L->getKind() == R->getKind() && isa<SDKNodeDecl>(L) &&
      L->getPrintedName() != R->getPrintedName()) {
    L->annotate(NodeAnnotation::Rename);
    L->annotate(NodeAnnotation::RenameOldName, L->getPrintedName());
    L->annotate(NodeAnnotation::RenameNewName, R->getPrintedName());
  }
}

static bool isOwnershipEquivalent(ReferenceOwnership Left,
                                  ReferenceOwnership Right) {
  if (Left == Right)
    return true;
  if (Left == ReferenceOwnership::Unowned && Right == ReferenceOwnership::Weak)
    return true;
  if (Left == ReferenceOwnership::Weak && Right == ReferenceOwnership::Unowned)
    return true;
  return false;
}
}// End of anonymous namespace

void swift::ide::api::SDKNodeDeclType::diagnose(SDKNode *Right) {
  SDKNodeDecl::diagnose(Right);
  auto *R = dyn_cast<SDKNodeDeclType>(Right);
  if (!R)
    return;
  auto Loc = R->getLoc();
  if (getDeclKind() != R->getDeclKind()) {
    emitDiag(Loc, diag::decl_kind_changed, getDeclKindStr(R->getDeclKind()));
    return;
  }

  assert(getDeclKind() == R->getDeclKind());
  auto DKind = getDeclKind();
  switch (DKind) {
  case DeclKind::Class: {
    auto LSuperClass = getSuperClassName();
    auto RSuperClass = R->getSuperClassName();
    if (!LSuperClass.empty() && LSuperClass != RSuperClass) {
      if (RSuperClass.empty()) {
        emitDiag(Loc, diag::super_class_removed, LSuperClass);
      } else if (!contains(R->getClassInheritanceChain(), LSuperClass)) {
        emitDiag(Loc, diag::super_class_changed, LSuperClass, RSuperClass);
      }
    }
    break;
  }
  default:
    break;
  }
}

void swift::ide::api::SDKNodeDeclAbstractFunc::diagnose(SDKNode *Right) {
  SDKNodeDecl::diagnose(Right);
  auto *R = dyn_cast<SDKNodeDeclAbstractFunc>(Right);
  if (!R)
    return;
  auto Loc = R->getLoc();
  if (!isThrowing() && R->isThrowing()) {
    emitDiag(Loc, diag::decl_new_attr, Ctx.buffer("throwing"));
  }
  if (Ctx.checkingABI()) {
    if (reqNewWitnessTableEntry() != R->reqNewWitnessTableEntry()) {
      emitDiag(Loc, diag::decl_new_witness_table_entry, reqNewWitnessTableEntry());
    }
  }
}

void swift::ide::api::SDKNodeDeclFunction::diagnose(SDKNode *Right) {
  SDKNodeDeclAbstractFunc::diagnose(Right);
  auto *R = dyn_cast<SDKNodeDeclFunction>(Right);
  if (!R)
    return;
  auto Loc = R->getLoc();
  if (getSelfAccessKind() != R->getSelfAccessKind()) {
    emitDiag(Loc, diag::func_self_access_change, getSelfAccessKind(),
             R->getSelfAccessKind());
  }
  if (Ctx.checkingABI()) {
    if (hasFixedBinaryOrder() != R->hasFixedBinaryOrder()) {
      emitDiag(Loc, diag::func_has_fixed_order_change, hasFixedBinaryOrder());
    }
  }
}

static StringRef getAttrName(DeclAttrKind Kind) {
  switch (Kind) {
#define DECL_ATTR(NAME, CLASS, ...)                                           \
  case DAK_##CLASS:                                                           \
      return DeclAttribute::isDeclModifier(DAK_##CLASS) ? #NAME : "@"#NAME;
#include "swift/AST/Attr.def"
  case DAK_Count:
    llvm_unreachable("unrecognized attribute kind.");
  }
  llvm_unreachable("covered switch");
}

static bool shouldDiagnoseAddingAttribute(SDKNodeDecl *D, DeclAttrKind Kind) {
  return true;
}

static bool shouldDiagnoseRemovingAttribute(SDKNodeDecl *D, DeclAttrKind Kind) {
  return true;
}

void swift::ide::api::SDKNodeDecl::diagnose(SDKNode *Right) {
  SDKNode::diagnose(Right);
  auto *RD = dyn_cast<SDKNodeDecl>(Right);
  if (!RD)
    return;
  detectRename(this, RD);
  auto Loc = RD->getLoc();
  if (isOpen() && !RD->isOpen()) {
    emitDiag(Loc, diag::no_longer_open);
  }

  // Diagnose static attribute change.
  if (isStatic() ^ RD->isStatic()) {
    emitDiag(Loc, diag::decl_new_attr, Ctx.buffer(isStatic() ? "not static" :
                                             "static"));
  }

  // Diagnose ownership change.
  if (!isOwnershipEquivalent(getReferenceOwnership(),
                             RD->getReferenceOwnership())) {
    auto getOwnershipDescription = [&](swift::ReferenceOwnership O) {
      if (O == ReferenceOwnership::Strong)
        return Ctx.buffer("strong");
      return keywordOf(O);
    };
    emitDiag(Loc, diag::decl_attr_change,
             getOwnershipDescription(getReferenceOwnership()),
             getOwnershipDescription(RD->getReferenceOwnership()));
  }
  // Diagnose generic signature change
  if (getGenericSignature() != RD->getGenericSignature()) {
    // Prefer sugared signature in diagnostics to be more user-friendly.
    if (Ctx.commonVersionAtLeast(2) &&
        getSugaredGenericSignature() != RD->getSugaredGenericSignature()) {
      emitDiag(Loc, diag::generic_sig_change,
               getSugaredGenericSignature(), RD->getSugaredGenericSignature());
    } else {
      emitDiag(Loc, diag::generic_sig_change,
               getGenericSignature(), RD->getGenericSignature());
    }
  }

  // ObjC name changes are considered breakage
  if (getObjCName() != RD->getObjCName()) {
    if (Ctx.commonVersionAtLeast(4)) {
      emitDiag(Loc, diag::objc_name_change, getObjCName(), RD->getObjCName());
    }
  }

  if (isOptional() != RD->isOptional()) {
    if (Ctx.checkingABI()) {
      // Both adding/removing optional is ABI-breaking.
      emitDiag(Loc, diag::optional_req_changed, isOptional());
    } else if (isOptional()) {
      // Removing optional is source-breaking.
      emitDiag(Loc, diag::optional_req_changed, isOptional());
    }
  }

  // Diagnose removing attributes.
  for (auto Kind: getDeclAttributes()) {
    if (!RD->hasDeclAttribute(Kind)) {
      if ((Ctx.checkingABI() ? DeclAttribute::isRemovingBreakingABI(Kind) :
                               DeclAttribute::isRemovingBreakingAPI(Kind)) &&
          shouldDiagnoseRemovingAttribute(this, Kind)) {
        emitDiag(Loc, diag::decl_new_attr,
                Ctx.buffer((llvm::Twine("without ") + getAttrName(Kind)).str()));
      }
    }
  }

  // Diagnose adding attributes.
  for (auto Kind: RD->getDeclAttributes()) {
    if (!hasDeclAttribute(Kind)) {
      if ((Ctx.checkingABI() ? DeclAttribute::isAddingBreakingABI(Kind) :
                               DeclAttribute::isAddingBreakingAPI(Kind)) &&
          shouldDiagnoseAddingAttribute(this, Kind)) {
        emitDiag(Loc, diag::decl_new_attr,
                Ctx.buffer((llvm::Twine("with ") + getAttrName(Kind)).str()));
      }
    }
  }

  if (Ctx.checkingABI()) {
    if (hasFixedBinaryOrder() && RD->hasFixedBinaryOrder() &&
        getFixedBinaryOrder() != RD->getFixedBinaryOrder()) {
      emitDiag(Loc, diag::decl_reorder, getFixedBinaryOrder(),
               RD->getFixedBinaryOrder());
    }
  }
}

void swift::ide::api::SDKNodeDeclOperator::diagnose(SDKNode *Right) {
  SDKNodeDecl::diagnose(Right);
  auto *RO = dyn_cast<SDKNodeDeclOperator>(Right);
  if (!RO)
    return;
  auto Loc = RO->getLoc();
  if (getDeclKind() != RO->getDeclKind()) {
    emitDiag(Loc, diag::decl_kind_changed, getDeclKindStr(RO->getDeclKind()));
  }
}

void swift::ide::api::SDKNodeDeclVar::diagnose(SDKNode *Right) {
  SDKNodeDecl::diagnose(Right);
  auto *RV = dyn_cast<SDKNodeDeclVar>(Right);
  if (!RV)
    return;
  auto Loc = RV->getLoc();
  if (Ctx.checkingABI()) {
    if (hasFixedBinaryOrder() != RV->hasFixedBinaryOrder()) {
      emitDiag(Loc, diag::var_has_fixed_order_change, hasFixedBinaryOrder());
    }
    if (isLet() != RV->isLet()) {
      emitDiag(Loc, diag::var_let_changed, isLet());
    }
  }
}

static bool shouldDiagnoseType(SDKNodeType *T) {
  return T->isTopLevelType();
}

void swift::ide::api::SDKNodeType::diagnose(SDKNode *Right) {
  SDKNode::diagnose(Right);
  auto *RT = dyn_cast<SDKNodeType>(Right);
  if (!RT || !shouldDiagnoseType(this))
    return;
  assert(isTopLevelType());

  // Diagnose type witness changes when diagnosing ABI breakages.
  if (auto *Wit = dyn_cast<SDKNodeTypeWitness>(getParent())) {
    auto *Conform = Wit->getParent()->getAs<SDKNodeConformance>();
    if (Ctx.checkingABI() && getPrintedName() != RT->getPrintedName()) {
      auto *LD = Conform->getNominalTypeDecl();
      LD->emitDiag(SourceLoc(), diag::type_witness_change,
                   Wit->getWitnessedTypeName(),
                   getPrintedName(), RT->getPrintedName());
    }
    return;
  }

  StringRef Descriptor = getTypeRoleDescription();
  assert(isa<SDKNodeDecl>(getParent()));
  auto LParent = cast<SDKNodeDecl>(getParent());
  assert(LParent->getKind() == RT->getParent()->getAs<SDKNodeDecl>()->getKind());
  auto Loc = RT->getParent()->getAs<SDKNodeDecl>()->getLoc();
  if (getPrintedName() != RT->getPrintedName()) {
    LParent->emitDiag(Loc, diag::decl_type_change,
                      Descriptor, getPrintedName(), RT->getPrintedName());
  }

  if (hasDefaultArgument() && !RT->hasDefaultArgument()) {
    LParent->emitDiag(Loc, diag::default_arg_removed, Descriptor);
  }
  if (getParamValueOwnership() != RT->getParamValueOwnership()) {
    LParent->emitDiag(Loc, diag::param_ownership_change,
                      getTypeRoleDescription(),
                      getParamValueOwnership(),
                      RT->getParamValueOwnership());
  }
}

void swift::ide::api::SDKNodeTypeFunc::diagnose(SDKNode *Right) {
  SDKNodeType::diagnose(Right);
  auto *RT = dyn_cast<SDKNodeTypeFunc>(Right);
  if (!RT || !shouldDiagnoseType(this))
    return;
  assert(isTopLevelType());
  auto Loc = RT->getParent()->getAs<SDKNodeDecl>()->getLoc();
  if (Ctx.checkingABI() && isEscaping() != RT->isEscaping()) {
    getParent()->getAs<SDKNodeDecl>()->emitDiag(Loc,
                                                diag::func_type_escaping_changed,
                                                getTypeRoleDescription(),
                                                isEscaping());
  }
}

namespace {
// This is first pass on two given SDKNode trees. This pass removes the common part
// of two versions of SDK, leaving only the changed part.
class PrunePass : public MatchedNodeListener, public SDKTreeDiffPass {

  static void removeCommon(NodeVector &Left, NodeVector &Right) {
    NodeVector LeftMinusRight, RightMinusLeft;
    nodeSetDifference(Left, Right, LeftMinusRight, RightMinusLeft);
    Left = LeftMinusRight;
    Right = RightMinusLeft;
  }

  static void removeCommonChildren(NodePtr Left, NodePtr Right) {
    removeCommon(Left->getChildren(), Right->getChildren());
  }

  SDKContext &Ctx;
  UpdatedNodesMap &UpdateMap;
  llvm::StringSet<> ProtocolReqWhitelist;
  SDKNodeRoot *LeftRoot;
  SDKNodeRoot *RightRoot;

  static void printSpaces(llvm::raw_ostream &OS, SDKNode *N) {
    assert(N);
    StringRef Space = "        ";
    // Accessor doesn't have parent.
    if (auto *AC = dyn_cast<SDKNodeDeclAccessor>(N)) {
      OS << Space;
      printSpaces(OS, AC->getStorage());
      return;
    }
    for (auto P = N; !isa<SDKNodeRoot>(P); P = P->getParent())
      OS << Space;
  }

  static void debugMatch(SDKNode *Left, SDKNode *Right, NodeMatchReason Reason,
                         llvm::raw_ostream &OS) {
    if (Left && !isa<SDKNodeDecl>(Left))
      return;
    if (Right && !isa<SDKNodeDecl>(Right))
      return;
    StringRef Arrow = "  <-------->  ";
    switch (Reason) {
    case NodeMatchReason::Added:
      printSpaces(OS, Right);
      OS << "<NULL>" << Arrow << Right->getPrintedName() << "\n";
      return;
    case NodeMatchReason::Removed:
      printSpaces(OS, Left);
      OS << Left->getPrintedName() << Arrow << "<NULL>\n";
      return;
    default:
      printSpaces(OS, Left);
      OS << Left->getPrintedName() << Arrow << Right->getPrintedName() << "\n";
      return;
    }
  }

  static StringRef getParentProtocolName(SDKNode *Node) {
    if (auto *Acc = dyn_cast<SDKNodeDeclAccessor>(Node)) {
      Node = Acc->getStorage();
    }
    return Node->getParent()->getAs<SDKNodeDecl>()->getFullyQualifiedName();
  }

public:
  PrunePass(SDKContext &Ctx): Ctx(Ctx), UpdateMap(Ctx.getNodeUpdateMap()) {}
  PrunePass(SDKContext &Ctx, llvm::StringSet<> prWhitelist):
    Ctx(Ctx),
    UpdateMap(Ctx.getNodeUpdateMap()),
    ProtocolReqWhitelist(std::move(prWhitelist)) {}

  void foundMatch(NodePtr Left, NodePtr Right, NodeMatchReason Reason) override {
    if (options::DebugMapping)
      debugMatch(Left, Right, Reason, llvm::errs());
    switch (Reason) {
    case NodeMatchReason::Added:
      assert(!Left);
      Right->annotate(NodeAnnotation::Added);
      if (Ctx.checkingABI()) {
        // Any order-important decl added to a non-resilient type breaks ABI.
        if (auto *D = dyn_cast<SDKNodeDecl>(Right)) {
          if (D->hasFixedBinaryOrder()) {
            D->emitDiag(D->getLoc(), diag::decl_added);
          }
          // Diagnose the missing of @available attributes.
          // Decls with @_alwaysEmitIntoClient aren't required to have an
          // @available attribute.
          if (!Ctx.getOpts().SkipOSCheck &&
              !D->getIntroducingVersion().hasOSAvailability() &&
              !D->hasDeclAttribute(DeclAttrKind::DAK_AlwaysEmitIntoClient)) {
            D->emitDiag(D->getLoc(), diag::new_decl_without_intro);
          }
        }
      }
      // Complain about added protocol requirements
      if (auto *D = dyn_cast<SDKNodeDecl>(Right)) {
        if (D->isNonOptionalProtocolRequirement()) {
          bool ShouldComplain = !D->isOverriding();
          // We should allow added associated types with default.
          if (auto ATD = dyn_cast<SDKNodeDeclAssociatedType>(D)) {
            if (ATD->getDefault())
              ShouldComplain = false;
          }
          if (ShouldComplain &&
              ProtocolReqWhitelist.count(getParentProtocolName(D))) {
            // Ignore protocol requirement additions if the protocol has been added
            // to the whitelist.
            ShouldComplain = false;
          }
          if (ShouldComplain)
            D->emitDiag(D->getLoc(), diag::protocol_req_added);
        }
      }
      // Diagnose an inherited protocol has been added.
      if (auto *Conf = dyn_cast<SDKNodeConformance>(Right)) {
        auto *TD = Conf->getNominalTypeDecl();
        if (TD->isProtocol()) {
          TD->emitDiag(TD->getLoc(), diag::conformance_added, Conf->getName());
        } else {
          // Adding conformance to an existing type can be ABI breaking.
          if (Ctx.checkingABI() &&
              !LeftRoot->getDescendantsByUsr(Conf->getUsr()).empty()) {
            TD->emitDiag(TD->getLoc(), diag::existing_conformance_added,
                         Conf->getName());
          }
        }
      }
      if (auto *CD = dyn_cast<SDKNodeDeclConstructor>(Right)) {
        if (auto *TD = dyn_cast<SDKNodeDeclType>(Right->getParent())) {
          if (TD->isOpen() && CD->getInitKind() == CtorInitializerKind::Designated) {
            // If client's subclass provides an implementation of all of its superclass designated
            // initializers, it automatically inherits all of the superclass convenience initializers.
            // This means if a new designated init is added to the base class, the inherited
            // convenience init may be missing and cause breakage.
            CD->emitDiag(CD->getLoc(), diag::desig_init_added);
          }
        }
      }

      return;
    case NodeMatchReason::Removed:
      assert(!Right);
      Left->annotate(NodeAnnotation::Removed);
      if (auto *LT = dyn_cast<SDKNodeType>(Left)) {
        if (auto *AT = dyn_cast<SDKNodeDeclAssociatedType>(LT->getParent())) {
          AT->emitDiag(SourceLoc(), diag::default_associated_type_removed,
                       LT->getPrintedName());
        }
      }
      // Diagnose a protocol conformance has been removed.
      if (auto *Conf = dyn_cast<SDKNodeConformance>(Left)) {
        auto *TD = Conf->getNominalTypeDecl();
        TD->emitDiag(SourceLoc(),
                     diag::conformance_removed,
                     Conf->getName(),
                     TD->isProtocol());
      }
      if (auto *Acc = dyn_cast<SDKNodeDeclAccessor>(Left)) {
        Acc->emitDiag(SourceLoc(), diag::removed_decl, Acc->isDeprecated());
      }
      return;
    case NodeMatchReason::FuncToProperty:
    case NodeMatchReason::ModernizeEnum:
    case NodeMatchReason::TypeToTypeAlias:
      Left->annotate(NodeAnnotation::Removed);
      Right->annotate(NodeAnnotation::Added);
      return;
    case NodeMatchReason::Root:
    case NodeMatchReason::Name:
    case NodeMatchReason::Sequential:
      break;
    }
    assert(Left && Right);
    Left->annotate(NodeAnnotation::Updated);
    Right->annotate(NodeAnnotation::Updated);
    // Push the updated node to the map for future reference.
    UpdateMap.insert(Left, Right);

    Left->diagnose(Right);
    if (Left->getKind() != Right->getKind()) {
      assert(isa<SDKNodeType>(Left) && isa<SDKNodeType>(Right) &&
        "only type nodes can match across kinds.");
      return;
    }
    assert(Left->getKind() == Right->getKind());
    SDKNodeKind Kind = Left->getKind();
    assert(Kind == SDKNodeKind::Root || *Left != *Right);
    switch(Kind) {
    case SDKNodeKind::DeclType: {
      // Remove common conformances and diagnose conformance changes.
      auto LConf = cast<SDKNodeDeclType>(Left)->getConformances();
      auto RConf = cast<SDKNodeDeclType>(Right)->getConformances();
      removeCommon(LConf, RConf);
      SameNameNodeMatcher(LConf, RConf, *this).match();
      LLVM_FALLTHROUGH;
    }
    case SDKNodeKind::Conformance:
    case SDKNodeKind::Root: {
      // If the matched nodes are both modules, remove the contained
      // type decls that are identical. If the matched nodes are both type decls,
      // remove the contained function decls that are identical.
      removeCommonChildren(Left, Right);
      SameNameNodeMatcher SNMatcher(Left->getChildren(), Right->getChildren(), *this);
      SNMatcher.match();
      break;
    }
    case SDKNodeKind::TypeWitness:
    case SDKNodeKind::DeclOperator:
    case SDKNodeKind::DeclAssociatedType:
    case SDKNodeKind::DeclFunction:
    case SDKNodeKind::DeclAccessor:
    case SDKNodeKind::DeclConstructor:
    case SDKNodeKind::DeclTypeAlias:
    case SDKNodeKind::TypeFunc:
    case SDKNodeKind::TypeNominal:
    case SDKNodeKind::TypeAlias: {
      // If matched nodes are both function/var/TypeAlias decls, mapping their
      // parameters sequentially.
      SequentialNodeMatcher SNMatcher(Left->getChildren(), Right->getChildren(),
                                      *this);
      SNMatcher.match();
      break;
    }
    case SDKNodeKind::DeclSubscript: {
      auto *LSub = dyn_cast<SDKNodeDeclSubscript>(Left);
      auto *RSub = dyn_cast<SDKNodeDeclSubscript>(Right);
      SequentialNodeMatcher(LSub->getChildren(), RSub->getChildren(), *this).match();
#define ACCESSOR(ID)                                                          \
      singleMatch(LSub->getAccessor(AccessorKind::ID),                        \
                  RSub->getAccessor(AccessorKind::ID), *this);
#include "swift/AST/AccessorKinds.def"
      break;
    }
    case SDKNodeKind::DeclVar: {
      auto *LVar = dyn_cast<SDKNodeDeclVar>(Left);
      auto *RVar = dyn_cast<SDKNodeDeclVar>(Right);
      // Match property type.
      singleMatch(LVar->getType(), RVar->getType(), *this);
#define ACCESSOR(ID)                                                          \
      singleMatch(LVar->getAccessor(AccessorKind::ID),                        \
                  RVar->getAccessor(AccessorKind::ID), *this);
#include "swift/AST/AccessorKinds.def"
      break;
    }
    }
  }

  void pass(NodePtr Left, NodePtr Right) override {
    LeftRoot = Left->getAs<SDKNodeRoot>();
    RightRoot = Right->getAs<SDKNodeRoot>();
    foundMatch(Left, Right, NodeMatchReason::Root);
  }
};


// Class to build up a diff of structurally different nodes, based on the given
// USR map for the left (original) side of the diff, based on parent types.
class TypeMemberDiffFinder : public SDKNodeVisitor {
  friend class SDKNode; // for visit()

  SDKNodeRoot *diffAgainst;

  // Vector of {givenNodePtr, diffAgainstPtr}
  NodePairVector TypeMemberDiffs;

  void visit(NodePtr node) override {
    // Skip nodes that we don't have a correlate for
    auto declNode = dyn_cast<SDKNodeDecl>(node);
    if (!declNode)
      return;
    auto usr = declNode->getUsr();
    auto &usrName = usr;

    // If we can find no nodes in the other tree with the same usr, abort.
    auto candidates = diffAgainst->getDescendantsByUsr(usrName);
    if (candidates.empty())
      return;

    // If any of the candidates has the same kind and name with the node, we
    // shouldn't continue.
    for (auto Can : candidates) {
      if (Can->getKind() == declNode->getKind() &&
          Can->getAs<SDKNodeDecl>()->getFullyQualifiedName() ==
            declNode->getFullyQualifiedName())
        return;
    }

    auto diffNode = candidates.front();
    assert(node && diffNode && "nullptr visited?");
    auto nodeParent = node->getParent();
    auto diffParent = diffNode->getParent();
    assert(nodeParent && diffParent && "trying to check Root?");

    // Move from global variable to a member variable.
    if (nodeParent->getKind() == SDKNodeKind::DeclType &&
        diffParent->getKind() == SDKNodeKind::Root)
      TypeMemberDiffs.insert({diffNode, node});

    // Move from a member variable to global variable.
    if (nodeParent->getKind() == SDKNodeKind::Root &&
        diffParent->getKind() == SDKNodeKind::DeclType)
      TypeMemberDiffs.insert({diffNode, node});

    // Move from a member variable to another member variable
    if (nodeParent->getKind() == SDKNodeKind::DeclType &&
        diffParent->getKind() == SDKNodeKind::DeclType &&
        declNode->isStatic())
      TypeMemberDiffs.insert({diffNode, node});
    // Move from a getter/setter function to a property
    else if (node->getKind() == SDKNodeKind::DeclAccessor &&
             diffNode->getKind() == SDKNodeKind::DeclFunction &&
             node->isNameValid()) {
      diffNode->annotate(NodeAnnotation::Rename);
      diffNode->annotate(NodeAnnotation::RenameOldName,
                         diffNode->getPrintedName());
      diffNode->annotate(NodeAnnotation::RenameNewName,
                         node->getParent()->getPrintedName());
    }
  }

public:
  TypeMemberDiffFinder(SDKNodeRoot *diffAgainst):
    diffAgainst(diffAgainst) {}

  void findDiffsFor(NodePtr ptr) { SDKNode::preorderVisit(ptr, *this); }

  const NodePairVector &getDiffs() const {
    return TypeMemberDiffs;
  }

  void dump(llvm::raw_ostream &) const;
  void dump() const { dump(llvm::errs()); }

private:
  TypeMemberDiffFinder(const TypeMemberDiffFinder &) = delete;
  TypeMemberDiffFinder &operator=(const TypeMemberDiffFinder &) = delete;

};

/// This is to find type alias of raw types being changed to RawRepresentable.
/// e.g. AttributeName was a typealias of String in the old SDK however it becomes
/// a RawRepresentable struct in the new SDK.
/// This happens typically when we use apinotes to preserve API stability by
/// using SwiftWrapper:none in the old SDK.
class TypeAliasDiffFinder: public SDKNodeVisitor {
  SDKNodeRoot *leftRoot;
  SDKNodeRoot *rightRoot;
  NodeMap &result;

  static bool checkTypeMatch(const SDKNodeType* aliasType,
      const SDKNodeType* rawType) {
    StringRef Left = aliasType->getPrintedName();
    StringRef Right = rawType->getPrintedName();
    if (Left == "NSString" && Right == "String")
      return true;
    if (Left == "String" && Right == "String")
      return true;
    if (Left == "Int" && Right == "Int")
      return true;
    if (Left == "UInt" && Right == "UInt")
      return true;
    return false;
  }

  void visit(NodePtr node) override {
    auto alias = dyn_cast<SDKNodeDeclTypeAlias>(node);
    if (!alias)
      return;
    const SDKNodeType* aliasType = alias->getUnderlyingType();
    for (auto *counter: rightRoot->getDescendantsByUsr(alias->getUsr())) {
      if (auto DT = dyn_cast<SDKNodeDeclType>(counter)) {
        if (auto *rawType = DT->getRawValueType()) {
          if (checkTypeMatch(aliasType, rawType)) {
            result.insert({alias, DT});
            return;
          }
        }
      }
    }
  }
public:
  TypeAliasDiffFinder(SDKNodeRoot *leftRoot, SDKNodeRoot *rightRoot,
    NodeMap &result): leftRoot(leftRoot), rightRoot(rightRoot),
      result(result) {}

  void search() {
    SDKNode::preorderVisit(leftRoot, *this);
  }
};

// Given a condition, search whether a node satisfies that condition exists
// in a tree.
class SearchVisitor : public SDKNodeVisitor {
  bool isFound = false;
  llvm::function_ref<bool(NodePtr)> Predicate;

public:
  SearchVisitor(llvm::function_ref<bool(NodePtr)> Predicate) :
    Predicate(Predicate) {}

  void visit(NodePtr Node) override {
    isFound |= Predicate(Node);
  }

  bool search(NodePtr Node) {
    SDKNode::preorderVisit(Node, *this);
    return isFound;
  }
};

class InterfaceTypeChangeDetector {
  bool IsVisitingLeft;

#define ANNOTATE(Node, Counter, X, Y)                                          \
  auto ToAnnotate = IsVisitingLeft ? Node : Counter;                           \
  ToAnnotate->annotate(IsVisitingLeft ? X : Y);

  bool detectWrapOptional(SDKNodeType *Node, SDKNodeType *Counter) {
    if (Node->getTypeKind() != KnownTypeKind::Optional &&
        Node->getTypeKind() != KnownTypeKind::ImplicitlyUnwrappedOptional &&
        Counter->getTypeKind() == KnownTypeKind::Optional &&
        *Node == *Counter->getOnlyChild()) {
      ANNOTATE(Node, Counter, NodeAnnotation::WrapOptional,
               NodeAnnotation::UnwrapOptional)
      return true;
    }
    return false;
  }

  bool detectWrapImplicitOptional(SDKNodeType *Node, SDKNodeType *Counter) {
    if (Node->getTypeKind() != KnownTypeKind::Optional &&
        Node->getTypeKind() != KnownTypeKind::ImplicitlyUnwrappedOptional &&
        Counter->getTypeKind() == KnownTypeKind::ImplicitlyUnwrappedOptional &&
        *Node == *Counter->getOnlyChild()) {
      ANNOTATE(Node, Counter, NodeAnnotation::WrapImplicitOptional,
               NodeAnnotation::UnwrapOptional)
      return true;
    }

    return false;
  }

  bool detectOptionalUpdate(SDKNodeType *Node, SDKNodeType *Counter) {
    if (Node->getTypeKind() == KnownTypeKind::Optional &&
        Counter->getTypeKind() == KnownTypeKind::ImplicitlyUnwrappedOptional &&
        *Node->getOnlyChild() == *Counter->getOnlyChild()) {
      ANNOTATE(Node, Counter,
               NodeAnnotation::OptionalToImplicitOptional,
               NodeAnnotation::ImplicitOptionalToOptional)
      return true;
    }
    return false;
  }

  bool detectUnmanagedUpdate(SDKNodeType *Node, SDKNodeType *Counter) {
    if (IsVisitingLeft && Node->getTypeKind() == KnownTypeKind::Unmanaged &&
        Counter->getTypeKind() != KnownTypeKind::Unmanaged &&
        *Node->getOnlyChild() == *Counter) {
      Node->annotate(NodeAnnotation::UnwrapUnmanaged);
      return true;
    }
    return false;
  }

#undef ANNOTATE

  bool detectTypeRewritten(SDKNodeType *Node, SDKNodeType *Counter) {
    if (IsVisitingLeft &&
        Node->getPrintedName() != Counter->getPrintedName() &&
        (Node->getName() != Counter->getName() ||
         Node->getChildrenCount() != Counter->getChildrenCount())) {
      Node->annotate(NodeAnnotation::TypeRewritten);
      Node->annotate(NodeAnnotation::TypeRewrittenLeft, Node->getPrintedName());
      Node->annotate(NodeAnnotation::TypeRewrittenRight, 
                     Counter->getPrintedName());
      return true;
    }
    return false;
  }

  static bool isRawType(const SDKNodeType *T, StringRef &Raw) {
    if (auto Alias = dyn_cast<SDKNodeTypeAlias>(T)) {
      // In case this type is an alias of the raw type.
      return isRawType(Alias->getUnderlyingType(), Raw);
    }
    switch(T->getTypeKind()) {
    case KnownTypeKind::String:
    case KnownTypeKind::Int:
      Raw = T->getName();
      return true;
    default:
      return false;
    }
  }

  static StringRef getStringRepresentableChange(SDKNode *L, SDKNode *R,
                                                StringRef &Raw) {
    if (!isRawType(L->getAs<SDKNodeType>(), Raw))
      return StringRef();
    auto* RKey = dyn_cast<SDKNodeTypeNominal>(R);
    if (!RKey)
      return StringRef();
    if (Raw.empty())
      return StringRef();
    auto Results = RKey->getRootNode()->getDescendantsByUsr(RKey->getUsr());
    if (Results.empty())
      return StringRef();
    if (auto DT = dyn_cast<SDKNodeDeclType>(Results.front())) {
      if (DT->isConformingTo(KnownProtocolKind::RawRepresentable)) {
        return DT->getFullyQualifiedName();
      }
    }
    return StringRef();
  }

  static StringRef detectDictionaryKeyChangeInternal(SDKNodeType *L,
                                                     SDKNodeType *R,
                                                     StringRef &Raw) {
    if (L->getTypeKind() != KnownTypeKind::Dictionary ||
        R->getTypeKind() != KnownTypeKind::Dictionary)
      return StringRef();
    auto *Left = dyn_cast<SDKNodeTypeNominal>(L);
    auto *Right = dyn_cast<SDKNodeTypeNominal>(R);
    assert(Left && Right);
    assert(Left->getChildrenCount() == 2);
    assert(Right->getChildrenCount() == 2);
    return getStringRepresentableChange(*Left->getChildBegin(),
      *Right->getChildBegin(), Raw);
  }

  bool detectDictionaryKeyChange(SDKNodeType *L, SDKNodeType *R) {
    // We only care if this the top-level type node.
    if (!L->isTopLevelType() || !R->isTopLevelType())
      return false;
    StringRef Raw;
    StringRef KeyChangedTo;
    bool HasOptional = L->getTypeKind() == KnownTypeKind::Optional &&
      R->getTypeKind() == KnownTypeKind::Optional;
    if (HasOptional) {
      // Detect [String: Any]? to [StringRepresentableStruct: Any]? Chnage
      KeyChangedTo =
        detectDictionaryKeyChangeInternal(L->getOnlyChild()->getAs<SDKNodeType>(),
                                          R->getOnlyChild()->getAs<SDKNodeType>(),
                                          Raw);
    } else {
      // Detect [String: Any] to [StringRepresentableStruct: Any] Chnage
      KeyChangedTo = detectDictionaryKeyChangeInternal(L, R, Raw);
    }
    if (!KeyChangedTo.empty()) {
      if (IsVisitingLeft) {
        L->annotate(HasOptional ?
                    NodeAnnotation::OptionalDictionaryKeyUpdate :
                    NodeAnnotation::DictionaryKeyUpdate);
        L->annotate(NodeAnnotation::RawTypeLeft, Raw);
        L->annotate(NodeAnnotation::RawTypeRight, KeyChangedTo);
      } else {
        R->annotate(HasOptional ?
                    NodeAnnotation::RevertOptionalDictionaryKeyUpdate :
                    NodeAnnotation::RevertDictionaryKeyUpdate);
        R->annotate(NodeAnnotation::RawTypeLeft, KeyChangedTo);
        R->annotate(NodeAnnotation::RawTypeRight, Raw);
      }
      return true;
    }
    return false;
  }

  static StringRef detectArrayMemberChangeInternal(SDKNodeType *L,
      SDKNodeType *R, StringRef &Raw) {
    if (L->getTypeKind() != KnownTypeKind::Array ||
        R->getTypeKind() != KnownTypeKind::Array)
      return StringRef();
    auto *Left = dyn_cast<SDKNodeTypeNominal>(L);
    auto *Right = dyn_cast<SDKNodeTypeNominal>(R);
    assert(Left && Right);
    assert(Left->getChildrenCount() == 1);
    assert(Right->getChildrenCount() == 1);
    return getStringRepresentableChange(Left->getOnlyChild(),
      Right->getOnlyChild(), Raw);
  }

  bool detectArrayMemberChange(SDKNodeType* L, SDKNodeType *R) {
    // We only care if this the top-level type node.
    if (!L->isTopLevelType() || !R->isTopLevelType())
      return false;
    StringRef Raw;
    StringRef KeyChangedTo;
    bool HasOptional = L->getTypeKind() == KnownTypeKind::Optional &&
      R->getTypeKind() == KnownTypeKind::Optional;
    if (HasOptional) {
      // Detect [String]? to [StringRepresentableStruct]? Chnage
      KeyChangedTo =
        detectArrayMemberChangeInternal(L->getOnlyChild()->getAs<SDKNodeType>(),
                                        R->getOnlyChild()->getAs<SDKNodeType>(),
                                        Raw);
    } else {
      // Detect [String] to [StringRepresentableStruct] Chnage
      KeyChangedTo = detectArrayMemberChangeInternal(L, R, Raw);
    }
    if (!KeyChangedTo.empty()) {
      if (IsVisitingLeft) {
        L->annotate(HasOptional ?
                    NodeAnnotation::OptionalArrayMemberUpdate :
                    NodeAnnotation::ArrayMemberUpdate);
        L->annotate(NodeAnnotation::RawTypeLeft, Raw);
        L->annotate(NodeAnnotation::RawTypeRight, KeyChangedTo);
      } else {
        R->annotate(HasOptional ?
                    NodeAnnotation::RevertOptionalArrayMemberUpdate :
                    NodeAnnotation::RevertArrayMemberUpdate);
        R->annotate(NodeAnnotation::RawTypeLeft, KeyChangedTo);
        R->annotate(NodeAnnotation::RawTypeRight, Raw);
      }
      return true;
    }
    return false;
  }

  bool detectSimpleStringRepresentableUpdate(SDKNodeType *L, SDKNodeType *R) {
    if (!L->isTopLevelType() || !R->isTopLevelType())
      return false;
    StringRef KeyChangedTo;
    StringRef Raw;
    bool HasOptional = L->getTypeKind() == KnownTypeKind::Optional &&
        R->getTypeKind() == KnownTypeKind::Optional;
    if (HasOptional) {
      // Detect String? changes to StringRepresentableStruct? change.
      KeyChangedTo =
        getStringRepresentableChange(L->getOnlyChild()->getAs<SDKNodeType>(),
                                     R->getOnlyChild()->getAs<SDKNodeType>(),
                                     Raw);
    } else {
      // Detect String changes to StringRepresentableStruct change.
      KeyChangedTo = getStringRepresentableChange(L, R, Raw);
    }
    if (!KeyChangedTo.empty()) {
      if (IsVisitingLeft) {
        L->annotate(NodeAnnotation::RawTypeLeft, Raw);
        L->annotate(NodeAnnotation::RawTypeRight, KeyChangedTo);
        L->annotate(HasOptional ?
                    NodeAnnotation::SimpleOptionalStringRepresentableUpdate:
                    NodeAnnotation::SimpleStringRepresentableUpdate);
      } else {
        R->annotate(NodeAnnotation::RawTypeLeft, KeyChangedTo);
        R->annotate(NodeAnnotation::RawTypeRight, Raw);
        R->annotate(HasOptional ?
                    NodeAnnotation::RevertSimpleOptionalStringRepresentableUpdate:
                    NodeAnnotation::RevertSimpleStringRepresentableUpdate);
      }
      return true;
    }
    return false;
  }

  bool isUnhandledCase(SDKNodeType *Node, SDKNodeType *Counter) {
    return Node->getTypeKind() == KnownTypeKind::Void ||
           Counter->getTypeKind() == KnownTypeKind::Void;
  }

  static void clearTypeRewritten(SDKNode *N) {
    if (!N->isAnnotatedAs(NodeAnnotation::TypeRewritten))
      return;
    N->removeAnnotate(NodeAnnotation::TypeRewritten);
    N->removeAnnotate(NodeAnnotation::TypeRewrittenLeft);
    N->removeAnnotate(NodeAnnotation::TypeRewrittenRight);
  }

public:
  InterfaceTypeChangeDetector(bool IsVisitingLeft):
    IsVisitingLeft(IsVisitingLeft) {}
  void detect(SDKNode *Left, SDKNode *Right) {
    auto *Node = dyn_cast<SDKNodeType>(Left);
    auto *Counter = dyn_cast<SDKNodeType>(Right);
    if (!Node || !Counter || isUnhandledCase(Node, Counter))
      return;
    if (detectWrapOptional(Node, Counter) ||
        detectOptionalUpdate(Node, Counter) ||
        detectWrapImplicitOptional(Node, Counter) ||
        detectUnmanagedUpdate(Node, Counter)) {

      // we may have detected type rewritten before (when visiting left),
      // so clear the annotation here.
      clearTypeRewritten(Node);
      clearTypeRewritten(Counter);
    } else {
      // Detect type re-written then.
      detectTypeRewritten(Node, Counter);
    }
    // The raw representable changes can co-exist with above attributes.
    auto Result = detectDictionaryKeyChange(Node, Counter) ||
      detectArrayMemberChange(Node, Counter) ||
      detectSimpleStringRepresentableUpdate(Node, Counter);
    (void) Result;
    return;
  }
};

class ChangeRefinementPass : public SDKTreeDiffPass, public SDKNodeVisitor {
  UpdatedNodesMap &UpdateMap;
  InterfaceTypeChangeDetector LeftDetector;
  InterfaceTypeChangeDetector RightDetector;
  InterfaceTypeChangeDetector *Detector;

public:
  ChangeRefinementPass(UpdatedNodesMap &UpdateMap) : UpdateMap(UpdateMap),
    LeftDetector(true), RightDetector(false), Detector(nullptr) {}

  void pass(NodePtr Left, NodePtr Right) override {

    // Post-order visit is necessary since we propagate annotations bottom-up
    Detector = &LeftDetector;
    SDKNode::postorderVisit(Left, *this);
    Detector = &RightDetector;
    SDKNode::postorderVisit(Right, *this);
  }

  void visit(NodePtr Node) override {
    assert(Detector);
    if (!Node || !Node->isAnnotatedAs(NodeAnnotation::Updated))
      return;
    auto *Counter = UpdateMap.findUpdateCounterpart(Node);
    Detector->detect(Node, Counter);
    return;
  }
};
} // end anonymous namespace

static void findTypeMemberDiffs(NodePtr leftSDKRoot, NodePtr rightSDKRoot,
                                TypeMemberDiffVector &out);

static void printNode(llvm::raw_ostream &os, NodePtr node) {
  os << "{" << node->getName() << " " << node->getKind() << " "
            << node->getPrintedName();
  if (auto F = dyn_cast<SDKNodeDeclAbstractFunc>(node)) {
    if (F->hasSelfIndex()) {
      os << " selfIndex: ";
      os << F->getSelfIndex();
    }
  }
  os << "}";
}

void TypeMemberDiffFinder::dump(llvm::raw_ostream &os) const {
  for (auto pair : getDiffs()) {
    os << " - ";
    printNode(os, pair.first);
    os << " parent: ";
    printNode(os, pair.first->getParent());

    os << "\n + ";
    printNode(os, pair.second);
    os << " parent: ";
    printNode(os, pair.second->getParent());
    os << "\n\n";
  }
}

namespace {

template<typename T>
void removeRedundantAndSort(std::vector<T> &Diffs) {
  std::set<T> DiffSet(Diffs.begin(), Diffs.end());
  Diffs.assign(DiffSet.begin(), DiffSet.end());
  std::sort(Diffs.begin(), Diffs.end());
}

template<typename T>
void serializeDiffs(llvm::raw_ostream &Fs, std::vector<T> &Diffs) {
  if (Diffs.empty())
    return;
  Fs << "\n";
  T::describe(Fs);
  for (auto &Diff : Diffs) {
    Diff.streamDef(Fs);
    Fs << "\n";
  }
  T::undef(Fs);
  Fs << "\n";
}

static bool isTypeChangeInterestedFuncNode(NodePtr Decl) {
  switch(Decl->getKind()) {
    case SDKNodeKind::DeclConstructor:
    case SDKNodeKind::DeclFunction:
      return true;
    default:
      return false;
  }
}

class DiffItemEmitter : public SDKNodeVisitor {
  DiffVector &AllItems;

  static bool isInterested(SDKNodeDecl* Decl, NodeAnnotation Anno) {
    switch (Anno) {
      case NodeAnnotation::WrapOptional:
      case NodeAnnotation::UnwrapOptional:
      case NodeAnnotation::ImplicitOptionalToOptional:
      case NodeAnnotation::OptionalToImplicitOptional:
      case NodeAnnotation::UnwrapUnmanaged:
      case NodeAnnotation::TypeRewritten:
        return isTypeChangeInterestedFuncNode(Decl) &&
        Decl->getParent()->getKind() == SDKNodeKind::DeclType;
      default:
        return true;
    }
  }

  bool doesAncestorHaveTypeRewritten() {
    return std::find_if(Ancestors.begin(), Ancestors.end(),[](NodePtr N) {
      return N->isAnnotatedAs(NodeAnnotation::TypeRewritten);
    }) != Ancestors.end();
  }

  static StringRef getLeftComment(NodePtr Node, NodeAnnotation Anno) {
    switch(Anno) {
      case NodeAnnotation::ArrayMemberUpdate:
      case NodeAnnotation::OptionalArrayMemberUpdate:
      case NodeAnnotation::DictionaryKeyUpdate:
      case NodeAnnotation::OptionalDictionaryKeyUpdate:
      case NodeAnnotation::SimpleStringRepresentableUpdate:
      case NodeAnnotation::SimpleOptionalStringRepresentableUpdate:
      case NodeAnnotation::RevertArrayMemberUpdate:
      case NodeAnnotation::RevertOptionalArrayMemberUpdate:
      case NodeAnnotation::RevertDictionaryKeyUpdate:
      case NodeAnnotation::RevertOptionalDictionaryKeyUpdate:
      case NodeAnnotation::RevertSimpleStringRepresentableUpdate:
      case NodeAnnotation::RevertSimpleOptionalStringRepresentableUpdate:
        return Node->getAnnotateComment(NodeAnnotation::RawTypeLeft);
      case NodeAnnotation::TypeRewritten:
        return Node->getAnnotateComment(NodeAnnotation::TypeRewrittenLeft);
      case NodeAnnotation::Rename:
        return Node->getAnnotateComment(NodeAnnotation::RenameOldName);
      default:
        return StringRef();
    }
  }

  static StringRef getRightComment(NodePtr Node, NodeAnnotation Anno) {
    switch (Anno) {
      case NodeAnnotation::ArrayMemberUpdate:
      case NodeAnnotation::OptionalArrayMemberUpdate:
      case NodeAnnotation::DictionaryKeyUpdate:
      case NodeAnnotation::OptionalDictionaryKeyUpdate:
      case NodeAnnotation::SimpleStringRepresentableUpdate:
      case NodeAnnotation::SimpleOptionalStringRepresentableUpdate:
      case NodeAnnotation::RevertArrayMemberUpdate:
      case NodeAnnotation::RevertOptionalArrayMemberUpdate:
      case NodeAnnotation::RevertDictionaryKeyUpdate:
      case NodeAnnotation::RevertOptionalDictionaryKeyUpdate:
      case NodeAnnotation::RevertSimpleStringRepresentableUpdate:
      case NodeAnnotation::RevertSimpleOptionalStringRepresentableUpdate:
        return Node->getAnnotateComment(NodeAnnotation::RawTypeRight);
      case NodeAnnotation::TypeRewritten:
        return Node->getAnnotateComment(NodeAnnotation::TypeRewrittenRight);
      case NodeAnnotation::ModernizeEnum:
        return Node->getAnnotateComment(NodeAnnotation::ModernizeEnum);
      case NodeAnnotation::Rename:
        return Node->getAnnotateComment(NodeAnnotation::RenameNewName);
      case NodeAnnotation::GetterToProperty:
      case NodeAnnotation::SetterToProperty:
        return Node->getAnnotateComment(NodeAnnotation::PropertyName);
      default:
        return StringRef();
    }
  }

  void handleAnnotations(NodePtr Node, SDKNodeDecl *NonTypeParent,
                         StringRef Index, ArrayRef<NodeAnnotation> Annotations) {
    for (auto Anno: Annotations) {
      if (isInterested(NonTypeParent, Anno) && Node->isAnnotatedAs(Anno)) {
        auto Kind = NonTypeParent->getKind();
        StringRef LC = getLeftComment(Node, Anno);
        StringRef RC = getRightComment(Node, Anno);
        AllItems.emplace_back(Kind, Anno, Index,
                              NonTypeParent->getUsr(), StringRef(), LC, RC,
                              NonTypeParent->getModuleName());
      }
    }
  }

  void visit(NodePtr Node) override {
    auto *Parent = dyn_cast<SDKNodeDecl>(Node);
    if (!Parent) {
      if (auto TN = dyn_cast<SDKNodeType>(Node)) {
        Parent = TN->getClosestParentDecl();
      }
    }

    if (!Parent)
      return;
    if (doesAncestorHaveTypeRewritten())
      return;

    handleAnnotations(Node, Parent,
                      isa<SDKNodeType>(Node) ? getIndexString(Node) : "0",
                      {
#define NODE_ANNOTATION_CHANGE_KIND(NAME) NodeAnnotation::NAME,
#include "swift/IDE/DigesterEnums.def"
                      });
  }

  StringRef getIndexString(NodePtr Node) {
    llvm::SmallString<32> Builder;
    std::vector<int> Indexes;
    collectIndexes(Node, Indexes);
    auto First = true;
    for (auto I : Indexes) {
      if (!First)
        Builder.append(":");
      else
        First = false;
      Builder.append(std::to_string(I));
    }
    return Node->getSDKContext().buffer(Builder.str());
  }

  void collectIndexes(NodePtr Node, std::vector<int> &Indexes) {
    for (unsigned I = Ancestors.size(); I > 0 && (I == Ancestors.size() ||
        isa<SDKNodeType>(Ancestors[I])); -- I) {
      auto Child = I == Ancestors.size() ? Node : Ancestors[I];
      auto Parent = Ancestors[I - 1];
      Indexes.insert(Indexes.begin(), Parent->getChildIndex(Child));
    }
  }
  DiffItemEmitter(DiffVector &AllItems) : AllItems(AllItems) {}

public:
  static void collectDiffItems(NodePtr Root, DiffVector &DV) {
    DiffItemEmitter Emitter(DV);
    SDKNode::postorderVisit(Root, Emitter);
  }
};

class DiagnosisEmitter : public SDKNodeVisitor {
  void handle(const SDKNodeDecl *D, NodeAnnotation Anno);
  void visitDecl(SDKNodeDecl *D);
  void visit(NodePtr Node) override;
  SDKNodeDecl *findAddedDecl(const SDKNodeDecl *Node);
  bool findTypeAliasDecl(const SDKNodeDecl *Node);
  static void collectAddedDecls(NodePtr Root, std::set<SDKNodeDecl*> &Results);

  std::set<SDKNodeDecl*> AddedDecls;

  UpdatedNodesMap &UpdateMap;
  NodeMap &TypeAliasUpdateMap;
  TypeMemberDiffVector &MemberChanges;
  DiagnosisEmitter(SDKContext &Ctx):
    UpdateMap(Ctx.getNodeUpdateMap()),
    TypeAliasUpdateMap(Ctx.getTypeAliasUpdateMap()),
    MemberChanges(Ctx.getTypeMemberDiffs()) {}

public:
  static void diagnosis(NodePtr LeftRoot, NodePtr RightRoot,
                        SDKContext &Ctx);
};

void DiagnosisEmitter::collectAddedDecls(NodePtr Root,
                                         std::set<SDKNodeDecl*> &Results) {
  if (auto *D = dyn_cast<SDKNodeDecl>(Root)) {
    if (Root->isAnnotatedAs(NodeAnnotation::Added))
      Results.insert(D);
  }
  for (auto &C : Root->getChildren())
    collectAddedDecls(C, Results);
}

SDKNodeDecl *DiagnosisEmitter::findAddedDecl(const SDKNodeDecl *Root) {
  for (auto *Added : AddedDecls) {
    if (Root->getKind() == Added->getKind() &&
        Root->getPrintedName() == Added->getPrintedName() &&
        Root->getUsr() == Added->getUsr())
      return Added;
  }
  return nullptr;
}

bool DiagnosisEmitter::findTypeAliasDecl(const SDKNodeDecl *Node) {
  if (Node->getKind() != SDKNodeKind::DeclType)
    return false;
  return std::any_of(AddedDecls.begin(), AddedDecls.end(),
      [&](SDKNodeDecl *Added) {
    return Added->getKind() == SDKNodeKind::DeclTypeAlias &&
           Added->getPrintedName() == Node->getPrintedName();
  });
}

void DiagnosisEmitter::diagnosis(NodePtr LeftRoot, NodePtr RightRoot,
                                 SDKContext &Ctx) {
  DiagnosisEmitter Emitter(Ctx);
  collectAddedDecls(RightRoot, Emitter.AddedDecls);
  SDKNode::postorderVisit(LeftRoot, Emitter);
}

static bool diagnoseRemovedExtensionMembers(const SDKNode *Node) {
  // If the removed decl is an extension, diagnose each member as being removed rather than
  // the extension itself has been removed.
  if (auto *DT= dyn_cast<SDKNodeDeclType>(Node)) {
    if (DT->isExtension()) {
      for (auto *C: DT->getChildren()) {
        auto *MD = cast<SDKNodeDecl>(C);
        MD->emitDiag(SourceLoc(), diag::removed_decl, MD->isDeprecated());
      }
      return true;
    }
  }
  return false;
}

void DiagnosisEmitter::handle(const SDKNodeDecl *Node, NodeAnnotation Anno) {
  assert(Node->isAnnotatedAs(Anno));
  auto &Ctx = Node->getSDKContext();
  switch(Anno) {
  case NodeAnnotation::Removed: {
    // If we can find a type alias decl with the same name of this type, we
    // consider the type is not removed.
    if (findTypeAliasDecl(Node))
      return;
    if (auto *Added = findAddedDecl(Node)) {
      if (Node->getDeclKind() != DeclKind::Constructor) {
        Node->emitDiag(Added->getLoc(), diag::moved_decl,
          Ctx.buffer((Twine(getDeclKindStr(Added->getDeclKind())) + " " +
            Added->getFullyQualifiedName()).str()));
        return;
      }
    }

    // If we can find a hoisted member for this removed delcaration, we
    // emit the diagnostics as rename instead of removal.
    auto It = std::find_if(MemberChanges.begin(), MemberChanges.end(),
      [&](TypeMemberDiffItem &Item) { return Item.usr == Node->getUsr(); });
    if (It != MemberChanges.end()) {
      Node->emitDiag(SourceLoc(), diag::renamed_decl,
        Ctx.buffer((Twine(getDeclKindStr(Node->getDeclKind())) + " " +
          It->newTypeName + "." + It->newPrintedName).str()));
      return;
    }

    // If a type alias of a raw type has been changed to a struct/enum that
    // conforms to RawRepresentable in the later version of SDK, we show the
    // refine diagnostics message instead of showing the type alias has been
    // removed.
    if (TypeAliasUpdateMap.find((SDKNode*)Node) != TypeAliasUpdateMap.end()) {
      Node->emitDiag(SourceLoc(), diag::raw_type_change,
        Node->getAs<SDKNodeDeclTypeAlias>()->getUnderlyingType()->getPrintedName(),
        TypeAliasUpdateMap[(SDKNode*)Node]->getAs<SDKNodeDeclType>()->
          getRawValueType()->getPrintedName());
      return;
    }

    // We should exlude those declarations that are pulled up to the super classes.
    bool FoundInSuperclass = false;
    if (auto PD = dyn_cast<SDKNodeDecl>(Node->getParent())) {
      if (PD->isAnnotatedAs(NodeAnnotation::Updated)) {
        // Get the updated counterpart of the parent decl.
        if (auto RTD = dyn_cast<SDKNodeDeclType>(UpdateMap.
            findUpdateCounterpart(PD))) {
          // Look up by the printed name in the counterpart.
          FoundInSuperclass =
            RTD->lookupChildByPrintedName(Node->getPrintedName()).hasValue();
        }
      }
    }
    if (FoundInSuperclass)
      return;

    // When diagnosing API changes, avoid complaining the removal of these
    // synthesized functions since they are compiler implementation details.
    // If an enum is no longer equatable, another diagnostic about removing
    // conforming protocol will be emitted.
    if (!Ctx.checkingABI()) {
      if (Node->getName() == Ctx.Id_derived_struct_equals ||
          Node->getName() == Ctx.Id_derived_enum_equals) {
        return;
      }
    }
    bool handled = diagnoseRemovedExtensionMembers(Node);
    if (!handled)
      Node->emitDiag(SourceLoc(), diag::removed_decl, Node->isDeprecated());
    return;
  }
  case NodeAnnotation::Rename: {
    SourceLoc DiagLoc;
    // Try to get the source location from the later version of this node
    // via UpdateMap.
    if (auto CD = dyn_cast_or_null<SDKNodeDecl>(UpdateMap
                                                .findUpdateCounterpart(Node))) {
      DiagLoc = CD->getLoc();
    }
    Node->emitDiag(DiagLoc, diag::renamed_decl,
        Ctx.buffer((Twine(getDeclKindStr(Node->getDeclKind())) + " " +
          Node->getAnnotateComment(NodeAnnotation::RenameNewName)).str()));
    return;
  }
  default:
    return;
  }
}

void DiagnosisEmitter::visitDecl(SDKNodeDecl *Node) {
  std::vector<NodeAnnotation> Scratch;
  for (auto Anno : Node->getAnnotations(Scratch))
    handle(Node, Anno);
}

void DiagnosisEmitter::visit(NodePtr Node) {
  if (auto *DNode = dyn_cast<SDKNodeDecl>(Node)) {
    visitDecl(DNode);
  }
}

typedef std::vector<NoEscapeFuncParam> NoEscapeFuncParamVector;

class NoEscapingFuncEmitter : public SDKNodeVisitor {
  NoEscapeFuncParamVector &AllItems;
  NoEscapingFuncEmitter(NoEscapeFuncParamVector &AllItems) : AllItems(AllItems) {}

  void visit(NodePtr Node) override {
    if (Node->getKind() != SDKNodeKind::TypeFunc)
      return;
    if (Node->getAs<SDKNodeTypeFunc>()->isEscaping())
      return;
    auto Parent = Node->getParent();
    if (auto ParentFunc = dyn_cast<SDKNodeDeclAbstractFunc>(Parent)) {
      if (ParentFunc->isObjc()) {
        unsigned Index = ParentFunc->getChildIndex(Node);
        AllItems.emplace_back(ParentFunc->getUsr(), Index);
      }
    }
  }

public:
  static void collectDiffItems(NodePtr Root, NoEscapeFuncParamVector &DV) {
    NoEscapingFuncEmitter Emitter(DV);
    SDKNode::postorderVisit(Root, Emitter);
  }
};
} // end anonymous namespace

namespace fs = llvm::sys::fs;
namespace path = llvm::sys::path;

class RenameDetectorForMemberDiff : public MatchedNodeListener {
  InterfaceTypeChangeDetector LeftDetector;
  InterfaceTypeChangeDetector RightDetector;
public:
  RenameDetectorForMemberDiff(): LeftDetector(true), RightDetector(false) {}
  void foundMatch(NodePtr Left, NodePtr Right, NodeMatchReason Reason) override {
    if (!Left || !Right)
      return;
    detectRename(Left, Right);
    LeftDetector.detect(Left, Right);
    RightDetector.detect(Right, Left);
  }
  void workOn(NodePtr Left, NodePtr Right) {
    if (Left->getKind() == Right->getKind() &&
        Left->getKind() == SDKNodeKind::DeclType) {
      SameNameNodeMatcher SNMatcher(Left->getChildren(), Right->getChildren(),
                                    *this);
      SNMatcher.match();
    }
    if (Left->getKind() == Right->getKind() &&
        Left->getKind() == SDKNodeKind::DeclVar) {
      SequentialNodeMatcher Matcher(Left->getChildren(),
                                    Right->getChildren(), *this);
      Matcher.match();
    }
  }
};

static Optional<uint8_t> findSelfIndex(SDKNode* Node) {
  if (auto func = dyn_cast<SDKNodeDeclAbstractFunc>(Node)) {
    return func->getSelfIndexOptional();
  } else if (auto vd = dyn_cast<SDKNodeDeclVar>(Node)) {
    for (auto &C : vd->getChildren()) {
      if (isa<SDKNodeDeclAbstractFunc>(C)) {
        if (auto Result = findSelfIndex(C))
          return Result;
      }
    }
  }
  return None;
}

/// Find cases where a diff is due to a change to being a type member
static void findTypeMemberDiffs(NodePtr leftSDKRoot, NodePtr rightSDKRoot,
                                TypeMemberDiffVector &out) {
  TypeMemberDiffFinder diffFinder(cast<SDKNodeRoot>(leftSDKRoot));
  diffFinder.findDiffsFor(rightSDKRoot);
  RenameDetectorForMemberDiff Detector;
  for (auto pair : diffFinder.getDiffs()) {
    auto left = pair.first;
    auto leftParent = left->getParent();
    auto right = pair.second;
    auto rightParent = right->getParent();

    // SDK_CHANGE_TYPE_MEMBER(USR, new type context name, new printed name, self
    // index, old printed name)
    TypeMemberDiffItem item = {
        right->getAs<SDKNodeDecl>()->getUsr(),
        rightParent->getKind() == SDKNodeKind::Root ?
          StringRef() : rightParent->getAs<SDKNodeDecl>()->getFullyQualifiedName(),
        right->getPrintedName(), findSelfIndex(right), None,
        leftParent->getKind() == SDKNodeKind::Root ?
          StringRef() : leftParent->getAs<SDKNodeDecl>()->getFullyQualifiedName(),
        left->getPrintedName()
    };
    out.emplace_back(item);
    Detector.workOn(left, right);
  }
}

static int diagnoseModuleChange(SDKContext &Ctx, SDKNodeRoot *LeftModule,
                             SDKNodeRoot *RightModule, StringRef OutputPath,
                             llvm::StringSet<> ProtocolReqWhitelist) {
  assert(LeftModule);
  assert(RightModule);
  llvm::raw_ostream *OS = &llvm::errs();
  if (!LeftModule || !RightModule) {
    *OS << "Cannot diagnose null SDKNodeRoot";
    exit(1);
  }
  std::unique_ptr<llvm::raw_ostream> FileOS;
  if (!OutputPath.empty()) {
    std::error_code EC;
    FileOS.reset(new llvm::raw_fd_ostream(OutputPath, EC, llvm::sys::fs::F_None));
    OS = FileOS.get();
  }
  std::unique_ptr<DiagnosticConsumer> pConsumer = options::CompilerStyleDiags ?
    llvm::make_unique<PrintingDiagnosticConsumer>():
    llvm::make_unique<ModuleDifferDiagsConsumer>(true, *OS);

  Ctx.addDiagConsumer(*pConsumer);
  Ctx.setCommonVersion(std::min(LeftModule->getJsonFormatVersion(),
                                RightModule->getJsonFormatVersion()));
  TypeAliasDiffFinder(LeftModule, RightModule,
                      Ctx.getTypeAliasUpdateMap()).search();
  PrunePass Prune(Ctx, std::move(ProtocolReqWhitelist));
  Prune.pass(LeftModule, RightModule);
  ChangeRefinementPass RefinementPass(Ctx.getNodeUpdateMap());
  RefinementPass.pass(LeftModule, RightModule);
  // Find member hoist changes to help refine diagnostics.
  findTypeMemberDiffs(LeftModule, RightModule, Ctx.getTypeMemberDiffs());
  DiagnosisEmitter::diagnosis(LeftModule, RightModule, Ctx);
  return options::CompilerStyleDiags && Ctx.getDiags().hadAnyError() ? 1 : 0;
}

static int diagnoseModuleChange(StringRef LeftPath, StringRef RightPath,
                                StringRef OutputPath,
                                CheckerOptions Opts,
                                llvm::StringSet<> ProtocolReqWhitelist) {
  if (!fs::exists(LeftPath)) {
    llvm::errs() << LeftPath << " does not exist\n";
    return 1;
  }
  if (!fs::exists(RightPath)) {
    llvm::errs() << RightPath << " does not exist\n";
    return 1;
  }
  SDKContext Ctx(Opts);
  SwiftDeclCollector LeftCollector(Ctx);
  LeftCollector.deSerialize(LeftPath);
  SwiftDeclCollector RightCollector(Ctx);
  RightCollector.deSerialize(RightPath);
  diagnoseModuleChange(Ctx, LeftCollector.getSDKRoot(), RightCollector.getSDKRoot(),
                       OutputPath, std::move(ProtocolReqWhitelist));
  return options::CompilerStyleDiags && Ctx.getDiags().hadAnyError() ? 1 : 0;
}

static void populateAliasChanges(NodeMap &AliasMap, DiffVector &AllItems,
    const bool isRevert) {
  for (auto Pair: AliasMap) {
    auto UnderlyingType = Pair.first->getAs<SDKNodeDeclTypeAlias>()->
      getUnderlyingType()->getPrintedName();
    auto RawType = AliasMap[(SDKNode*)Pair.first]->getAs<SDKNodeDeclType>()->
      getRawValueType()->getPrintedName();
    if (isRevert) {
      auto *D = Pair.second->getAs<SDKNodeDecl>();
      AllItems.emplace_back(SDKNodeKind::DeclType,
        NodeAnnotation::RevertTypeAliasDeclToRawRepresentable, "0",
        D->getUsr(), "", RawType, UnderlyingType, D->getModuleName());
    } else {
      auto *D = Pair.first->getAs<SDKNodeDecl>();
      AllItems.emplace_back(SDKNodeKind::DeclTypeAlias,
        NodeAnnotation::TypeAliasDeclToRawRepresentable, "0",
        D->getUsr(), "", UnderlyingType, RawType, D->getModuleName());
    }
  }
}

static int generateMigrationScript(StringRef LeftPath, StringRef RightPath,
                                   StringRef DiffPath,
                                   llvm::StringSet<> &IgnoredRemoveUsrs,
                                   CheckerOptions Opts) {
  if (!fs::exists(LeftPath)) {
    llvm::errs() << LeftPath << " does not exist\n";
    return 1;
  }
  if (!fs::exists(RightPath)) {
    llvm::errs() << RightPath << " does not exist\n";
    return 1;
  }
  llvm::errs() << "Diffing: " << LeftPath << " and " << RightPath << "\n";
  std::unique_ptr<DiagnosticConsumer> pConsumer = options::CompilerStyleDiags ?
    llvm::make_unique<PrintingDiagnosticConsumer>():
    llvm::make_unique<ModuleDifferDiagsConsumer>(false);
  SDKContext Ctx(Opts);
  Ctx.addDiagConsumer(*pConsumer);

  SwiftDeclCollector LeftCollector(Ctx);
  LeftCollector.deSerialize(LeftPath);
  SwiftDeclCollector RightCollector(Ctx);
  RightCollector.deSerialize(RightPath);
  llvm::errs() << "Finished deserializing" << "\n";
  auto LeftModule = LeftCollector.getSDKRoot();
  auto RightModule = RightCollector.getSDKRoot();
  Ctx.setCommonVersion(std::min(LeftModule->getJsonFormatVersion(),
                                RightModule->getJsonFormatVersion()));
  // Structural diffs: not merely name changes but changes in SDK tree
  // structure.
  llvm::errs() << "Detecting type member diffs" << "\n";
  findTypeMemberDiffs(LeftModule, RightModule, Ctx.getTypeMemberDiffs());

  PrunePass Prune(Ctx);
  Prune.pass(LeftModule, RightModule);
  llvm::errs() << "Finished pruning" << "\n";
  ChangeRefinementPass RefinementPass(Ctx.getNodeUpdateMap());
  RefinementPass.pass(LeftModule, RightModule);
  DiffVector AllItems;
  DiffItemEmitter::collectDiffItems(LeftModule, AllItems);

  // Find type alias change first.
  auto &AliasMap = Ctx.getTypeAliasUpdateMap();
  TypeAliasDiffFinder(LeftModule, RightModule, AliasMap).search();
  populateAliasChanges(AliasMap, AllItems, /*IsRevert*/false);

  // Find type alias revert change.
  auto &RevertAliasMap = Ctx.getRevertTypeAliasUpdateMap();
  TypeAliasDiffFinder(RightModule, LeftModule, RevertAliasMap).search();
  populateAliasChanges(RevertAliasMap, AllItems, /*IsRevert*/true);

  AllItems.erase(std::remove_if(AllItems.begin(), AllItems.end(),
                                [&](CommonDiffItem &Item) {
    return Item.DiffKind == NodeAnnotation::RemovedDecl &&
      IgnoredRemoveUsrs.find(Item.LeftUsr) != IgnoredRemoveUsrs.end();
  }), AllItems.end());

  NoEscapeFuncParamVector AllNoEscapingFuncs;
  NoEscapingFuncEmitter::collectDiffItems(RightModule, AllNoEscapingFuncs);

  llvm::errs() << "Dumping diff to " << DiffPath << '\n';
  std::vector<OverloadedFuncInfo> Overloads;
  // OverloadMemberFunctionEmitter::collectDiffItems(RightModule, Overloads);

  auto &typeMemberDiffs = Ctx.getTypeMemberDiffs();
  std::error_code EC;
  llvm::raw_fd_ostream Fs(DiffPath, EC, llvm::sys::fs::F_None);
  removeRedundantAndSort(AllItems);
  removeRedundantAndSort(typeMemberDiffs);
  removeRedundantAndSort(AllNoEscapingFuncs);
  removeRedundantAndSort(Overloads);
  if (options::OutputInJson) {
    std::vector<APIDiffItem*> TotalItems;
    std::transform(AllItems.begin(), AllItems.end(),
                   std::back_inserter(TotalItems),
                   [](CommonDiffItem &Item) { return &Item; });
    std::transform(typeMemberDiffs.begin(), typeMemberDiffs.end(),
                   std::back_inserter(TotalItems),
                   [](TypeMemberDiffItem &Item) { return &Item; });
    std::transform(AllNoEscapingFuncs.begin(), AllNoEscapingFuncs.end(),
                   std::back_inserter(TotalItems),
                   [](NoEscapeFuncParam &Item) { return &Item; });
    std::transform(Overloads.begin(), Overloads.end(),
                   std::back_inserter(TotalItems),
                   [](OverloadedFuncInfo &Item) { return &Item; });
    APIDiffItemStore::serialize(Fs, TotalItems);
    return 0;
  }
  serializeDiffs(Fs, AllItems);
  serializeDiffs(Fs, typeMemberDiffs);
  serializeDiffs(Fs, AllNoEscapingFuncs);
  serializeDiffs(Fs, Overloads);
  return 0;
}

static int readFileLineByLine(StringRef Path, llvm::StringSet<> &Lines) {
  auto FileBufOrErr = llvm::MemoryBuffer::getFile(Path);
  if (!FileBufOrErr) {
    llvm::errs() << "error opening file '" << Path << "': "
      << FileBufOrErr.getError().message() << '\n';
    return 1;
  }

  StringRef BufferText = FileBufOrErr.get()->getBuffer();
  while (!BufferText.empty()) {
    StringRef Line;
    std::tie(Line, BufferText) = BufferText.split('\n');
    Line = Line.trim();
    if (Line.empty())
      continue;
    if (Line.startswith("// ")) // comment.
      continue;
    Lines.insert(Line);
  }
  return 0;
}

// This function isn't referenced outside its translation unit, but it
// can't use the "static" keyword because its address is used for
// getMainExecutable (since some platforms don't support taking the
// address of main, and some platforms can't implement getMainExecutable
// without being given the address of a function in the main executable).
void anchorForGetMainExecutable() {}

static void setSDKPath(CompilerInvocation &InitInvok, bool IsBaseline) {
  if (IsBaseline) {
    // Set baseline SDK
    if (!options::BaselineSDK.empty()) {
      InitInvok.setSDKPath(options::BaselineSDK);
    }
  } else {
    // Set current SDK
    if (!options::SDK.empty()) {
      InitInvok.setSDKPath(options::SDK);
    } else if (const char *SDKROOT = getenv("SDKROOT")) {
      InitInvok.setSDKPath(SDKROOT);
    } else {
      llvm::errs() << "Provide '-sdk <path>' option or run with 'xcrun -sdk <..>\
      swift-api-digester'\n";
      exit(1);
    }
  }
}

static int prepareForDump(const char *Main,
                          CompilerInvocation &InitInvok,
                          llvm::StringSet<> &Modules,
                          bool IsBaseline = false) {
  InitInvok.setMainExecutablePath(fs::getMainExecutable(Main,
    reinterpret_cast<void *>(&anchorForGetMainExecutable)));
  InitInvok.setModuleName("swift_ide_test");
  setSDKPath(InitInvok, IsBaseline);

  if (!options::Triple.empty())
    InitInvok.setTargetTriple(options::Triple);

  // Ensure the tool works on linux properly
  InitInvok.getLangOptions().EnableObjCInterop =
    InitInvok.getLangOptions().Target.isOSDarwin();
  InitInvok.getClangImporterOptions().ModuleCachePath =
  options::ModuleCachePath;

  if (!options::SwiftVersion.empty()) {
    using version::Version;
    bool isValid = false;
    if (auto Version = Version::parseVersionString(options::SwiftVersion,
                                                   SourceLoc(), nullptr)) {
      if (auto Effective = Version.getValue().getEffectiveLanguageVersion()) {
        InitInvok.getLangOptions().EffectiveLanguageVersion = *Effective;
        isValid = true;
      }
    }
    if (!isValid) {
      llvm::errs() << "Unsupported Swift Version.\n";
      exit(1);
    }
  }

  if (!options::ResourceDir.empty()) {
    InitInvok.setRuntimeResourcePath(options::ResourceDir);
  }
  std::vector<SearchPathOptions::FrameworkSearchPath> FramePaths;
  for (const auto &path : options::CCSystemFrameworkPaths) {
    FramePaths.push_back({path, /*isSystem=*/true});
  }
  if (IsBaseline) {
    for (const auto &path : options::BaselineFrameworkPaths) {
      FramePaths.push_back({path, /*isSystem=*/false});
    }
    InitInvok.setImportSearchPaths(options::BaselineModuleInputPaths);
  } else {
    for (const auto &path : options::FrameworkPaths) {
      FramePaths.push_back({path, /*isSystem=*/false});
    }
    InitInvok.setImportSearchPaths(options::ModuleInputPaths);
  }
  InitInvok.setFrameworkSearchPaths(FramePaths);
  if (!options::ModuleList.empty()) {
    if (readFileLineByLine(options::ModuleList, Modules))
      exit(1);
  }
  for (auto M : options::ModuleNames) {
    Modules.insert(M);
  }
  for (auto M: options::PreferInterfaceForModules) {
    InitInvok.getFrontendOptions().PreferInterfaceForModules.push_back(M);
  }
  if (Modules.empty()) {
    llvm::errs() << "Need to specify -include-all or -module <name>\n";
    exit(1);
  }
  return 0;
}

static void readIgnoredUsrs(llvm::StringSet<> &IgnoredUsrs) {
  StringRef Path = options::IgnoreRemovedDeclUSRs;
  if (Path.empty())
    return;
  if (!fs::exists(Path)) {
    llvm::errs() << Path << " does not exist.\n";
    return;
  }
  readFileLineByLine(Path, IgnoredUsrs);
}

static int deserializeDiffItems(APIDiffItemStore &Store, StringRef DiffPath,
    StringRef OutputPath) {
  Store.addStorePath(DiffPath);
  std::error_code EC;
  llvm::raw_fd_ostream FS(OutputPath, EC, llvm::sys::fs::F_None);
  APIDiffItemStore::serialize(FS, Store.getAllDiffItems());
  return 0;
}

static int deserializeNameCorrection(APIDiffItemStore &Store,
                                     StringRef OutputPath) {
  std::error_code EC;
  llvm::raw_fd_ostream FS(OutputPath, EC, llvm::sys::fs::F_None);
  std::set<NameCorrectionInfo> Result;
  for (auto *Item: Store.getAllDiffItems()) {
    if (auto *CI = dyn_cast<CommonDiffItem>(Item)) {
      if (CI->DiffKind == NodeAnnotation::Rename) {
        auto NewName = CI->getNewName();
        auto Module = CI->ModuleName;
        if (CI->rightCommentUnderscored()) {
          Result.insert(NameCorrectionInfo(NewName, NewName, Module));
        }
      }
    }
  }
  std::vector<NameCorrectionInfo> Vec;
  Vec.insert(Vec.end(), Result.begin(), Result.end());
  APIDiffItemStore::serialize(FS, Vec);
  return EC.value();
}

static CheckerOptions getCheckOpts(int argc, char *argv[]) {
  CheckerOptions Opts;
  Opts.AvoidLocation = options::AvoidLocation;
  Opts.AvoidToolArgs = options::AvoidToolArgs;
  Opts.ABI = options::Abi;
  Opts.Migrator = options::Migrator;
  Opts.Verbose = options::Verbose;
  Opts.AbortOnModuleLoadFailure = options::AbortOnModuleLoadFailure;
  Opts.LocationFilter = options::LocationFilter;
  Opts.PrintModule = options::PrintModule;
  Opts.SwiftOnly = options::SwiftOnly;
  Opts.SkipOSCheck = options::DisableOSChecks;
  for (int i = 1; i < argc; ++i)
    Opts.ToolArgs.push_back(argv[i]);

  if (!options::SDK.empty()) {
    auto Ver = getSDKVersion(options::SDK);
    if (!Ver.empty()) {
      Opts.ToolArgs.push_back("-sdk-version");
      Opts.ToolArgs.push_back(Ver);
    }
  }
  return Opts;
}

static SDKNodeRoot *getSDKRoot(const char *Main, SDKContext &Ctx, bool IsBaseline) {
  CompilerInvocation Invok;
  llvm::StringSet<> Modules;
  if (prepareForDump(Main, Invok, Modules, IsBaseline))
    return nullptr;
  return getSDKNodeRoot(Ctx, Invok, Modules);
}

static bool hasBaselineInput() {
  return !options::BaselineModuleInputPaths.empty() ||
    !options::BaselineFrameworkPaths.empty() || !options::BaselineSDK.empty();
}

enum class ComparisonInputMode: uint8_t {
  BothJson,
  BaselineJson,
  BothLoad,
};

static ComparisonInputMode checkComparisonInputMode() {
  if (options::SDKJsonPaths.size() == 2)
    return ComparisonInputMode::BothJson;
  else if (hasBaselineInput())
    return ComparisonInputMode::BothLoad;
  else
    return ComparisonInputMode::BaselineJson;
}

static SDKNodeRoot *getBaselineFromJson(const char *Main, SDKContext &Ctx) {
  SwiftDeclCollector Collector(Ctx);
  // If the baseline path has been given, honor that.
  if (!options::BaselineFilePath.empty()) {
    Collector.deSerialize(options::BaselineFilePath);
    return Collector.getSDKRoot();
  }
  CompilerInvocation Invok;
  llvm::StringSet<> Modules;
  // We need to call prepareForDump to parse target triple.
  if (prepareForDump(Main, Invok, Modules, true))
    return nullptr;

  assert(Modules.size() == 1 &&
         "Cannot find builtin baseline for more than one module");
  // The path of the swift-api-digester executable.
  std::string ExePath = llvm::sys::fs::getMainExecutable(Main,
    reinterpret_cast<void *>(&anchorForGetMainExecutable));
  llvm::SmallString<128> BaselinePath(ExePath);
  llvm::sys::path::remove_filename(BaselinePath); // Remove /swift-api-digester
  llvm::sys::path::remove_filename(BaselinePath); // Remove /bin
  llvm::sys::path::append(BaselinePath, "lib", "swift", "FrameworkABIBaseline");
  if (options::UseEmptyBaseline) {
    // Use the empty baseline for comparison.
    llvm::sys::path::append(BaselinePath, "nil.json");
  } else {
    llvm::sys::path::append(BaselinePath, Modules.begin()->getKey());
    // Look for ABI or API baseline
    if (Ctx.checkingABI())
      llvm::sys::path::append(BaselinePath, "ABI");
    else
      llvm::sys::path::append(BaselinePath, "API");
    // Look for deployment target specific baseline files.
    auto Triple = Invok.getLangOptions().Target;
    if (Triple.isMacCatalystEnvironment())
      llvm::sys::path::append(BaselinePath, "iosmac.json");
    else if (Triple.isMacOSX())
      llvm::sys::path::append(BaselinePath, "macos.json");
    else if (Triple.isiOS())
      llvm::sys::path::append(BaselinePath, "iphoneos.json");
    else if (Triple.isTvOS())
      llvm::sys::path::append(BaselinePath, "appletvos.json");
    else if (Triple.isWatchOS())
      llvm::sys::path::append(BaselinePath, "watchos.json");
    else {
      llvm::errs() << "Unsupported triple target\n";
      exit(1);
    }
  }
  StringRef Path = BaselinePath.str();
  if (!fs::exists(Path)) {
    llvm::errs() << "Baseline at " << Path << " does not exist\n";
    exit(1);
  }
  if (options::Verbose) {
    llvm::errs() << "Using baseline at " << Path << "\n";
  }
  Collector.deSerialize(Path);
  return Collector.getSDKRoot();
}

int main(int argc, char *argv[]) {
  PROGRAM_START(argc, argv);
  INITIALIZE_LLVM();

  llvm::cl::HideUnrelatedOptions(options::Category);
  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift SDK Digester\n");
  CompilerInvocation InitInvok;

  llvm::StringSet<> Modules;
  std::vector<std::string> PrintApis;
  llvm::StringSet<> IgnoredUsrs;
  readIgnoredUsrs(IgnoredUsrs);
  CheckerOptions Opts = getCheckOpts(argc, argv);
  for (auto Name : options::ApisPrintUsrs)
    PrintApis.push_back(Name);
  switch (options::Action) {
  case ActionType::DumpSDK:
    return (prepareForDump(argv[0], InitInvok, Modules)) ? 1 :
      dumpSDKContent(InitInvok, Modules, options::OutputFile, Opts);
  case ActionType::MigratorGen:
  case ActionType::DiagnoseSDKs: {
    ComparisonInputMode Mode = checkComparisonInputMode();
    llvm::StringSet<> protocolWhitelist;
    if (!options::ProtReqWhiteList.empty()) {
      if (readFileLineByLine(options::ProtReqWhiteList, protocolWhitelist))
          return 1;
    }
    if (options::Action == ActionType::MigratorGen) {
      assert(Mode == ComparisonInputMode::BothJson && "Only BothJson mode is supported");
      return generateMigrationScript(options::SDKJsonPaths[0],
                                     options::SDKJsonPaths[1],
                                     options::OutputFile, IgnoredUsrs, Opts);
    }
    switch(Mode) {
    case ComparisonInputMode::BothJson: {
      return diagnoseModuleChange(options::SDKJsonPaths[0],
                                  options::SDKJsonPaths[1],
                                  options::OutputFile, Opts,
                                  std::move(protocolWhitelist));
    }
    case ComparisonInputMode::BaselineJson: {
      SDKContext Ctx(Opts);
      return diagnoseModuleChange(Ctx, getBaselineFromJson(argv[0], Ctx),
                                  getSDKRoot(argv[0], Ctx, false),
                                  options::OutputFile,
                                  std::move(protocolWhitelist));
    }
    case ComparisonInputMode::BothLoad: {
      SDKContext Ctx(Opts);
      return diagnoseModuleChange(Ctx, getSDKRoot(argv[0], Ctx, true),
                                  getSDKRoot(argv[0], Ctx, false),
                                  options::OutputFile,
                                  std::move(protocolWhitelist));
    }
    }
  }
  case ActionType::DeserializeSDK:
  case ActionType::DeserializeDiffItems: {
    if (options::SDKJsonPaths.size() != 1) {
      llvm::cl::PrintHelpMessage();
      return 1;
    }
    if (options::Action == ActionType::DeserializeDiffItems) {
      CompilerInstance CI;
      APIDiffItemStore Store(CI.getDiags());
      return deserializeDiffItems(Store, options::SDKJsonPaths[0],
        options::OutputFile);
    } else {
      return deserializeSDKDump(options::SDKJsonPaths[0], options::OutputFile,
        Opts);
    }
  }
  case ActionType::GenerateNameCorrectionTemplate: {
    CompilerInstance CI;
    APIDiffItemStore Store(CI.getDiags());
    auto &Paths = options::SDKJsonPaths;
    for (unsigned I = 0; I < Paths.size(); I ++)
      Store.addStorePath(Paths[I]);
    return deserializeNameCorrection(Store, options::OutputFile);
  }
  case ActionType::GenerateEmptyBaseline: {
    SDKContext Ctx(Opts);
    dumpSDKRoot(getEmptySDKNodeRoot(Ctx), options::OutputFile);
    return 0;
  }
  case ActionType::FindUsr: {
    if (options::SDKJsonPaths.size() != 1) {
      llvm::cl::PrintHelpMessage();
      return 1;
    }
    return findDeclUsr(options::SDKJsonPaths[0], Opts);
  }
  case ActionType::None:
    llvm::errs() << "Action required\n";
    llvm::cl::PrintHelpMessage();
    return 1;
  }
}
