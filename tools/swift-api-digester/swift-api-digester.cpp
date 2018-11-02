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

#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/ColorUtils.h"
#include "swift/Basic/JSONSerialization.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/Version.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/Utils.h"
#include "swift/IDE/APIDigesterData.h"
#include <functional>

using namespace swift;
using namespace ide;
using namespace api;

namespace  {
  enum class ActionType {
    None,
    DumpSDK,
    DumpSwiftModules,
    CompareSDKs,
    DiagnoseSDKs,
    // The following two are for testing purposes
    DeserializeDiffItems,
    DeserializeSDK,
    GenerateNameCorrectionTemplate,
  };
} // end anonymous namespace

namespace options {

static llvm::cl::opt<bool>
IncludeAllModules("include-all", llvm::cl::desc("Include all modules from the SDK"));

static llvm::cl::list<std::string>
ModuleNames("module", llvm::cl::ZeroOrMore, llvm::cl::desc("Names of modules"));

static llvm::cl::opt<std::string>
ModuleList("module-list-file",
           llvm::cl::desc("File containing a new-line separated list of modules"));

static llvm::cl::opt<std::string>
OutputFile("o", llvm::cl::desc("Output file"));

static llvm::cl::opt<std::string>
SDK("sdk", llvm::cl::desc("path to the SDK to build against"));

static llvm::cl::opt<std::string>
Triple("target", llvm::cl::desc("target triple"));

static llvm::cl::opt<std::string>
ModuleCachePath("module-cache-path", llvm::cl::desc("Clang module cache path"));

static llvm::cl::opt<std::string>
ResourceDir("resource-dir",
            llvm::cl::desc("The directory that holds the compiler resource files"));

static llvm::cl::list<std::string>
FrameworkPaths("F", llvm::cl::desc("add a directory to the framework search path"));

static llvm::cl::list<std::string>
ModuleInputPaths("I", llvm::cl::desc("add a module for input"));

static llvm::cl::list<std::string>
CCSystemFrameworkPaths("iframework",
  llvm::cl::desc("add a directory to the clang importer system framework search path"));

static llvm::cl::opt<bool>
AbortOnModuleLoadFailure("abort-on-module-fail",
                        llvm::cl::desc("Abort if a module failed to load"));

static llvm::cl::opt<bool>
Verbose("v", llvm::cl::desc("Verbose"));

static llvm::cl::opt<bool>
PrintModule("print-module", llvm::cl::desc("Print module names in diagnostics"));

static llvm::cl::opt<ActionType>
Action(llvm::cl::desc("Mode:"), llvm::cl::init(ActionType::None),
      llvm::cl::values(
          clEnumValN(ActionType::DumpSDK,
                     "dump-sdk",
                     "Dump SDK content to JSON file"),
          clEnumValN(ActionType::DumpSwiftModules,
                     "dump-swift",
                     "dump swift modules in SDK"),
          clEnumValN(ActionType::CompareSDKs,
                     "compare-sdk",
                     "Compare SDK content in JSON file"),
          clEnumValN(ActionType::DiagnoseSDKs,
                     "diagnose-sdk",
                     "Diagnose SDK content in JSON file"),
          clEnumValN(ActionType::DeserializeDiffItems,
                     "deserialize-diff",
                     "Deserialize diff items in a JSON file"),
          clEnumValN(ActionType::DeserializeSDK,
                     "deserialize-sdk",
                     "Deserialize sdk digester in a JSON file"),
          clEnumValN(ActionType::GenerateNameCorrectionTemplate,
                     "generate-name-correction",
                     "Generate name correction template")));

static llvm::cl::list<std::string>
SDKJsonPaths("input-paths",
            llvm::cl::desc("The SDK contents under comparison"));

static llvm::cl::list<std::string>
ApisPrintUsrs("api-usrs",
              llvm::cl::desc("The name of APIs to print their usrs, "
                             "e.g. Type::Function"));

static llvm::cl::opt<std::string>
IgnoreRemovedDeclUSRs("ignored-usrs",
                      llvm::cl::desc("the file containing USRs of removed decls "
                                     "that the digester should ignore"));

static llvm::cl::opt<std::string>
SwiftVersion("swift-version",
             llvm::cl::desc("The Swift compiler version to invoke"));

static llvm::cl::opt<bool>
OutputInJson("json", llvm::cl::desc("Print output in JSON format."));

static llvm::cl::opt<bool>
AvoidLocation("avoid-location",
              llvm::cl::desc("Avoid serializing the file paths of SDK nodes."));
} // namespace options

namespace {

template<typename T>
bool contains(std::vector<T*> &container, T *instance) {
  return std::find(container.begin(), container.end(), instance) != container.end();
}

template<typename T>
bool contains(ArrayRef<T> container, T instance) {
  return std::find(container.begin(), container.end(), instance) != container.end();
}

class SDKNode;
typedef SDKNode* NodePtr;
typedef std::map<NodePtr, NodePtr> ParentMap;
typedef std::map<NodePtr, NodePtr> NodeMap;
typedef std::vector<NodePtr> NodeVector;
typedef std::vector<CommonDiffItem> DiffVector;
typedef std::vector<TypeMemberDiffItem> TypeMemberDiffVector;

// The interface used to visit the SDK tree.
class SDKNodeVisitor {
  friend SDKNode;
protected:
  NodeVector Ancestors;
  virtual void visit(NodePtr Node) = 0;

  NodePtr parent() {
    if (Ancestors.empty())
      return nullptr;
    return Ancestors.back();
  }

  int depth() {
    return Ancestors.size() + 1;
  }
public:
  virtual ~SDKNodeVisitor() = default;
};

// During the matching phase, any matched node will be reported using this API.
// For update Node left = {Node before change} Right = {Node after change};
// For added Node left = {NilNode} Right = {Node after change};
// For removed Node left = {Node before change} Right = {NilNode}
struct MatchedNodeListener {
  virtual void foundMatch(NodePtr Left, NodePtr Right) = 0;
  virtual void foundRemoveAddMatch(NodePtr Removed, NodePtr Added) {}
  virtual ~MatchedNodeListener() = default;
};

using NodePairVector = llvm::MapVector<NodePtr, NodePtr>;

// This map keeps track of updated nodes; thus we can conveniently find out what
// is the counterpart of a node before or after being updated.
class UpdatedNodesMap : public MatchedNodeListener {
  NodePairVector MapImpl;
  UpdatedNodesMap(const UpdatedNodesMap& that) = delete;
public:
  UpdatedNodesMap() = default;
  NodePtr findUpdateCounterpart(const SDKNode *Node) const;
  void foundMatch(NodePtr Left, NodePtr Right) override {
    assert(Left && Right && "Not update operation.");
    MapImpl.insert({Left, Right});
  }
};

class SDKContext {
  llvm::StringSet<> TextData;
  llvm::BumpPtrAllocator Allocator;
  UpdatedNodesMap UpdateMap;
  NodeMap TypeAliasUpdateMap;
  TypeMemberDiffVector TypeMemberDiffs;
public:
  llvm::BumpPtrAllocator &allocator() {
    return Allocator;
  }
  StringRef buffer(StringRef Text) {
    return TextData.insert(Text).first->getKey();
  }
  UpdatedNodesMap &getNodeUpdateMap() {
    return UpdateMap;
  }
  NodeMap &getTypeAliasUpdateMap() {
    return TypeAliasUpdateMap;
  }
  TypeMemberDiffVector &getTypeMemberDiffs() {
    return TypeMemberDiffs;
  }
};

// A node matcher will traverse two trees of SDKNode and find matched nodes
struct NodeMatcher {
  virtual void match() = 0;
  virtual ~NodeMatcher() = default;
};

enum class KeyKind {
#define KEY(NAME) KK_##NAME,
#include "swift/IDE/DigesterEnums.def"
};

static KeyKind parseKeyKind(StringRef Content) {
  return llvm::StringSwitch<KeyKind>(Content)
#define KEY(NAME) .Case(#NAME, KeyKind::KK_##NAME)
#include "swift/IDE/DigesterEnums.def"
  ;
}

static StringRef getKeyContent(SDKContext &Ctx, KeyKind Kind) {
  switch (Kind) {
#define KEY(NAME) case KeyKind::KK_##NAME: return Ctx.buffer(#NAME);
#include "swift/IDE/DigesterEnums.def"
  }

  llvm_unreachable("Unhandled KeyKind in switch.");
}

enum class KnownTypeKind: uint8_t {
#define KNOWN_TYPE(NAME) NAME,
#include "swift/IDE/DigesterEnums.def"
  Unknown,
};

enum class KnownProtocolKind: uint8_t {
#define KNOWN_PROTOCOL(NAME) NAME,
#include "swift/IDE/DigesterEnums.def"
};

enum class SDKDeclAttrKind: uint8_t {
#define DECL_ATTR(Name) DAK_##Name,
#include "swift/IDE/DigesterEnums.def"
};

  // Redefine << so that we can output the name of decl kind.
static raw_ostream &operator<<(raw_ostream &Out, const DeclKind Value) {
  switch (Value) {
#define DECL(X, PARENT) case DeclKind::X: return Out << #X;
#include "swift/AST/DeclNodes.def"
  }

  llvm_unreachable("Unhandled DeclKind in switch.");
}

/// We don't dump individual extension declaration in the digester. However,
/// we still want to detect whether an extension's applicability changes. Therefore,
/// by using ParentExtensionInfo, we keep track of extension's information in
/// each member of the extension.
class ParentExtensionInfo {
  friend struct SDKNodeInitInfo;
  friend class SDKNode;
  std::vector<StringRef> Requirements;

  void *operator new(size_t Bytes, SDKContext &C) {
    return C.allocator().Allocate<ParentExtensionInfo>();
  }
public:
  ArrayRef<StringRef> getGenericRequirements() const { return Requirements; }
};

/// The additional information we need to create a type node.
struct TypeInitInfo {
  bool IsImplicitlyUnwrappedOptional = false;
  /// When this type node represents a function parameter, this boolean value
  /// indicates whether the parameter has default argument.
  bool hasDefaultArgument = false;
};

struct SDKNodeInitInfo {
  SDKContext &Ctx;
  StringRef Name;
  StringRef PrintedName;
  DeclKind DKind;
  StringRef USR;
  StringRef Location;
  StringRef ModuleName;
  bool IsThrowing = false;
  bool IsMutating = false;
  bool IsStatic = false;
  Optional<uint8_t> SelfIndex;
  ReferenceOwnership ReferenceOwnership = ReferenceOwnership::Strong;
  std::vector<SDKDeclAttrKind> DeclAttrs;
  std::vector<TypeAttrKind> TypeAttrs;
  std::vector<StringRef> ConformingProtocols;
  StringRef SuperclassUsr;
  StringRef EnumRawTypeName;
  ParentExtensionInfo *ExtInfo = nullptr;
  TypeInitInfo TypeInfo;

  SDKNodeInitInfo(SDKContext &Ctx) : Ctx(Ctx) {}
  SDKNodeInitInfo(SDKContext &Ctx, ValueDecl *VD);
  SDKNodeInitInfo(SDKContext &Ctx, Type Ty, TypeInitInfo Info = TypeInitInfo());
  SDKNode* createSDKNode(SDKNodeKind Kind);
};

class SDKNodeRoot;

class SDKNode {
  typedef std::vector<SDKNode*>::iterator ChildIt;
  SDKContext &Ctx;
  StringRef Name;
  StringRef PrintedName;
  unsigned TheKind : 4;
  NodeVector Children;
  std::set<NodeAnnotation> Annotations;
  std::map<NodeAnnotation, StringRef> AnnotateComments;
  NodePtr Parent = nullptr;

protected:
  SDKNode(SDKNodeInitInfo Info, SDKNodeKind Kind) : Ctx(Info.Ctx), Name(Info.Name),
    PrintedName(Info.PrintedName), TheKind(unsigned(Kind)) {}

public:
  static SDKNode *constructSDKNode(SDKContext &Ctx, llvm::yaml::MappingNode *Node);
  static void preorderVisit(NodePtr Root, SDKNodeVisitor &Visitor);
  static void postorderVisit(NodePtr Root, SDKNodeVisitor &Visitor);

  bool operator==(const SDKNode &Other) const;
  bool operator!=(const SDKNode &Other) const { return !((*this) == Other); }

  ArrayRef<NodeAnnotation>
    getAnnotations(std::vector<NodeAnnotation> &Scratch) const;
  bool isLeaf() const { return Children.empty(); }
  SDKNodeKind getKind() const { return SDKNodeKind(TheKind); }
  StringRef getName() const { return Name; }
  bool isNameValid() const { return Name != "_"; }
  StringRef getPrintedName() const { return PrintedName; }
  void removeChild(ChildIt CI) { Children.erase(CI); }
  ChildIt getChildBegin() { return Children.begin(); }
  void annotate(NodeAnnotation Anno) { Annotations.insert(Anno); }
  void annotate(NodeAnnotation Anno, StringRef Comment);
  NodePtr getParent() const { return Parent; };
  unsigned getChildrenCount() const { return Children.size(); }
  NodePtr childAt(unsigned I) const;
  void removeChild(NodePtr C);
  StringRef getAnnotateComment(NodeAnnotation Anno) const;
  bool isAnnotatedAs(NodeAnnotation Anno) const;
  void addChild(SDKNode *Child);
  ArrayRef<SDKNode*> getChildren() const;
  bool hasSameChildren(const SDKNode &Other) const;
  unsigned getChildIndex(NodePtr Child) const;
  SDKNode* getOnlyChild() const;
  SDKContext &getSDKContext() const { return Ctx; }
  SDKNodeRoot *getRootNode() const;
  template <typename T> const T *getAs() const;
  template <typename T> T *getAs();
};

class SDKNodeDecl : public SDKNode {
  DeclKind DKind;
  StringRef Usr;
  StringRef Location;
  StringRef ModuleName;
  std::vector<SDKDeclAttrKind> DeclAttributes;
  bool IsStatic;
  uint8_t ReferenceOwnership;
  bool hasDeclAttribute(SDKDeclAttrKind DAKind) const;
  // Non-null ExtInfo implies this decl is defined in an type extension.
  ParentExtensionInfo *ExtInfo;

protected:
  SDKNodeDecl(SDKNodeInitInfo Info, SDKNodeKind Kind)
      : SDKNode(Info, Kind), DKind(Info.DKind), Usr(Info.USR),
        Location(Info.Location), ModuleName(Info.ModuleName),
        DeclAttributes(Info.DeclAttrs), IsStatic(Info.IsStatic),
        ReferenceOwnership(uint8_t(Info.ReferenceOwnership)),
        ExtInfo(Info.ExtInfo) {}

public:
  StringRef getUsr() const { return Usr; }
  StringRef getLocation() const { return Location; }
  StringRef getModuleName() const {return ModuleName;}
  StringRef getHeaderName() const;
  void addDeclAttribute(SDKDeclAttrKind DAKind);
  ArrayRef<SDKDeclAttrKind> getDeclAttributes() const;
  swift::ReferenceOwnership getReferenceOwnership() const {
    return swift::ReferenceOwnership(ReferenceOwnership);
  }
  bool isObjc() const { return Usr.startswith("c:"); }
  static bool classof(const SDKNode *N);
  DeclKind getDeclKind() const { return DKind; }
  void printFullyQualifiedName(llvm::raw_ostream &OS) const;
  StringRef getFullyQualifiedName() const;
  bool isSDKPrivate() const;
  bool isDeprecated() const;
  bool hasFixedLayout() const;
  bool isStatic() const { return IsStatic; };
  bool isFromExtension() const { return ExtInfo; }
  const ParentExtensionInfo& getExtensionInfo() const {
    assert(isFromExtension());
    return *ExtInfo;
  }
};

StringRef SDKNodeDecl::getHeaderName() const {
  if (Location.empty())
    return StringRef();
  return llvm::sys::path::filename(Location.split(":").first);
}

class SDKNodeRoot :public SDKNode {
  /// This keeps track of all decl descendants with USRs.
  llvm::StringMap<llvm::SmallSetVector<SDKNodeDecl*, 2>> DescendantDeclTable;

public:
  SDKNodeRoot(SDKNodeInitInfo Info) : SDKNode(Info, SDKNodeKind::Root) {}
  static SDKNode *getInstance(SDKContext &Ctx);
  static bool classof(const SDKNode *N);
  void registerDescendant(SDKNode *D) {
    if (auto DD = dyn_cast<SDKNodeDecl>(D)) {
      assert(!DD->getUsr().empty());
      DescendantDeclTable[DD->getUsr()].insert(DD);
    }
  }
  ArrayRef<SDKNodeDecl*> getDescendantsByUsr(StringRef Usr) {
    return DescendantDeclTable[Usr].getArrayRef();
  }
};

NodePtr UpdatedNodesMap::findUpdateCounterpart(const SDKNode *Node) const {
  assert(Node->isAnnotatedAs(NodeAnnotation::Updated) && "Not update operation.");
  auto FoundPair = std::find_if(MapImpl.begin(), MapImpl.end(),
                      [&](std::pair<NodePtr, NodePtr> Pair) {
    return Pair.second == Node || Pair.first == Node;
  });
  assert(FoundPair != MapImpl.end() && "Cannot find update counterpart.");
  return Node == FoundPair->first ? FoundPair->second : FoundPair->first;
}

class SDKNodeType : public SDKNode {
  std::vector<TypeAttrKind> TypeAttributes;
  bool HasDefaultArg;

protected:
  bool hasTypeAttribute(TypeAttrKind DAKind) const;
  SDKNodeType(SDKNodeInitInfo Info, SDKNodeKind Kind) : SDKNode(Info, Kind),
    TypeAttributes(Info.TypeAttrs),
    HasDefaultArg(Info.TypeInfo.hasDefaultArgument) {}

public:
  KnownTypeKind getTypeKind() const;
  void addTypeAttribute(TypeAttrKind AttrKind);
  ArrayRef<TypeAttrKind> getTypeAttributes() const;
  SDKNodeDecl *getClosestParentDecl() const;

  // When the type node represents a function parameter, this function returns
  // whether the parameter has a default value.
  bool hasDefaultArgument() const { return HasDefaultArg; }
  bool isTopLevelType() const { return !isa<SDKNodeType>(getParent()); }
  static bool classof(const SDKNode *N);
};

bool SDKNodeType::classof(const SDKNode *N) {
  switch (N->getKind()) {
  case SDKNodeKind::TypeNominal:
  case SDKNodeKind::TypeFunc:
  case SDKNodeKind::TypeAlias:
    return true;
  default:
    return false;
  }
}

class SDKNodeTypeNominal : public SDKNodeType {
  StringRef USR;
public:
  SDKNodeTypeNominal(SDKNodeInitInfo Info) : SDKNodeType(Info,
    SDKNodeKind::TypeNominal), USR(Info.USR) {}
  // Get the usr of the correspoding nominal type decl.
  StringRef getUsr() const { return USR; }
  static bool classof(const SDKNode *N);
};

class SDKNodeTypeFunc : public SDKNodeType {
public:
  SDKNodeTypeFunc(SDKNodeInitInfo Info) : SDKNodeType(Info, SDKNodeKind::TypeFunc) {}
  bool isEscaping() const { return !hasTypeAttribute(TypeAttrKind::TAK_noescape); }
  static bool classof(const SDKNode *N);
};

class SDKNodeTypeAlias : public SDKNodeType {
public:
  SDKNodeTypeAlias(SDKNodeInitInfo Info):
    SDKNodeType(Info, SDKNodeKind::TypeAlias) {}
  const SDKNodeType *getUnderlyingType() const {
    return getOnlyChild()->getAs<SDKNodeType>();
  }
  static bool classof(const SDKNode *N);
};

template <typename T> const T *
SDKNode::getAs() const {
  if (T::classof(this))
    return static_cast<const T*>(this);
  llvm_unreachable("incompatible types");
}

template <typename T> T *
SDKNode::getAs() {
  if (T::classof(this))
    return static_cast<T*>(this);
  llvm_unreachable("incompatible types");
}

unsigned SDKNode::getChildIndex(NodePtr Child) const {
  return std::find(Children.begin(), Children.end(), Child) - Children.begin();
}

SDKNode* SDKNode::getOnlyChild() const {
  assert(Children.size() == 1 && "more that one child.");
  return *Children.begin();
}

SDKNodeRoot *SDKNode::getRootNode() const {
  for (auto *Root = const_cast<SDKNode*>(this); ; Root = Root->getParent()) {
    if (auto Result = dyn_cast<SDKNodeRoot>(Root))
      return Result;
  }
  llvm_unreachable("Unhandled SDKNodeKind in switch.");
}

void SDKNode::addChild(SDKNode *Child) {
  Child->Parent = this;
  Children.push_back(Child);
  if (auto *Root = dyn_cast<SDKNodeRoot>(this)) {
    struct DeclCollector: public SDKNodeVisitor {
      SDKNodeRoot &Root;
      DeclCollector(SDKNodeRoot &Root): Root(Root) {}
      void visit(NodePtr Node) override {
        Root.registerDescendant(Node);
      }
    } Collector(*Root);
    SDKNode::preorderVisit(Child, Collector);
  }
}

ArrayRef<SDKNode*> SDKNode::getChildren() const {
  return llvm::makeArrayRef(Children);
}

NodePtr SDKNode::childAt(unsigned I) const {
  assert(I < getChildrenCount());
  return getChildren()[I];
}

void SDKNode::removeChild(NodePtr C) {
  Children.erase(std::find(Children.begin(), Children.end(), C));
}

void SDKNode::annotate(NodeAnnotation Anno, StringRef Comment) {
  assert(!isAnnotatedAs(Anno) && "already annotated");
  annotate(Anno);
  AnnotateComments[Anno] = Comment;
}

StringRef SDKNode::getAnnotateComment(NodeAnnotation Anno) const {
  return AnnotateComments.find(Anno)->second;
}

ArrayRef<NodeAnnotation> SDKNode::
getAnnotations(std::vector<NodeAnnotation> &Scratch) const {
  for (auto Ann : Annotations)
    Scratch.push_back(Ann);
  return llvm::makeArrayRef(Scratch);
}

bool SDKNode::isAnnotatedAs(NodeAnnotation Anno) const {
  return Annotations.find(Anno) != Annotations.end();;
}

void SDKNode::preorderVisit(NodePtr Root, SDKNodeVisitor &Visitor) {
  Visitor.visit(Root);
  Visitor.Ancestors.push_back(Root);
  for (auto Child : Root->Children)
    preorderVisit(Child, Visitor);
  Visitor.Ancestors.pop_back();
}

void SDKNode::postorderVisit(NodePtr Root, SDKNodeVisitor &Visitor) {
  Visitor.Ancestors.push_back(Root);
  for (auto Child : Root->Children)
    postorderVisit(Child, Visitor);
  Visitor.Ancestors.pop_back();
  Visitor.visit(Root);
}

class SDKNodeVectorViewer {
  ArrayRef<SDKNode*> Collection;
  llvm::function_ref<bool(NodePtr)> Selector;
  typedef ArrayRef<SDKNode*>::iterator VectorIt;
  VectorIt getNext(VectorIt Start);
  class ViewerIterator;

public:
  SDKNodeVectorViewer(ArrayRef<SDKNode*> Collection,
                      llvm::function_ref<bool(NodePtr)> Selector) :
                        Collection(Collection),
                        Selector(Selector) {}
  ViewerIterator begin();
  ViewerIterator end();
};

class SDKNodeVectorViewer::ViewerIterator :
    public std::iterator<std::input_iterator_tag, VectorIt> {
  SDKNodeVectorViewer &Viewer;
  VectorIt P;
public:
  ViewerIterator(SDKNodeVectorViewer &Viewer, VectorIt P) : Viewer(Viewer), P(P) {}
  ViewerIterator(const ViewerIterator& mit) : Viewer(mit.Viewer), P(mit.P) {}
  ViewerIterator& operator++();
  ViewerIterator operator++(int) {ViewerIterator tmp(*this); operator++(); return tmp;}
  bool operator==(const ViewerIterator& rhs) {return P==rhs.P;}
  bool operator!=(const ViewerIterator& rhs) {return P!=rhs.P;}
  const NodePtr& operator*() {return *P;}
};

SDKNodeVectorViewer::VectorIt
SDKNodeVectorViewer::getNext(VectorIt Start) {
  for (auto It = Start; It != Collection.end(); ++ It)
    if (Selector(*It))
      return It;
  return Collection.end();
}

SDKNodeVectorViewer::ViewerIterator&
SDKNodeVectorViewer::ViewerIterator::operator++() {
  P = Viewer.getNext(P + 1);
  return *this;
}

SDKNodeVectorViewer::ViewerIterator SDKNodeVectorViewer::begin() {
  return ViewerIterator(*this, getNext(Collection.begin()));
}

SDKNodeVectorViewer::ViewerIterator SDKNodeVectorViewer::end() {
  return ViewerIterator(*this, Collection.end());
}

class SDKNodeDecl;

KnownTypeKind SDKNodeType::getTypeKind() const {
#define KNOWN_TYPE(NAME) if (getName() == #NAME) return KnownTypeKind::NAME;
#include "swift/IDE/DigesterEnums.def"
  return KnownTypeKind::Unknown;
}

ArrayRef<TypeAttrKind> SDKNodeType::getTypeAttributes() const {
  return llvm::makeArrayRef(TypeAttributes.data(), TypeAttributes.size());
}

void SDKNodeType::addTypeAttribute(TypeAttrKind AttrKind) {
  TypeAttributes.push_back(AttrKind);
}

bool SDKNodeType::hasTypeAttribute(TypeAttrKind DAKind) const {
  return std::find(TypeAttributes.begin(), TypeAttributes.end(), DAKind) !=
    TypeAttributes.end();
}

SDKNode *SDKNodeRoot::getInstance(SDKContext &Ctx) {
  SDKNodeInitInfo Info(Ctx);
  Info.Name = Ctx.buffer("TopLevel");
  Info.PrintedName = Ctx.buffer("TopLevel");
  return Info.createSDKNode(SDKNodeKind::Root);
}

bool SDKNodeDecl::isDeprecated() const {
  return hasDeclAttribute(SDKDeclAttrKind::DAK_deprecated);
}

bool SDKNodeDecl::hasFixedLayout() const {
  return hasDeclAttribute(SDKDeclAttrKind::DAK_fixedLayout);
}

bool SDKNodeDecl::isSDKPrivate() const {
  if (getName().startswith("__"))
    return true;
  if (auto *PD = dyn_cast<SDKNodeDecl>(getParent()))
    return PD->isSDKPrivate();
  return false;
}

void SDKNodeDecl::printFullyQualifiedName(llvm::raw_ostream &OS) const {
  std::vector<NodePtr> Parent;
  for (auto *P = getParent(); isa<SDKNodeDecl>(P); P = P->getParent())
    Parent.push_back(P);
  for (auto It = Parent.rbegin(); It != Parent.rend(); ++ It)
    OS << (*It)->getPrintedName() << ".";
  OS << getPrintedName();
}

StringRef SDKNodeDecl::getFullyQualifiedName() const {
  llvm::SmallString<32> Buffer;
  llvm::raw_svector_ostream OS(Buffer);
  printFullyQualifiedName(OS);
  return getSDKContext().buffer(OS.str());
}

bool SDKNodeDecl::classof(const SDKNode *N) {
  switch (N->getKind()) {
    case SDKNodeKind::DeclConstructor:
    case SDKNodeKind::DeclFunction:
    case SDKNodeKind::DeclGetter:
    case SDKNodeKind::DeclSetter:
    case SDKNodeKind::DeclTypeAlias:
    case SDKNodeKind::DeclType:
    case SDKNodeKind::DeclVar:
      return true;
    case SDKNodeKind::Root:
    case SDKNodeKind::TypeNominal:
    case SDKNodeKind::TypeFunc:
    case SDKNodeKind::TypeAlias:
      return false;
  }

  llvm_unreachable("Unhandled SDKNodeKind in switch.");
}

void SDKNodeDecl::addDeclAttribute(SDKDeclAttrKind DAKind) {
  DeclAttributes.push_back(DAKind);
}

bool SDKNodeDecl::hasDeclAttribute(SDKDeclAttrKind DAKind) const {
  return std::find(DeclAttributes.begin(), DeclAttributes.end(), DAKind) !=
    DeclAttributes.end();
}

ArrayRef<SDKDeclAttrKind> SDKNodeDecl::getDeclAttributes() const {
  return llvm::makeArrayRef(DeclAttributes.data(), DeclAttributes.size());
}

SDKNodeDecl *SDKNodeType::getClosestParentDecl() const {
  auto *Result = getParent();
  for (; !isa<SDKNodeDecl>(Result); Result = Result->getParent());
  return Result->getAs<SDKNodeDecl>();
}

class SDKNodeDeclType : public SDKNodeDecl {
  StringRef SuperclassUsr;
  std::vector<StringRef> ConformingProtocols;
  StringRef EnumRawTypeName;
public:
  SDKNodeDeclType(SDKNodeInitInfo Info) : SDKNodeDecl(Info, SDKNodeKind::DeclType),
                                          SuperclassUsr(Info.SuperclassUsr),
                                ConformingProtocols(Info.ConformingProtocols),
                                        EnumRawTypeName(Info.EnumRawTypeName) {}
  static bool classof(const SDKNode *N);
  StringRef getSuperClassUsr() const { return SuperclassUsr; }
  ArrayRef<StringRef> getAllProtocols() const { return ConformingProtocols; }

#define NOMINAL_TYPE_DECL(ID, PARENT) \
  bool is##ID() const { return getDeclKind() == DeclKind::ID; }
#define DECL(ID, PARENT)
#include "swift/AST/DeclNodes.def"

  StringRef getEnumRawTypeName() const {
    assert(isEnum());
    return EnumRawTypeName;
  }

  Optional<SDKNodeDeclType*> getSuperclass() const {
    if (SuperclassUsr.empty())
      return None;
    auto Descendants = getRootNode()->getDescendantsByUsr(SuperclassUsr);
    if (!Descendants.empty()) {
      return Descendants.front()->getAs<SDKNodeDeclType>();
    }
    return None;
  }

  /// Finding the node through all children, including the inheritted ones,
  /// whose printed name matches with the given name.
  Optional<SDKNodeDecl*> lookupChildByPrintedName(StringRef Name) const {
    for (auto C : getChildren()) {
      if (C->getPrintedName() == Name)
        return C->getAs<SDKNodeDecl>();
    }
    // Finding from the inheritance chain.
    if (auto Super = getSuperclass()) {
      return (*Super)->lookupChildByPrintedName(Name);
    }
    return None;
  }

  SDKNodeType *getRawValueType() const {
    if (isConformingTo(KnownProtocolKind::RawRepresentable)) {
      if (auto RV = lookupChildByPrintedName("rawValue")) {
        return (*(*RV)->getChildBegin())->getAs<SDKNodeType>();
      }
    }
    return nullptr;
  }

  bool isConformingTo(KnownProtocolKind Kind) const {
    StringRef Usr;
    switch (Kind) {
#define KNOWN_PROTOCOL(NAME)                                                  \
      case KnownProtocolKind::NAME:                                           \
        return std::find(ConformingProtocols.begin(),                         \
                         ConformingProtocols.end(),                           \
                         #NAME) != ConformingProtocols.end();
#include "swift/IDE/DigesterEnums.def"
    }
  }
};

class SDKNodeDeclTypeAlias : public SDKNodeDecl {
public:
  SDKNodeDeclTypeAlias(SDKNodeInitInfo Info) : SDKNodeDecl(Info,
                                                       SDKNodeKind::DeclTypeAlias) {}
  const SDKNodeType* getUnderlyingType() const {
    return getOnlyChild()->getAs<SDKNodeType>();
  }
  static bool classof(const SDKNode *N);
};

class SDKNodeDeclVar : public SDKNodeDecl {
public:
  SDKNodeDeclVar(SDKNodeInitInfo Info) : SDKNodeDecl(Info, SDKNodeKind::DeclVar) {}
  static bool classof(const SDKNode *N);
};

class SDKNodeDeclAbstractFunc : public SDKNodeDecl {
  const bool IsThrowing;
  const bool IsMutating;
  const Optional<uint8_t> SelfIndex;

protected:
  SDKNodeDeclAbstractFunc(SDKNodeInitInfo Info, SDKNodeKind Kind) :
                                                    SDKNodeDecl(Info, Kind),
                                                    IsThrowing(Info.IsThrowing),
                                                    IsMutating(Info.IsMutating),
                                                    SelfIndex(Info.SelfIndex){}
public:
  bool isThrowing() const { return IsThrowing; }
  bool isMutating() const { return IsMutating; }
  uint8_t getSelfIndex() const { return SelfIndex.getValue(); }
  Optional<uint8_t> getSelfIndexOptional() const { return SelfIndex; }
  bool hasSelfIndex() const { return SelfIndex.hasValue(); }
  static bool classof(const SDKNode *N);
  static StringRef getTypeRoleDescription(SDKContext &Ctx, unsigned Index);
};

bool SDKNodeDeclAbstractFunc::classof(const SDKNode *N) {
  switch (N->getKind()) {
    case SDKNodeKind::DeclFunction:
    case SDKNodeKind::DeclSetter:
    case SDKNodeKind::DeclGetter:
    case SDKNodeKind::DeclConstructor:
      return true;

    default:
      return false;
  }
}

class SDKNodeDeclFunction : public SDKNodeDeclAbstractFunc {
public:
  SDKNodeDeclFunction(SDKNodeInitInfo Info) : SDKNodeDeclAbstractFunc(Info,
                                                       SDKNodeKind::DeclFunction) {}
  SDKNode *getReturnType() { return *getChildBegin(); }
  static bool classof(const SDKNode *N);
};

StringRef SDKNodeDeclAbstractFunc::getTypeRoleDescription(SDKContext &Ctx,
                                                      unsigned Index) {
  if (Index == 0) {
    return Ctx.buffer("return");
  } else {
    llvm::SmallString<4> Buffer;
    Buffer += "parameter ";
    Buffer += std::to_string(Index - 1);
    return Ctx.buffer(Buffer.str());
  }
}

class SDKNodeDeclConstructor : public SDKNodeDeclAbstractFunc {
public:
  SDKNodeDeclConstructor(SDKNodeInitInfo Info) : SDKNodeDeclAbstractFunc(Info,
                                                    SDKNodeKind::DeclConstructor) {}
  static bool classof(const SDKNode *N);
};

class SDKNodeDeclGetter : public SDKNodeDeclAbstractFunc {
public:
  SDKNodeDeclGetter(SDKNodeInitInfo Info) : SDKNodeDeclAbstractFunc(Info,
                                                         SDKNodeKind::DeclGetter) {}
  static bool classof(const SDKNode *N);
};

class SDKNodeDeclSetter : public SDKNodeDeclAbstractFunc {
public:
  SDKNodeDeclSetter(SDKNodeInitInfo Info) : SDKNodeDeclAbstractFunc(Info,
                                                         SDKNodeKind::DeclSetter) {}
  static bool classof(const SDKNode *N);
};

#define NODE_KIND(X, NAME)                                                 \
  bool SDKNode##X::classof(const SDKNode *N) {                             \
    return N->getKind() == SDKNodeKind::X;                                 \
  }
#include "swift/IDE/DigesterEnums.def"


SDKNode* SDKNode::constructSDKNode(SDKContext &Ctx,
                                   llvm::yaml::MappingNode *Node) {
  static auto GetScalarString = [&](llvm::yaml::Node *N) -> StringRef {
    auto WithQuote = cast<llvm::yaml::ScalarNode>(N)->getRawValue();
    return WithQuote.substr(1, WithQuote.size() - 2);
  };

  static auto getAsInt = [&](llvm::yaml::Node *N) -> int {
    return std::stoi(cast<llvm::yaml::ScalarNode>(N)->getRawValue());
  };
  SDKNodeKind Kind;
  SDKNodeInitInfo Info(Ctx);
  NodeVector Children;

  for (auto &Pair : *Node) {
    switch(parseKeyKind(GetScalarString(Pair.getKey()))) {
    case KeyKind::KK_kind:
      Kind = parseSDKNodeKind(GetScalarString(Pair.getValue()));
      break;
    case KeyKind::KK_name:
      Info.Name = GetScalarString(Pair.getValue());
      break;
    case KeyKind::KK_selfIndex:
      Info.SelfIndex = getAsInt(Pair.getValue());
      break;
    case KeyKind::KK_usr:
      Info.USR = GetScalarString(Pair.getValue());
      break;

    case KeyKind::KK_location:
      Info.Location = GetScalarString(Pair.getValue());
      break;
    case KeyKind::KK_children:
      for (auto &Mapping : *cast<llvm::yaml::SequenceNode>(Pair.getValue())) {
        Children.push_back(constructSDKNode(Ctx,
                                      cast<llvm::yaml::MappingNode>(&Mapping)));
      }
      break;
    case KeyKind::KK_parentExtensionReqs: {
      assert(!Info.ExtInfo);
      Info.ExtInfo = new (Ctx) ParentExtensionInfo();
      for (auto &Req : *cast<llvm::yaml::SequenceNode>(Pair.getValue())) {
        Info.ExtInfo->Requirements.push_back(GetScalarString(&Req));
      }
      break;
    }
    case KeyKind::KK_conformingProtocols: {
      assert(Info.ConformingProtocols.empty());
      for (auto &Name : *cast<llvm::yaml::SequenceNode>(Pair.getValue())) {
        Info.ConformingProtocols.push_back(GetScalarString(&Name));
      }
      break;
    }
    case KeyKind::KK_enumRawTypeName: {
      assert(Info.DKind == DeclKind::Enum);
      Info.EnumRawTypeName = GetScalarString(Pair.getValue());
      break;
    }
    case KeyKind::KK_printedName:
      Info.PrintedName = GetScalarString(Pair.getValue());
      break;
    case KeyKind::KK_moduleName:
      Info.ModuleName = GetScalarString(Pair.getValue());
      break;
    case KeyKind::KK_superclassUsr:
      Info.SuperclassUsr = GetScalarString(Pair.getValue());
      break;
    case KeyKind::KK_throwing:
      Info.IsThrowing = true;
      break;
    case KeyKind::KK_mutating:
      Info.IsMutating = true;
      break;
    case KeyKind::KK_hasDefaultArg:
      Info.TypeInfo.hasDefaultArgument = true;
      break;
    case KeyKind::KK_static:
      Info.IsStatic = true;
      break;
    case KeyKind::KK_ownership:
      Info.ReferenceOwnership =
          swift::ReferenceOwnership(getAsInt(Pair.getValue()));
      assert(Info.ReferenceOwnership != swift::ReferenceOwnership::Strong &&
             "Strong is implied.");
      break;

    case KeyKind::KK_typeAttributes: {
      auto *Seq = cast<llvm::yaml::SequenceNode>(Pair.getValue());
      for (auto It = Seq->begin(); It != Seq->end(); ++ It) {
        Info.TypeAttrs.push_back(
          llvm::StringSwitch<TypeAttrKind>(GetScalarString(&*It))
#define TYPE_ATTR(X) .Case(#X, TypeAttrKind::TAK_##X)
#include "swift/AST/Attr.def"
          .Case("Count", TypeAttrKind::TAK_Count));
      }
      break;
    }
    case KeyKind::KK_declAttributes: {
      auto *Seq = cast<llvm::yaml::SequenceNode>(Pair.getValue());
      for (auto It = Seq->begin(); It != Seq->end(); ++ It) {
        Info.DeclAttrs.push_back(
          llvm::StringSwitch<SDKDeclAttrKind>(GetScalarString(&*It))
#define DECL_ATTR(X) .Case(#X, SDKDeclAttrKind::DAK_##X)
#include "swift/IDE/DigesterEnums.def"
          );
      }
      break;
    }
    case KeyKind::KK_declKind:
      Info.DKind = llvm::StringSwitch<DeclKind>(GetScalarString(Pair.getValue()))
#define DECL(X, PARENT) .Case(#X, DeclKind::X)
#include "swift/AST/DeclNodes.def"
      ;
      break;
    }
  };
  SDKNode *Result = Info.createSDKNode(Kind);
  for (auto C : Children) {
    Result->addChild(C);
  }
  return Result;
}

bool SDKNode::hasSameChildren(const SDKNode &Other) const {
  if (Children.size() != Other.Children.size())
    return false;
  for (unsigned I = 0; I < Children.size(); ++ I) {
    if (*Children[I] != *Other.Children[I])
      return false;
  }
  return true;
}

bool SDKNode::operator==(const SDKNode &Other) const {
  auto *LeftAlias = dyn_cast<SDKNodeTypeAlias>(this);
  auto *RightAlias = dyn_cast<SDKNodeTypeAlias>(&Other);
  if (LeftAlias || RightAlias) {
    // Comparing the underlying types if any of the inputs are alias.
    const SDKNode *Left = LeftAlias ? LeftAlias->getUnderlyingType() : this;
    const SDKNode *Right = RightAlias ? RightAlias->getUnderlyingType() : &Other;
    return *Left == *Right;
  }

  if (getKind() != Other.getKind())
    return false;

  switch(getKind()) {
    case SDKNodeKind::TypeAlias:
      llvm_unreachable("Should be handled above.");
    case SDKNodeKind::TypeNominal:
    case SDKNodeKind::TypeFunc: {
      auto Left = this->getAs<SDKNodeType>();
      auto Right = (&Other)->getAs<SDKNodeType>();
      if (!Left->getTypeAttributes().equals(Right->getTypeAttributes()))
        return false;
      if (Left->getPrintedName() == Right->getPrintedName())
        return true;
      return Left->getName() == Right->getName() &&
        Left->hasSameChildren(*Right);
    }

    case SDKNodeKind::DeclFunction:
    case SDKNodeKind::DeclConstructor:
    case SDKNodeKind::DeclGetter:
    case SDKNodeKind::DeclSetter: {
      auto Left = this->getAs<SDKNodeDeclAbstractFunc>();
      auto Right = (&Other)->getAs<SDKNodeDeclAbstractFunc>();
      if (Left->isMutating() ^ Right->isMutating())
        return false;
      if (Left->isThrowing() ^ Right->isThrowing())
        return false;
      LLVM_FALLTHROUGH;
    }
    case SDKNodeKind::DeclType:
    case SDKNodeKind::DeclVar:
    case SDKNodeKind::DeclTypeAlias: {
      auto Left = this->getAs<SDKNodeDecl>();
      auto Right = (&Other)->getAs<SDKNodeDecl>();
      if (Left->isStatic() ^ Right->isStatic())
        return false;
      if (Left->getReferenceOwnership() != Right->getReferenceOwnership())
        return false;
      LLVM_FALLTHROUGH;
    }
    case SDKNodeKind::Root: {
      return getPrintedName() == Other.getPrintedName() &&
        hasSameChildren(Other);
    }
  }

  llvm_unreachable("Unhanlded SDKNodeKind in switch.");
}

// The pretty printer of a tree of SDKNode
class SDKNodeDumpVisitor : public SDKNodeVisitor {
  void dumpSpace(int Num) {
    while (Num != 0) {
      llvm::outs() << "\t";
      Num --;
    }
  }
  void visit(NodePtr Node) override {
    dumpSpace(depth());
    llvm::outs() << "[" << Node->getKind() << "]" << Node->getName() << "\n";
  }
public:
  SDKNodeDumpVisitor() {};
};

static StringRef getPrintedName(SDKContext &Ctx, Type Ty,
                                bool IsImplicitlyUnwrappedOptional) {
  std::string S;
  llvm::raw_string_ostream OS(S);
  PrintOptions PO;
  PO.SkipAttributes = true;
  if (IsImplicitlyUnwrappedOptional)
    PO.PrintOptionalAsImplicitlyUnwrapped = true;

  Ty.print(OS, PO);
  return Ctx.buffer(OS.str());
}

static StringRef getTypeName(SDKContext &Ctx, Type Ty,
                             bool IsImplicitlyUnwrappedOptional) {
  if (Ty->getKind() == TypeKind::Paren) {
    return Ctx.buffer("Paren");
  }
  if (Ty->isVoid()) {
    return Ctx.buffer("Void");
  }
  if (auto *NAT = dyn_cast<NameAliasType>(Ty.getPointer())) {
    return NAT->getDecl()->getNameStr();
  }
  if (Ty->getAnyNominal()) {
    if (IsImplicitlyUnwrappedOptional) {
      assert(Ty->getOptionalObjectType());
      return StringRef("ImplicitlyUnwrappedOptional");
    }
    return Ty->getAnyNominal()->getNameStr();
  }
#define TYPE(id, parent)                                                      \
  if (Ty->getKind() == TypeKind::id) {                                        \
    return Ctx.buffer(#id);                                                   \
  }
#include "swift/AST/TypeNodes.def"
  llvm_unreachable("Unhandled type name.");
}

static StringRef calculateUsr(SDKContext &Ctx, ValueDecl *VD) {
  llvm::SmallString<64> SS;
  llvm::raw_svector_ostream OS(SS);
  if (!ide::printDeclUSR(VD, OS)) {
    return Ctx.buffer(SS.str());
  }
  return StringRef();
}

static StringRef calculateLocation(SDKContext &SDKCtx, ValueDecl *VD) {
  if (options::AvoidLocation) {
    return StringRef();
  }
  auto &Ctx = VD->getASTContext();
  auto &Importer = static_cast<ClangImporter &>(*Ctx.getClangModuleLoader());

  clang::SourceManager &SM = Importer.getClangPreprocessor().getSourceManager();
  if (ClangNode CN = VD->getClangNode()) {
    clang::SourceLocation Loc = CN.getLocation();
    Loc = SM.getFileLoc(Loc);
    if (Loc.isValid())
      return SDKCtx.buffer(Loc.printToString(SM));
  }

  return StringRef();
}

static bool isFunctionTypeNoEscape(Type Ty) {
  if (auto *AFT = Ty->getAs<AnyFunctionType>()) {
    return AFT->getExtInfo().isNoEscape();
  }
  return false;
}

/// Converts a DeclBaseName to a string by assigning special names strings and
/// escaping identifiers that would clash with these strings using '`'
static StringRef getEscapedName(DeclBaseName name) {
  switch (name.getKind()) {
  case DeclBaseName::Kind::Subscript:
    return "subscript";
  case DeclBaseName::Kind::Constructor:
    return "init";
  case DeclBaseName::Kind::Destructor:
    return "deinit";
  case DeclBaseName::Kind::Normal:
    return llvm::StringSwitch<StringRef>(name.getIdentifier().str())
        .Case("subscript", "`subscript`")
        .Case("init", "`init`")
        .Case("deinit", "`deinit`")
        .Default(name.getIdentifier().str());
  }
}

static StringRef getPrintedName(SDKContext &Ctx, ValueDecl *VD) {
  llvm::SmallString<32> Result;
  if (auto FD = dyn_cast<AbstractFunctionDecl>(VD)) {
    auto DM = FD->getFullName();

    if (DM.getBaseName().empty()) {
      Result.append("_");
    } else {
      Result.append(getEscapedName(DM.getBaseName()));
    }

    Result.append("(");
    for (auto Arg : DM.getArgumentNames()) {
      Result.append(Arg.empty() ? "_" : Arg.str());
      Result.append(":");
    }
    Result.append(")");
    return Ctx.buffer(Result.str());
  }
  auto DM = VD->getFullName();
  Result.append(getEscapedName(DM.getBaseName()));
  return Ctx.buffer(Result.str());
}

static bool isFuncThrowing(ValueDecl *VD) {
  if (auto AF = dyn_cast<AbstractFunctionDecl>(VD)) {
    return AF->hasThrows();
  }
  return false;
}

static bool isFuncMutating(ValueDecl *VD) {
  if (auto AF = dyn_cast<FuncDecl>(VD)) {
    return AF->isMutating();
  }
  return false;
}

static Optional<uint8_t> getSelfIndex(ValueDecl *VD) {
  if (auto AF = dyn_cast<AbstractFunctionDecl>(VD)) {
    if (AF->isImportAsInstanceMember())
      return AF->getSelfIndex();
  }
  return None;
}

static ReferenceOwnership getReferenceOwnership(ValueDecl *VD) {
  if (auto OA = VD->getAttrs().getAttribute<ReferenceOwnershipAttr>()) {
    return OA->get();
  }
  return ReferenceOwnership::Strong;
}

SDKNodeInitInfo::SDKNodeInitInfo(SDKContext &Ctx, Type Ty,
                                 TypeInitInfo TypeInfo) :
    Ctx(Ctx), Name(getTypeName(Ctx, Ty, TypeInfo.IsImplicitlyUnwrappedOptional)),
    PrintedName(getPrintedName(Ctx, Ty, TypeInfo.IsImplicitlyUnwrappedOptional)),
    TypeInfo(TypeInfo) {
  if (isFunctionTypeNoEscape(Ty))
    TypeAttrs.push_back(TypeAttrKind::TAK_noescape);
  // If this is a nominal type, get its Usr.
  if (auto *ND = Ty->getAnyNominal()) {
    USR = calculateUsr(Ctx, ND);
  }
}

SDKNodeInitInfo::SDKNodeInitInfo(SDKContext &Ctx, ValueDecl *VD)
    : Ctx(Ctx),
      Name(VD->hasName() ? getEscapedName(VD->getBaseName()) : Ctx.buffer("_")),
      PrintedName(getPrintedName(Ctx, VD)), DKind(VD->getKind()),
      USR(calculateUsr(Ctx, VD)), Location(calculateLocation(Ctx, VD)),
      ModuleName(VD->getModuleContext()->getName().str()),
      IsThrowing(isFuncThrowing(VD)), IsMutating(isFuncMutating(VD)),
      IsStatic(VD->isStatic()), SelfIndex(getSelfIndex(VD)),
      ReferenceOwnership(getReferenceOwnership(VD)), ExtInfo(nullptr) {

  // Calculate usr for its super class.
  if (auto *CD = dyn_cast_or_null<ClassDecl>(VD)) {
    if (auto *Super = CD->getSuperclassDecl())
      SuperclassUsr = calculateUsr(Ctx, Super);
  }
  if (VD->getAttrs().getDeprecated(VD->getASTContext()))
    DeclAttrs.push_back(SDKDeclAttrKind::DAK_deprecated);

  // If this is fixed_layout struct.
  if (VD->getAttrs().hasAttribute<FixedLayoutAttr>()) {
    DeclAttrs.push_back(SDKDeclAttrKind::DAK_fixedLayout);
  }

  // If the decl is declared in an extension, calculate the extension info.
  if (auto *Ext = dyn_cast_or_null<ExtensionDecl>(VD->getDeclContext())) {
    ExtInfo = new (Ctx) ParentExtensionInfo();
    // Print each generic requirement to the extension info.
    for (auto Req: Ext->getGenericRequirements()) {
      llvm::SmallString<32> Result;
      llvm::raw_svector_ostream OS(Result);
      Req.print(OS, PrintOptions::printInterface());
      ExtInfo->Requirements.emplace_back(Ctx.buffer(OS.str()));
    }
  }

  // Get all protocol names this type decl conforms to.
  if (auto *NTD = dyn_cast<NominalTypeDecl>(VD)) {
    for (auto *P: NTD->getAllProtocols()) {
      ConformingProtocols.push_back(P->getName().str());
    }
  }

  // Get enum raw type name if this is an enum.
  if (auto *ED = dyn_cast<EnumDecl>(VD)) {
    if (auto RT = ED->getRawType()) {
      if (auto *D = RT->getNominalOrBoundGenericNominal()) {
        EnumRawTypeName = D->getName().str();
      }
    }
  }
}

SDKNode *SDKNodeInitInfo::createSDKNode(SDKNodeKind Kind) {
  switch(Kind) {
#define NODE_KIND(X, NAME)                                                     \
case SDKNodeKind::X:                                                           \
  return static_cast<SDKNode*>(new (Ctx.allocator().Allocate<SDKNode##X>())    \
    SDKNode##X(*this));                                                        \
  break;
#include "swift/IDE/DigesterEnums.def"
  }
}

// Recursively construct a node that represents a type, for instance,
// representing the return value type of a function decl.
static SDKNode *constructTypeNode(SDKContext &Ctx, Type T,
                                  TypeInitInfo InitInfo = TypeInitInfo()) {
  SDKNode* Root = SDKNodeInitInfo(Ctx, T, InitInfo)
    .createSDKNode(SDKNodeKind::TypeNominal);

  if (auto NAT = dyn_cast<NameAliasType>(T.getPointer())) {
    SDKNode* Root = SDKNodeInitInfo(Ctx, T).createSDKNode(SDKNodeKind::TypeAlias);
    Root->addChild(constructTypeNode(Ctx, NAT->getCanonicalType()));
    return Root;
  }

  if (auto Fun = T->getAs<AnyFunctionType>()) {
    SDKNode* Root = SDKNodeInitInfo(Ctx, T).createSDKNode(SDKNodeKind::TypeFunc);

    // Still, return type first
    Root->addChild(constructTypeNode(Ctx, Fun->getResult()));
    Root->addChild(constructTypeNode(Ctx, Fun->getInput()));
    return Root;
  }

  // Keep paren type as a stand-alone level.
  if (auto *PT = dyn_cast<ParenType>(T.getPointer())) {
    Root->addChild(constructTypeNode(Ctx, PT->getSinglyDesugaredType()));
    return Root;
  }

  // Handle the case where Type has sub-types.
  if (auto BGT = T->getAs<BoundGenericType>()) {
    for (auto Arg : BGT->getGenericArgs()) {
      Root->addChild(constructTypeNode(Ctx, Arg));
    }
  } else if (auto Tup = T->getAs<TupleType>()) {
    for (auto Elt : Tup->getElementTypes())
      Root->addChild(constructTypeNode(Ctx, Elt));
  } else if (auto MTT = T->getAs<AnyMetatypeType>()) {
    Root->addChild(constructTypeNode(Ctx, MTT->getInstanceType()));
  } else if (auto ATT = T->getAs<ArchetypeType>()) {
    for (auto Pro : ATT->getConformsTo()) {
      Root->addChild(constructTypeNode(Ctx, Pro->getDeclaredType()));
    }
  }
  return Root;
}

static std::vector<SDKNode*>
createParameterNodes(SDKContext &Ctx, ArrayRef<ParameterList*> AllParamLists) {
  std::vector<SDKNode*> Result;
  for (auto PL: AllParamLists) {
    for (auto param: *PL) {
      if (param->isSelfParameter())
        continue;
      TypeInitInfo TypeInfo;
      TypeInfo.IsImplicitlyUnwrappedOptional = param->getAttrs().
        hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
      TypeInfo.hasDefaultArgument = param->getDefaultArgumentKind() !=
        DefaultArgumentKind::None;
      Result.push_back(constructTypeNode(Ctx, param->getInterfaceType(),
                                         TypeInfo));
    }
  }
  return Result;
}

// Construct a node for a function decl. The first child of the function decl
// is guaranteed to be the return value type of this function.
// We sometimes skip the first parameter because it can be metatype of dynamic
// this if the function is a member function.
static SDKNode *constructFunctionNode(SDKContext &Ctx, FuncDecl* FD,
                                      SDKNodeKind Kind) {
  auto Func = SDKNodeInitInfo(Ctx, FD).createSDKNode(Kind);
  TypeInitInfo TypeInfo;
  TypeInfo.IsImplicitlyUnwrappedOptional = FD->getAttrs().
    hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
  Func->addChild(constructTypeNode(Ctx, FD->getResultInterfaceType(), TypeInfo));
  for (auto *Node : createParameterNodes(Ctx, FD->getParameterLists()))
    Func->addChild(Node);
  return Func;
}

static SDKNode* constructInitNode(SDKContext &Ctx, ConstructorDecl *CD) {
  auto Func = SDKNodeInitInfo(Ctx, CD).createSDKNode(SDKNodeKind::DeclConstructor);
  Func->addChild(constructTypeNode(Ctx, CD->getResultInterfaceType()));
  for (auto *Node : createParameterNodes(Ctx, CD->getParameterLists()))
    Func->addChild(Node);
  return Func;
}

static bool shouldIgnore(Decl *D, const Decl* Parent) {
  if (D->isPrivateStdlibDecl(false))
    return true;
  if (AvailableAttr::isUnavailable(D))
    return true;
  if (isa<ConstructorDecl>(D))
    return false;
  if (isa<OperatorDecl>(D))
    return true;
  if (auto VD = dyn_cast<ValueDecl>(D)) {
    if (VD->isOperator())
      return true;
    if (VD->getBaseName().empty())
      return true;
    // This shouldn't happen, being forgiving here.
    if (!VD->hasAccess())
      return true;
    switch (VD->getFormalAccess()) {
    case AccessLevel::Internal:
    case AccessLevel::Private:
    case AccessLevel::FilePrivate:
      return true;
    case AccessLevel::Public:
    case AccessLevel::Open:
      break;
    }
  }

  if (auto *ClangD = D->getClangDecl()) {
    if (isa<clang::ObjCIvarDecl>(ClangD))
      return true;
    if (isa<clang::FieldDecl>(ClangD))
      return true;
    if (ClangD->hasAttr<clang::SwiftPrivateAttr>())
      return true;

    // If this decl is a synthesized member from a conformed clang protocol, we
    // should ignore this member to reduce redundancy.
    if (Parent &&
        !isa<swift::ProtocolDecl>(Parent) &&
        isa<clang::ObjCProtocolDecl>(ClangD->getDeclContext()))
      return true;
  }
  return false;
}

static void addMembersToRoot(SDKContext &Ctx, SDKNode *Root,
                             IterableDeclContext *Context);

static SDKNode *constructTypeDeclNode(SDKContext &Ctx, NominalTypeDecl *NTD) {
  auto TypeNode = SDKNodeInitInfo(Ctx, NTD).createSDKNode(SDKNodeKind::DeclType);
  addMembersToRoot(Ctx, TypeNode, NTD);
  for (auto Ext : NTD->getExtensions()) {
    addMembersToRoot(Ctx, TypeNode, Ext);
  }
  return TypeNode;
}

static SDKNode *constructVarNode(SDKContext &Ctx, ValueDecl *VD) {
  auto Var = SDKNodeInitInfo(Ctx, VD).createSDKNode(SDKNodeKind::DeclVar);
  TypeInitInfo TypeInfo;
  TypeInfo.IsImplicitlyUnwrappedOptional = VD->getAttrs().
    hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
  Var->addChild(constructTypeNode(Ctx, VD->getInterfaceType(), TypeInfo));
  if (auto VAD = dyn_cast<AbstractStorageDecl>(VD)) {
    if (auto Getter = VAD->getGetter())
      Var->addChild(constructFunctionNode(Ctx, Getter, SDKNodeKind::DeclGetter));
    if (auto Setter = VAD->getSetter())
      Var->addChild(constructFunctionNode(Ctx, Setter, SDKNodeKind::DeclSetter));
  }
  return Var;
}

static SDKNode *constructTypeAliasNode(SDKContext &Ctx,TypeAliasDecl *TAD) {
  auto Alias = SDKNodeInitInfo(Ctx, TAD).createSDKNode(SDKNodeKind::DeclTypeAlias);
  Alias->addChild(constructTypeNode(Ctx, TAD->getUnderlyingTypeLoc().getType()));
  return Alias;
}

static void addMembersToRoot(SDKContext &Ctx, SDKNode *Root,
                             IterableDeclContext *Context) {
  for (auto *Member : Context->getMembers()) {
    if (shouldIgnore(Member, Context->getDecl()))
      continue;
    if (auto Func = dyn_cast<FuncDecl>(Member)) {
      Root->addChild(constructFunctionNode(Ctx, Func, SDKNodeKind::DeclFunction));
    } else if (auto CD = dyn_cast<ConstructorDecl>(Member)) {
      Root->addChild(constructInitNode(Ctx, CD));
    } else if (auto VD = dyn_cast<VarDecl>(Member)) {
      Root->addChild(constructVarNode(Ctx, VD));
    } else if (auto TAD = dyn_cast<TypeAliasDecl>(Member)) {
      Root->addChild(constructTypeAliasNode(Ctx, TAD));
    } else if (auto EED = dyn_cast<EnumElementDecl>(Member)) {
      Root->addChild(constructVarNode(Ctx, EED));
    } else if (auto NTD = dyn_cast<NominalTypeDecl>(Member)) {
      Root->addChild(constructTypeDeclNode(Ctx, NTD));
    }
  }
}

static void emitSDKNodeRoot(raw_ostream&, SDKNode *&);

static std::pair<std::unique_ptr<llvm::MemoryBuffer>, SDKNode *>
parseJsonEmit(SDKContext &Ctx, StringRef);

class SwiftDeclCollector : public VisibleDeclConsumer {
  SDKContext &Ctx;
  std::vector<std::unique_ptr<llvm::MemoryBuffer>> OwnedBuffers;
  SDKNode *RootNode;
  llvm::DenseSet<ValueDecl*> KnownDecls;
  // Collected and sorted after we get all of them.
  std::vector<ValueDecl *> ClangMacros;

public:
  void visitAllRoots(SDKNodeVisitor &Visitor) {
    SDKNode::preorderVisit(RootNode, Visitor);
  }
  SwiftDeclCollector(SDKContext &Ctx) : Ctx(Ctx),
    RootNode(SDKNodeRoot::getInstance(Ctx)) {}

  // Construct all roots vector from a given file where a forest was
  // previously dumped.
  void deSerialize(StringRef Filename) {
    auto Pair = parseJsonEmit(Ctx, Filename);
    OwnedBuffers.push_back(std::move(Pair.first));
    RootNode = std::move(Pair.second);
  }

  // Serialize the content of all roots to a given file using JSON format.
  void serialize(StringRef Filename) {
    std::error_code EC;
    llvm::raw_fd_ostream fs(Filename, EC, llvm::sys::fs::F_None);
    emitSDKNodeRoot(fs, RootNode);
  }

  // After collecting decls, either from imported modules or from a previously
  // serialized JSON file, using this function to get the root of the SDK.
  SDKNodeRoot* getSDKRoot() {
    return static_cast<SDKNodeRoot*>(RootNode);
  }

  void printTopLevelNames() {
    for (auto &Node : RootNode->getChildren()) {
      llvm::outs() << Node->getKind() << ": " << Node->getName() << '\n';
    }
  }

public:
  void lookupVisibleDecls(ArrayRef<ModuleDecl *> Modules) {
    for (auto M : Modules) {
      llvm::SmallVector<Decl*, 512> Decls;
      M->getDisplayDecls(Decls);
      for (auto D : Decls) {
        if (auto VD = dyn_cast<ValueDecl>(D))
          foundDecl(VD, DeclVisibilityKind::DynamicLookup);
      }
    }

    // Now sort the macros before processing so that we can have deterministic
    // output.
    llvm::array_pod_sort(ClangMacros.begin(), ClangMacros.end(),
       [](ValueDecl * const *lhs,
          ValueDecl * const *rhs) -> int {
         return (*lhs)->getBaseName().userFacingName().compare(
                  (*rhs)->getBaseName().userFacingName());
       });

    for (auto *VD : ClangMacros)
      processDecl(VD);
  }

  void processDecl(ValueDecl *VD) {
    if (shouldIgnore(VD, nullptr))
      return;

    if (auto FD = dyn_cast<FuncDecl>(VD)) {
      RootNode->addChild(constructFunctionNode(Ctx, FD, SDKNodeKind::DeclFunction));
    } else if (auto NTD = dyn_cast<NominalTypeDecl>(VD)) {
      RootNode->addChild(constructTypeDeclNode(Ctx, NTD));
    }
    if (auto VAD = dyn_cast<VarDecl>(VD)) {
      RootNode->addChild(constructVarNode(Ctx, VAD));
    }
    if (auto TAD = dyn_cast<TypeAliasDecl>(VD)) {
      RootNode->addChild(constructTypeAliasNode(Ctx, TAD));
    }
  }

  void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
    if (KnownDecls.count(VD))
      return;
    KnownDecls.insert(VD);

    if (VD->getClangMacro()) {
      // Collect macros, we will sort them afterwards.
      ClangMacros.push_back(VD);
      return;
    }

    processDecl(VD);
  }
};
} // end anonymous namespace


namespace swift {
  namespace json {
  // In the namespace of swift::json, we define several functions so that the
  // JSON serializer will know how to interpret and dump types defined in this
  // file.
    template<>
    struct ScalarEnumerationTraits<TypeAttrKind> {
      static void enumeration(Output &out, TypeAttrKind &value) {
#define TYPE_ATTR(X) out.enumCase(value, #X, TypeAttrKind::TAK_##X);
#include "swift/AST/Attr.def"
        out.enumCase(value, "Count", TypeAttrKind::TAK_Count);
      }
    };

    template<>
    struct ScalarEnumerationTraits<SDKDeclAttrKind> {
      static void enumeration(Output &out, SDKDeclAttrKind &value) {
#define DECL_ATTR(X) out.enumCase(value, #X, SDKDeclAttrKind::DAK_##X);
#include "swift/IDE/DigesterEnums.def"
      }
    };

    template<>
    struct ScalarEnumerationTraits<DeclKind> {
      static void enumeration(Output &out, DeclKind &value) {
#define DECL(X, PARENT) out.enumCase(value, #X, DeclKind::X);
#include "swift/AST/DeclNodes.def"
      }
    };

    template<>
    struct ObjectTraits<SDKNode *> {
      static void mapping(Output &out, SDKNode *&value) {
        auto Kind = value->getKind();
        auto Name = value->getName();
        auto PrintedName = value->getPrintedName();
        auto &Ctx = value->getSDKContext();
        out.mapRequired(getKeyContent(Ctx, KeyKind::KK_kind).data(), Kind);
        out.mapRequired(getKeyContent(Ctx, KeyKind::KK_name).data(), Name);
        out.mapRequired(getKeyContent(Ctx, KeyKind::KK_printedName).data(),
                        PrintedName);

        if (auto D = dyn_cast<SDKNodeDecl>(value)) {
          DeclKind DK = D->getDeclKind();
          StringRef Usr = D->getUsr();
          StringRef Location = D->getLocation();
          StringRef ModuleName = D->getModuleName();
          out.mapRequired(getKeyContent(Ctx, KeyKind::KK_declKind).data(), DK);
          out.mapRequired(getKeyContent(Ctx, KeyKind::KK_usr).data(), Usr);
          out.mapRequired(getKeyContent(Ctx, KeyKind::KK_location).data(), Location);
          out.mapRequired(getKeyContent(Ctx, KeyKind::KK_moduleName).data(),
                          ModuleName);

          if (auto isStatic = D->isStatic())
            out.mapRequired(getKeyContent(Ctx, KeyKind::KK_static).data(), isStatic);

          if (auto F = dyn_cast<SDKNodeDeclAbstractFunc>(value)) {
            if (bool isThrowing = F->isThrowing())
              out.mapRequired(getKeyContent(Ctx, KeyKind::KK_throwing).data(),
                              isThrowing);
            if (bool isMutating = F->isMutating())
              out.mapRequired(getKeyContent(Ctx, KeyKind::KK_mutating).data(),
                              isMutating);
            if (F->hasSelfIndex()) {
              auto Index = F->getSelfIndex();
              out.mapRequired(getKeyContent(Ctx, KeyKind::KK_selfIndex).data(),
                              Index);
            }
          }
          if (auto *TD = dyn_cast<SDKNodeDeclType>(value)) {
            auto Super = TD->getSuperClassUsr();
            if (!Super.empty()) {
              out.mapRequired(getKeyContent(Ctx, KeyKind::KK_superclassUsr).data(),
                              Super);
            }
            auto Pros = TD->getAllProtocols();
            if (!Pros.empty()) {
              out.mapRequired(getKeyContent(Ctx,
                                        KeyKind::KK_conformingProtocols).data(),
                              Pros);
            }

            auto RawTypeName = TD->isEnum() ? TD->getEnumRawTypeName() : StringRef();
            if (!RawTypeName.empty()) {
              out.mapRequired(getKeyContent(Ctx,
                                            KeyKind::KK_enumRawTypeName).data(),
                              RawTypeName);
            }

          }
          if (D->isFromExtension()) {
            // Even if we don't have any requirements on this parent extension,
            // we still want to have this key present to indicate the member
            // is from an extension.
            auto Reqs = D->getExtensionInfo().getGenericRequirements();
            out.mapRequired(getKeyContent(Ctx,
                                          KeyKind::KK_parentExtensionReqs).data(),
                            Reqs);
          }
          auto Attributes = D->getDeclAttributes();
          if (!Attributes.empty())
            out.mapRequired(getKeyContent(Ctx, KeyKind::KK_declAttributes).data(),
                            Attributes);
          // Strong reference is implied, no need for serialization.
          if (D->getReferenceOwnership() != ReferenceOwnership::Strong) {
            uint8_t Raw = uint8_t(D->getReferenceOwnership());
            out.mapRequired(getKeyContent(Ctx, KeyKind::KK_ownership).data(), Raw);
          }
        } else if (auto T = dyn_cast<SDKNodeType>(value)) {
          auto Attributes = T->getTypeAttributes();
          if (!Attributes.empty())
            out.mapRequired(getKeyContent(Ctx, KeyKind::KK_typeAttributes).data(),
                            Attributes);
          if (bool HasDefault = T->hasDefaultArgument()) {
            out.mapRequired(getKeyContent(Ctx, KeyKind::KK_hasDefaultArg).data(),
                            HasDefault);
          }
          // Serialize nominal type's USR.
          if (auto NT = dyn_cast<SDKNodeTypeNominal>(value)) {
            auto Usr = NT->getUsr();
            if (!Usr.empty())
              out.mapRequired(getKeyContent(Ctx, KeyKind::KK_usr).data(), Usr);
          }
        }
        if (!value->isLeaf()) {
          ArrayRef<SDKNode *> Children = value->getChildren();
          out.mapRequired(getKeyContent(Ctx, KeyKind::KK_children).data(), Children);
        }
      }
    };

    template<>
    struct ArrayTraits<ArrayRef<SDKNode*>> {
      static size_t size(Output &out, ArrayRef<SDKNode *> &seq) {
        return seq.size();
      }
      static SDKNode *&element(Output &, ArrayRef<SDKNode *> &seq,
                                    size_t index) {
        return const_cast<SDKNode *&>(seq[index]);
      }
    };

    template<>
    struct ArrayTraits<ArrayRef<TypeAttrKind>> {
      static size_t size(Output &out, ArrayRef<TypeAttrKind> &seq) {
        return seq.size();
      }
      static TypeAttrKind& element(Output &, ArrayRef<TypeAttrKind> &seq,
                                   size_t index) {
        return const_cast<TypeAttrKind&>(seq[index]);
      }
    };

    template<>
    struct ArrayTraits<ArrayRef<SDKDeclAttrKind>> {
      static size_t size(Output &out, ArrayRef<SDKDeclAttrKind> &seq) {
        return seq.size();
      }
      static SDKDeclAttrKind& element(Output &, ArrayRef<SDKDeclAttrKind> &seq,
                                   size_t index) {
        return const_cast<SDKDeclAttrKind&>(seq[index]);
      }
    };
    template<>
    struct ArrayTraits<ArrayRef<StringRef>> {
      static size_t size(Output &out, ArrayRef<StringRef> &seq) {
        return seq.size();
      }
      static StringRef& element(Output &, ArrayRef<StringRef> &seq,
                                   size_t index) {
        return const_cast<StringRef&>(seq[index]);
      }
    };
  } // namespace json
} // namespace swift

namespace  {// Anonymous namespace resumes.

// Serialize a forest of SDKNode trees to the given stream.
static void emitSDKNodeRoot(llvm::raw_ostream &os, SDKNode *&Root) {
  json::Output yout(os);
  yout << Root;
}

// Deserialize an SDKNode tree.
std::pair<std::unique_ptr<llvm::MemoryBuffer>, SDKNode*>
parseJsonEmit(SDKContext &Ctx, StringRef FileName) {
  namespace yaml = llvm::yaml;

  // Load the input file.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
  llvm::MemoryBuffer::getFileOrSTDIN(FileName);
  if (!FileBufOrErr) {
    llvm_unreachable("Failed to read JSON file");
  }
  StringRef Buffer = FileBufOrErr->get()->getBuffer();
  llvm::SourceMgr SM;
  yaml::Stream Stream(Buffer, SM);
  SDKNode *Result = nullptr;
  for (auto DI = Stream.begin(); DI != Stream.end(); ++ DI) {
    assert(DI != Stream.end() && "Failed to read a document");
    yaml::Node *N = DI->getRoot();
    assert(N && "Failed to find a root");
    Result = SDKNode::constructSDKNode(Ctx, cast<yaml::MappingNode>(N));
  }
  return {std::move(FileBufOrErr.get()), Result};
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
      if (L && R && *L == *R)
        continue;
      if (!L || !R)
        Listener.foundRemoveAddMatch(L, R);
      else
        Listener.foundMatch(L, R);
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
                   MatchedNodeListener &Listener) : Left(Left), Right(Right),
  CanMatch(CanMatch),
  IsFirstMatchBetter(IsFirstMatchBetter),
  Listener(Listener){}

  void match() override {
    for (auto L : Left) {
      if (auto Best = findBestMatch(L, Right)) {
        MatchedRight.insert(Best.getValue());
        Listener.foundMatch(L, Best.getValue());
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
        Listener.foundRemoveAddMatch(A, nullptr);
      else
        Listener.foundRemoveAddMatch(nullptr, A);
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
        foundMatch(R, A);
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
          foundMatch(R, A);
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
      foundMatch(R, A);
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

  void foundMatch(NodePtr R, NodePtr A) override {
    Listener.foundRemoveAddMatch(R, A);
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
            detectSameAnonymousEnum(RD, AD)) {
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
                                   isBetterMatch, *this);
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

  // Given two SDK nodes, figure out the reason for why they have the same name.
  Optional<NameMatchKind> getNameMatchKind(SDKNode *L, SDKNode *R) {
    if (L->getKind() != R->getKind())
      return None;
    auto *LD = L->getAs<SDKNodeDecl>();
    auto *RD = R->getAs<SDKNodeDecl>();
    assert(LD && RD);
    auto NameEqual = LD->getPrintedName() == RD->getPrintedName();
    auto UsrEqual = LD->getUsr() == RD->getUsr();
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
      Listener.foundMatch(LN, Match);
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
    Listener.foundMatch(L, R);
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

static void detectFuncDeclChange(NodePtr L, NodePtr R) {
  assert(L->getKind() == R->getKind());
  if (auto LF = dyn_cast<SDKNodeDeclAbstractFunc>(L)) {
    auto RF = R->getAs<SDKNodeDeclAbstractFunc>();
    if (!LF->isThrowing() && RF->isThrowing()) {
      LF->annotate(NodeAnnotation::NowThrowing);
    }
    if (!LF->isMutating() && RF->isMutating()) {
      LF->annotate(NodeAnnotation::NowMutating);
    }
  }
}

static void detectRename(NodePtr L, NodePtr R) {
  assert(L->getKind() == R->getKind());
  if (isa<SDKNodeDecl>(L) && L->getPrintedName() != R->getPrintedName()) {
    L->annotate(NodeAnnotation::Rename);
    L->annotate(NodeAnnotation::RenameOldName, L->getPrintedName());
    L->annotate(NodeAnnotation::RenameNewName, R->getPrintedName());
  }
}

static void detectDeclChange(NodePtr L, NodePtr R) {
  assert(L->getKind() == R->getKind());
  if (auto LD = dyn_cast<SDKNodeDecl>(L)) {
    auto *RD = R->getAs<SDKNodeDecl>();
    if (LD->isStatic() ^ RD->isStatic())
      L->annotate(NodeAnnotation::StaticChange);
    if (LD->getReferenceOwnership() != RD->getReferenceOwnership())
      L->annotate(NodeAnnotation::OwnershipChange);
    detectRename(L, R);
  }
}

// This is first pass on two given SDKNode trees. This pass removes the common part
// of two versions of SDK, leaving only the changed part.
class PrunePass : public MatchedNodeListener, public SDKTreeDiffPass {
  static void removeCommonChildren(NodePtr Left, NodePtr Right) {
    llvm::SmallPtrSet<NodePtr, 16> LeftToRemove;
    llvm::SmallPtrSet<NodePtr, 16> RightToRemove;
    for (auto LC : Left->getChildren()) {
      for (auto RC : Right->getChildren()) {
        if (*LC == *RC) {
          LeftToRemove.insert(LC);
          RightToRemove.insert(RC);
          break;
        }
      }
    }
    for (NodePtr L : LeftToRemove)
      Left->removeChild(L);
    for (NodePtr R : RightToRemove)
      Right->removeChild(R);
  }

  UpdatedNodesMap &UpdateMap;

public:
  PrunePass(UpdatedNodesMap &UpdateMap) : UpdateMap(UpdateMap) {}

  void foundRemoveAddMatch(NodePtr Left, NodePtr Right) override {
    if (!Left)
      Right->annotate(NodeAnnotation::Added);
    else if (!Right) {
      Left->annotate(NodeAnnotation::Removed);
    } else if (Right->getKind() == Left->getKind()) {
      foundMatch(Left, Right);
    } else {
      Left->annotate(NodeAnnotation::Removed);
      Right->annotate(NodeAnnotation::Added);
    }
  }

  void foundMatch(NodePtr Left, NodePtr Right) override {
    assert(Left && Right);
    Left->annotate(NodeAnnotation::Updated);
    Right->annotate(NodeAnnotation::Updated);
    // Push the updated node to the map for future reference.
    UpdateMap.foundMatch(Left, Right);

    if (Left->getKind() != Right->getKind()) {
      assert(isa<SDKNodeType>(Left) && isa<SDKNodeType>(Right) &&
        "only type nodes can match across kinds.");
      return;
    }
    assert(Left->getKind() == Right->getKind());
    SDKNodeKind Kind = Left->getKind();
    assert(Kind == SDKNodeKind::Root || *Left != *Right);

    detectDeclChange(Left, Right);
    detectFuncDeclChange(Left, Right);

    switch(Kind) {
    case SDKNodeKind::Root:
    case SDKNodeKind::DeclType: {
      // If the matched nodes are both modules, remove the contained
      // type decls that are identical. If the matched nodes are both type decls,
      // remove the contained function decls that are identical.
      removeCommonChildren(Left, Right);
      SameNameNodeMatcher SNMatcher(Left->getChildren(), Right->getChildren(), *this);
      SNMatcher.match();
      break;
    }

    case SDKNodeKind::DeclFunction:
    case SDKNodeKind::DeclSetter:
    case SDKNodeKind::DeclGetter:
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

    case SDKNodeKind::DeclVar: {
      auto LC = Left->getChildren()[0];
      auto RC = Right->getChildren()[0];
      if (!(*LC == *RC))
        foundMatch(LC, RC);
      break;
    }
    }
  }

  void pass(NodePtr Left, NodePtr Right) override {
    foundMatch(Left, Right);
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
    // Move from a member variable to another member variable
    if (nodeParent->getKind() == SDKNodeKind::DeclType &&
        diffParent->getKind() == SDKNodeKind::DeclType &&
        declNode->isStatic())
      TypeMemberDiffs.insert({diffNode, node});
    // Move from a getter/setter function to a property
    else if (node->getKind() == SDKNodeKind::DeclGetter &&
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

class ChangeRefinementPass : public SDKTreeDiffPass, public SDKNodeVisitor {
  bool IsVisitingLeft;
  UpdatedNodesMap &UpdateMap;

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
        (Node->getName() != Counter->getName()||
        Node->getChildrenCount() != Counter->getChildrenCount())) {
      Node->annotate(NodeAnnotation::TypeRewritten);
      Node->annotate(NodeAnnotation::TypeRewrittenLeft, Node->getPrintedName());
      Node->annotate(NodeAnnotation::TypeRewrittenRight, 
                     Counter->getPrintedName());
      return true;
    }
    return false;
  }

  static StringRef getStringRepresentableChange(SDKNode *L, SDKNode *R) {
    auto* LKey = dyn_cast<SDKNodeTypeNominal>(L);
    auto* RKey = dyn_cast<SDKNodeTypeNominal>(R);
    if (!LKey || !RKey)
      return StringRef();
    if (LKey->getTypeKind() != KnownTypeKind::String)
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
                                                     SDKNodeType *R) {
    if (L->getTypeKind() != KnownTypeKind::Dictionary ||
        R->getTypeKind() != KnownTypeKind::Dictionary)
      return StringRef();
    auto *Left = dyn_cast<SDKNodeTypeNominal>(L);
    auto *Right = dyn_cast<SDKNodeTypeNominal>(R);
    assert(Left && Right);
    assert(Left->getChildrenCount() == 2);
    assert(Right->getChildrenCount() == 2);
    return getStringRepresentableChange(*Left->getChildBegin(),
      *Right->getChildBegin());
  }

  bool detectDictionaryKeyChange(SDKNodeType *L, SDKNodeType *R) {
    // We only care if this the top-level type node.
    if (!L->isTopLevelType() || !R->isTopLevelType())
      return false;
    StringRef KeyChangedTo;
    bool HasOptional = L->getTypeKind() == KnownTypeKind::Optional &&
      R->getTypeKind() == KnownTypeKind::Optional;
    if (HasOptional) {
      // Detect [String: Any]? to [StringRepresentableStruct: Any]? Chnage
      KeyChangedTo =
        detectDictionaryKeyChangeInternal(L->getOnlyChild()->getAs<SDKNodeType>(),
                                          R->getOnlyChild()->getAs<SDKNodeType>());
    } else {
      // Detect [String: Any] to [StringRepresentableStruct: Any] Chnage
      KeyChangedTo = detectDictionaryKeyChangeInternal(L, R);
    }
    if (!KeyChangedTo.empty()) {
      if (IsVisitingLeft) {
        L->annotate(HasOptional ?
                    NodeAnnotation::OptionalDictionaryKeyUpdate :
                    NodeAnnotation::DictionaryKeyUpdate);
        L->annotate(NodeAnnotation::TypeRewrittenRight, KeyChangedTo);
      } else {
        R->annotate(HasOptional ?
                    NodeAnnotation::RevertOptionalDictionaryKeyUpdate :
                    NodeAnnotation::RevertDictionaryKeyUpdate);
      }
      return true;
    }
    return false;
  }

  static StringRef detectArrayMemberChangeInternal(SDKNodeType *L,
      SDKNodeType *R) {
    if (L->getTypeKind() != KnownTypeKind::Array ||
        R->getTypeKind() != KnownTypeKind::Array)
      return StringRef();
    auto *Left = dyn_cast<SDKNodeTypeNominal>(L);
    auto *Right = dyn_cast<SDKNodeTypeNominal>(R);
    assert(Left && Right);
    assert(Left->getChildrenCount() == 1);
    assert(Right->getChildrenCount() == 1);
    return getStringRepresentableChange(Left->getOnlyChild(),
      Right->getOnlyChild());
  }

  bool detectArrayMemberChange(SDKNodeType* L, SDKNodeType *R) {
    // We only care if this the top-level type node.
    if (!L->isTopLevelType() || !R->isTopLevelType())
      return false;
    StringRef KeyChangedTo;
    bool HasOptional = L->getTypeKind() == KnownTypeKind::Optional &&
      R->getTypeKind() == KnownTypeKind::Optional;
    if (HasOptional) {
      // Detect [String]? to [StringRepresentableStruct]? Chnage
      KeyChangedTo =
        detectArrayMemberChangeInternal(L->getOnlyChild()->getAs<SDKNodeType>(),
                                        R->getOnlyChild()->getAs<SDKNodeType>());
    } else {
      // Detect [String] to [StringRepresentableStruct] Chnage
      KeyChangedTo = detectArrayMemberChangeInternal(L, R);
    }
    if (!KeyChangedTo.empty()) {
      if (IsVisitingLeft) {
        L->annotate(HasOptional ?
                    NodeAnnotation::OptionalArrayMemberUpdate :
                    NodeAnnotation::ArrayMemberUpdate);
        L->annotate(NodeAnnotation::TypeRewrittenRight, KeyChangedTo);
      } else {
        R->annotate(HasOptional ?
                    NodeAnnotation::RevertOptionalArrayMemberUpdate :
                    NodeAnnotation::RevertArrayMemberUpdate);
      }
      return true;
    }
    return false;
  }

  bool detectSimpleStringRepresentableUpdate(SDKNodeType *L, SDKNodeType *R) {
    if (!L->isTopLevelType() || !R->isTopLevelType())
      return false;
    StringRef KeyChangedTo;
    bool HasOptional = L->getTypeKind() == KnownTypeKind::Optional &&
        R->getTypeKind() == KnownTypeKind::Optional;
    if (HasOptional) {
      // Detect String? changes to StringRepresentableStruct? change.
      KeyChangedTo =
        getStringRepresentableChange(L->getOnlyChild()->getAs<SDKNodeType>(),
                                     R->getOnlyChild()->getAs<SDKNodeType>());
    } else {
      // Detect String changes to StringRepresentableStruct change.
      KeyChangedTo = getStringRepresentableChange(L, R);
    }
    if (!KeyChangedTo.empty()) {
      if (IsVisitingLeft) {
        L->annotate(NodeAnnotation::TypeRewrittenRight, KeyChangedTo);
        L->annotate(HasOptional ?
                    NodeAnnotation::SimpleOptionalStringRepresentableUpdate:
                    NodeAnnotation::SimpleStringRepresentableUpdate);
      } else {
        R->annotate(HasOptional ?
                    NodeAnnotation::RevertSimpleOptionalStringRepresentableUpdate:
                    NodeAnnotation::RevertSimpleStringRepresentableUpdate);
      }
      return true;
    }
    return false;
  }

  bool isUnhandledCase(SDKNodeType *Node) {
    auto Counter = UpdateMap.findUpdateCounterpart(Node)->getAs<SDKNodeType>();
    return Node->getTypeKind() == KnownTypeKind::Void ||
           Counter->getTypeKind() == KnownTypeKind::Void;
  }

public:
  ChangeRefinementPass(UpdatedNodesMap &UpdateMap) : UpdateMap(UpdateMap) {}

  void pass(NodePtr Left, NodePtr Right) override {

    // Post-order visit is necessary since we propagate annotations bottom-up
    IsVisitingLeft = true;
    SDKNode::postorderVisit(Left, *this);
    IsVisitingLeft = false;
    SDKNode::postorderVisit(Right, *this);
  }

  void visit(NodePtr N) override {
    auto Node = dyn_cast<SDKNodeType>(N);
    if (!Node || !Node->isAnnotatedAs(NodeAnnotation::Updated) ||
        isUnhandledCase(Node))
      return;
    auto Counter = const_cast<SDKNodeType*>(UpdateMap.
      findUpdateCounterpart(Node)->getAs<SDKNodeType>());

    bool Result = detectWrapOptional(Node, Counter)||
                  detectOptionalUpdate(Node, Counter)||
                  detectWrapImplicitOptional(Node, Counter)||
                  detectUnmanagedUpdate(Node, Counter)||
                  detectDictionaryKeyChange(Node, Counter) ||
                  detectArrayMemberChange(Node, Counter) ||
                  detectSimpleStringRepresentableUpdate(Node, Counter) ||
                  detectTypeRewritten(Node, Counter);
    (void) Result;
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
  removeRedundantAndSort(Diffs);
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
    for (auto Annotation: Annotations) {
      if (isInterested(NonTypeParent, Annotation) &&
          Node->isAnnotatedAs(Annotation)) {
        auto Kind = NonTypeParent->getKind();
        StringRef LC = getLeftComment(Node, Annotation);
        StringRef RC = getRightComment(Node, Annotation);
        AllItems.emplace_back(Kind, Annotation, Index,
                              NonTypeParent->getUsr(), StringRef(), LC, RC,
                              NonTypeParent->getModuleName());
        return;
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
  void visitType(SDKNodeType *T);
  void visitDecl(SDKNodeDecl *D);
  void visit(NodePtr Node) override;
  SDKNodeDecl *findAddedDecl(const SDKNodeDecl *Node);
  bool findTypeAliasDecl(const SDKNodeDecl *Node);
  static StringRef printName(StringRef Name);
  static StringRef printDiagKeyword(StringRef Name);
  static void collectAddedDecls(NodePtr Root, std::set<SDKNodeDecl*> &Results);

  template<typename T>
  struct DiagBag {
    std::vector<T> Diags;
    ~DiagBag() {
      llvm::outs() << "\n/* ";
      T::theme(llvm::outs());
      llvm::outs() << " */\n";
      removeRedundantAndSort(Diags);
      std::for_each(Diags.begin(), Diags.end(), [](T &Diag) {
        Diag.outputModule();
        Diag.output();
      });
    }
  };

  struct MetaInfo {
    StringRef ModuleName;
    StringRef HeaderName;
    MetaInfo(const SDKNodeDecl *Node):
      ModuleName(Node->getModuleName()), HeaderName(Node->getHeaderName()) {}
  };

  struct DiagBase {
    MetaInfo Info;
    DiagBase(MetaInfo Info): Info(Info) {}
    virtual ~DiagBase() = default;
    void outputModule() const {
      if (options::PrintModule) {
        llvm::outs() << Info.ModuleName;
        if (!Info.HeaderName.empty())
          llvm::outs() << "(" << Info.HeaderName << ")";
        llvm::outs() << ": ";
      }
    }
    virtual void output() const = 0;
  };

  struct RemovedDeclDiag: public DiagBase  {
    DeclKind Kind;
    StringRef Name;
    bool IsDeprecated;
    RemovedDeclDiag(MetaInfo Info, DeclKind Kind, StringRef Name,
                    bool IsDeprecated): DiagBase(Info), Kind(Kind),
                                        Name(Name), IsDeprecated(IsDeprecated) {}
    bool operator<(RemovedDeclDiag Other) const;
    void output() const override;
    static void theme(raw_ostream &OS) { OS << "Removed Decls"; };
  };

  struct MovedDeclDiag: public DiagBase {
    DeclKind RemovedKind;
    DeclKind AddedKind;
    StringRef RemovedName;
    StringRef AddedName;
    MovedDeclDiag(MetaInfo Info, DeclKind RemovedKind, DeclKind AddedKind,
                  StringRef RemovedName, StringRef AddedName):
      DiagBase(Info), RemovedKind(RemovedKind), AddedKind(AddedKind),
      RemovedName(RemovedName), AddedName(AddedName) {}
    bool operator<(MovedDeclDiag other) const;
    void output() const override;
    static void theme(raw_ostream &OS) { OS << "Moved Decls"; };
  };

  struct RenamedDeclDiag: public DiagBase  {
    DeclKind KindBefore;
    DeclKind KindAfter;
    StringRef NameBefore;
    StringRef NameAfter;
    RenamedDeclDiag(MetaInfo Info, DeclKind KindBefore, DeclKind KindAfter,
                    StringRef NameBefore, StringRef NameAfter):
                      DiagBase(Info),
                      KindBefore(KindBefore), KindAfter(KindAfter),
                      NameBefore(NameBefore), NameAfter(NameAfter) {}
    bool operator<(RenamedDeclDiag Other) const;
    void output() const override;
    static void theme(raw_ostream &OS) { OS << "Renamed Decls"; };
  };

  struct DeclAttrDiag: public DiagBase {
    DeclKind Kind;
    StringRef DeclName;
    StringRef AttrBefore;
    StringRef AttrAfter;
    DeclAttrDiag(MetaInfo Info, DeclKind Kind, StringRef DeclName,
                 StringRef AttrBefore, StringRef AttrAfter):
                   DiagBase(Info), Kind(Kind), DeclName(DeclName),
                   AttrBefore(AttrBefore), AttrAfter(AttrAfter) {}
    DeclAttrDiag(MetaInfo Info, DeclKind Kind, StringRef DeclName,
                 StringRef AttrAfter): DeclAttrDiag(Info, Kind, DeclName,
                                                    StringRef(), AttrAfter) {}

    bool operator<(DeclAttrDiag Other) const;
    void output() const override;
    static void theme(raw_ostream &OS) { OS << "Decl Attribute changes"; };
  };

  struct DeclTypeChangeDiag: public DiagBase {
    DeclKind Kind;
    StringRef DeclName;
    StringRef TypeNameBefore;
    StringRef TypeNameAfter;
    StringRef Description;
    DeclTypeChangeDiag(MetaInfo Info, DeclKind Kind, StringRef DeclName,
                       StringRef TypeNameBefore, StringRef TypeNameAfter,
                       StringRef Description): DiagBase(Info),
      Kind(Kind), DeclName(DeclName), TypeNameBefore(TypeNameBefore),
      TypeNameAfter(TypeNameAfter), Description(Description) {}
    bool operator<(DeclTypeChangeDiag Other) const;
    void output() const override;
    static void theme(raw_ostream &OS) { OS << "Type Changes"; };
  };

  struct RawRepresentableChangeDiag: public DiagBase {
    DeclKind Kind;
    StringRef DeclName;
    StringRef UnderlyingType;
    StringRef RawTypeName;
    RawRepresentableChangeDiag(MetaInfo Info, DeclKind Kind, StringRef DeclName,
        StringRef UnderlyingType, StringRef RawTypeName): DiagBase(Info),
        Kind(Kind), DeclName(DeclName), UnderlyingType(UnderlyingType),
        RawTypeName(RawTypeName) {}
    bool operator<(RawRepresentableChangeDiag Other) const {
      if (Kind != Other.Kind)
        return Kind < Other.Kind;
      return DeclName.compare(Other.DeclName) < 0;
    }
    void output() const override {
      llvm::outs() << Kind << " " << printName(DeclName)
        << "(" << UnderlyingType << ")"
        << " is now " << RawTypeName << " representable\n";
    }
    static void theme(raw_ostream &OS) { OS << "RawRepresentable Changes"; };
  };

  std::set<SDKNodeDecl*> AddedDecls;
  DiagBag<DeclAttrDiag> AttrChangedDecls;
  DiagBag<DeclTypeChangeDiag> TypeChangedDecls;
  DiagBag<RenamedDeclDiag> RenamedDecls;
  DiagBag<MovedDeclDiag> MovedDecls;
  DiagBag<RemovedDeclDiag> RemovedDecls;
  DiagBag<RawRepresentableChangeDiag> RawRepresentableDecls;

  UpdatedNodesMap &UpdateMap;
  NodeMap &TypeAliasUpdateMap;
  TypeMemberDiffVector &MemberChanges;
  DiagnosisEmitter(SDKContext &Ctx):
    UpdateMap(Ctx.getNodeUpdateMap()),
    TypeAliasUpdateMap(Ctx.getTypeAliasUpdateMap()),
    MemberChanges(Ctx.getTypeMemberDiffs()){}

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

StringRef DiagnosisEmitter::printName(StringRef Name) {
  OSColor Color(llvm::outs(), llvm::raw_ostream::CYAN);
  Color << Name;
  return StringRef();
}

StringRef DiagnosisEmitter::printDiagKeyword(StringRef Name) {
  OSColor Color(llvm::outs(), llvm::raw_ostream::YELLOW);
  Color << Name;
  return StringRef();
}

bool DiagnosisEmitter::RemovedDeclDiag::
operator<(RemovedDeclDiag Other) const {
  if (Kind != Other.Kind)
    return Kind < Other.Kind;
  return Name.compare(Other.Name) < 0;
}

void DiagnosisEmitter::RemovedDeclDiag::output() const {
  llvm::outs() << Kind << " " << printName(Name) << " has been "
    << printDiagKeyword("removed");
  if (IsDeprecated)
    llvm::outs() << " (deprecated)";
  llvm::outs() << "\n";
}

bool DiagnosisEmitter::MovedDeclDiag::
operator<(MovedDeclDiag Other) const {
  if (RemovedKind != Other.RemovedKind)
    return RemovedKind < Other.RemovedKind;
  return RemovedName.compare(Other.RemovedName) < 0;
}

void DiagnosisEmitter::MovedDeclDiag::output() const {
  llvm::outs() << RemovedKind << " " << printName(RemovedName) << " has been "
    << printDiagKeyword("moved") << " to " << AddedKind << " "
    << printName(AddedName) << "\n";
}

bool DiagnosisEmitter::RenamedDeclDiag::
operator<(RenamedDeclDiag Other) const {
  if (KindBefore != Other.KindBefore)
    return KindBefore < Other.KindBefore;
  return NameBefore.compare(Other.NameBefore) < 0;
}

void DiagnosisEmitter::RenamedDeclDiag::output() const {
  llvm::outs() << KindBefore << " " << printName(NameBefore)
               << " has been " << printDiagKeyword("renamed") << " to "
               << KindAfter << " " << printName(NameAfter) << "\n";
}

bool DiagnosisEmitter::DeclTypeChangeDiag::
operator<(DeclTypeChangeDiag Other) const {
  if (Kind != Other.Kind)
    return Kind < Other.Kind;
  return DeclName.compare(Other.DeclName) < 0;
}

void DiagnosisEmitter::DeclTypeChangeDiag::output() const {
  llvm::outs() << Kind << " " << printName(DeclName) << " has "
               << Description << " type change from "
               << printName(TypeNameBefore) << " to "
               << printName(TypeNameAfter) << "\n";
}


bool DiagnosisEmitter::DeclAttrDiag::operator<(DeclAttrDiag Other) const {
  if (Kind != Other.Kind)
    return Kind < Other.Kind;
  return DeclName.compare_lower(Other.DeclName);
}

void DiagnosisEmitter::DeclAttrDiag::output() const {
  if (AttrBefore.empty())
    llvm::outs() << Kind << " " << printName(DeclName) << " is now " <<
      printDiagKeyword(AttrAfter)<< "\n";
  else
    llvm::outs() << Kind << " " << printName(DeclName) << " changes from " <<
      printDiagKeyword(AttrBefore) << " to "<< printDiagKeyword(AttrAfter)<< "\n";
}

void DiagnosisEmitter::diagnosis(NodePtr LeftRoot, NodePtr RightRoot,
                                 SDKContext &Ctx) {
  DiagnosisEmitter Emitter(Ctx);
  collectAddedDecls(RightRoot, Emitter.AddedDecls);
  SDKNode::postorderVisit(LeftRoot, Emitter);
}

void DiagnosisEmitter::handle(const SDKNodeDecl *Node, NodeAnnotation Anno) {
  assert(Node->isAnnotatedAs(Anno));
  auto &Ctx = Node->getSDKContext();
  MetaInfo ScreenInfo(Node);
  switch(Anno) {
  case NodeAnnotation::Removed: {
    // If we can find a type alias decl with the same name of this type, we
    // consider the type is not removed.
    if (findTypeAliasDecl(Node))
      return;
    if (auto *Added = findAddedDecl(Node)) {
      if (Node->getDeclKind() != DeclKind::Constructor) {
        MovedDecls.Diags.emplace_back(ScreenInfo,
                                      Node->getDeclKind(),
                                      Added->getDeclKind(),
                                      Node->getFullyQualifiedName(),
                                      Added->getFullyQualifiedName());
        return;
      }
    }

    // If we can find a hoisted member for this removed delcaration, we
    // emit the diagnostics as rename instead of removal.
    auto It = std::find_if(MemberChanges.begin(), MemberChanges.end(),
      [&](TypeMemberDiffItem &Item) { return Item.usr == Node->getUsr(); });
    if (It != MemberChanges.end()) {
      RenamedDecls.Diags.emplace_back(ScreenInfo, Node->getDeclKind(),
        Node->getDeclKind(), Node->getFullyQualifiedName(),
        Ctx.buffer((Twine(It->newTypeName) + "." + It->newPrintedName).str()));
      return;
    }

    // If a type alias of a raw type has been changed to a struct/enum that
    // conforms to RawRepresentable in the later version of SDK, we show the
    // refine diagnostics message instead of showing the type alias has been
    // removed.
    if (TypeAliasUpdateMap.find((SDKNode*)Node) != TypeAliasUpdateMap.end()) {
      RawRepresentableDecls.Diags.emplace_back(ScreenInfo, Node->getDeclKind(),
        Node->getFullyQualifiedName(),
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
    RemovedDecls.Diags.emplace_back(ScreenInfo,
                                    Node->getDeclKind(),
                                    Node->getFullyQualifiedName(),
                                    Node->isDeprecated());
    return;
  }
  case NodeAnnotation::Rename: {
    auto *Count = UpdateMap.findUpdateCounterpart(Node)->getAs<SDKNodeDecl>();
    RenamedDecls.Diags.emplace_back(ScreenInfo,
                                    Node->getDeclKind(), Count->getDeclKind(),
                                    Node->getFullyQualifiedName(),
                                    Count->getFullyQualifiedName());
    return;
  }
  case NodeAnnotation::NowMutating: {
    AttrChangedDecls.Diags.emplace_back(ScreenInfo,
                                        Node->getDeclKind(),
                                        Node->getFullyQualifiedName(),
                                        Ctx.buffer("mutating"));
    return;
  }
  case NodeAnnotation::NowThrowing: {
    AttrChangedDecls.Diags.emplace_back(ScreenInfo,
                                        Node->getDeclKind(),
                                        Node->getFullyQualifiedName(),
                                        Ctx.buffer("throwing"));
    return;
  }
  case NodeAnnotation::StaticChange: {
    AttrChangedDecls.Diags.emplace_back(ScreenInfo,
                                        Node->getDeclKind(),
                                        Node->getFullyQualifiedName(),
                        Ctx.buffer(Node->isStatic() ? "not static" : "static"));
    return;
  }
  case NodeAnnotation::OwnershipChange: {
    auto getOwnershipDescription = [&](swift::ReferenceOwnership O) {
      switch (O) {
      case ReferenceOwnership::Strong:
        return Ctx.buffer("strong");
      case ReferenceOwnership::Weak:
        return Ctx.buffer("weak");
      case ReferenceOwnership::Unowned:
        return Ctx.buffer("unowned");
      case ReferenceOwnership::Unmanaged:
        return Ctx.buffer("unowned(unsafe)");
      }

      llvm_unreachable("Unhandled Ownership in switch.");
    };
    auto *Count = UpdateMap.findUpdateCounterpart(Node)->getAs<SDKNodeDecl>();
    AttrChangedDecls.Diags.emplace_back(
        ScreenInfo, Node->getDeclKind(), Node->getFullyQualifiedName(),
        getOwnershipDescription(Node->getReferenceOwnership()),
        getOwnershipDescription(Count->getReferenceOwnership()));
    return;
  }
  default:
    return;
  }
}

void DiagnosisEmitter::visitDecl(SDKNodeDecl *Node) {
  if (Node->isSDKPrivate())
    return;
  std::vector<NodeAnnotation> Scratch;
  for (auto Anno : Node->getAnnotations(Scratch))
    handle(Node, Anno);
}

void DiagnosisEmitter::visitType(SDKNodeType *Node) {
  auto *Parent = dyn_cast<SDKNodeDecl>(Node->getParent());
  if (!Parent || Parent->isSDKPrivate())
    return;
  MetaInfo ScreenInfo(Parent);
  SDKContext &Ctx = Node->getSDKContext();
  if (Node->isAnnotatedAs(NodeAnnotation::Updated)) {
    auto *Count = UpdateMap.findUpdateCounterpart(Node)->getAs<SDKNodeType>();
    StringRef Descriptor;
    switch (Parent->getKind()) {
    case SDKNodeKind::DeclConstructor:
    case SDKNodeKind::DeclFunction:
    case SDKNodeKind::DeclVar:
      Descriptor = isa<SDKNodeDeclAbstractFunc>(Parent) ?
        SDKNodeDeclAbstractFunc::getTypeRoleDescription(Ctx, Parent->getChildIndex(Node)) :
        Ctx.buffer("declared");
      if (Node->getPrintedName() != Count->getPrintedName())
        TypeChangedDecls.Diags.emplace_back(ScreenInfo,
                                            Parent->getDeclKind(),
                                            Parent->getFullyQualifiedName(),
                                            Node->getPrintedName(),
                                            Count->getPrintedName(),
                                            Descriptor);
      break;
    default:
      break;
    }
  }
}

void DiagnosisEmitter::visit(NodePtr Node) {
  if (auto *DNode = dyn_cast<SDKNodeDecl>(Node)) {
    visitDecl(DNode);
  }
  if (auto *TNode = dyn_cast<SDKNodeType>(Node)) {
    visitType(TNode);
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

class OverloadMemberFunctionEmitter : public SDKNodeVisitor {

  std::vector<OverloadedFuncInfo> &AllItems;

  void visit(NodePtr Node) override {
    if (Node->getKind() != SDKNodeKind::DeclFunction)
      return;
    auto Parent = Node->getParent();
    if (Parent->getKind() != SDKNodeKind::DeclType)
      return;
    DeclNameViewer CurrentViewer(Node->getPrintedName());
    if (CurrentViewer.args().empty())
      return;
    for (auto &C : Parent->getChildren()) {
      if (C == Node)
        continue;
      if (C->getKind() != SDKNodeKind::DeclFunction)
        continue;
      DeclNameViewer ChildViewer(C->getPrintedName());
      if (ChildViewer.args().empty())
        continue;
      if (CurrentViewer.commonPartsCount(ChildViewer) >=
          CurrentViewer.partsCount() - 1) {
        AllItems.emplace_back(Node->getAs<SDKNodeDecl>()->getUsr());
        return;
      }
    }
  }

  OverloadMemberFunctionEmitter(std::vector<OverloadedFuncInfo> &AllItems) :
    AllItems(AllItems) {}

public:
  static void collectDiffItems(NodePtr Root,
                               std::vector<OverloadedFuncInfo> &AllItems) {
    OverloadMemberFunctionEmitter Emitter(AllItems);
    SDKNode::postorderVisit(Root, Emitter);
  }
};

} // end anonymous namespace

namespace fs = llvm::sys::fs;
namespace path = llvm::sys::path;

struct RenameDetectorForMemberDiff : public MatchedNodeListener {
  void foundMatch(NodePtr Left, NodePtr Right) override {
    detectRename(Left, Right);
  }
  void workOn(NodePtr Left, NodePtr Right) {
    if (Left->getKind() == Right->getKind() &&
        Left->getKind() == SDKNodeKind::DeclType) {
      SameNameNodeMatcher SNMatcher(Left->getChildren(), Right->getChildren(),
                                    *this);
      SNMatcher.match();
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
        rightParent->getAs<SDKNodeDecl>()->getFullyQualifiedName(),
        right->getPrintedName(), findSelfIndex(right), None,
        leftParent->getKind() == SDKNodeKind::Root ?
          StringRef() : leftParent->getAs<SDKNodeDecl>()->getFullyQualifiedName(),
        left->getPrintedName()
    };
    out.emplace_back(item);
    Detector.workOn(left, right);
  }
}

static int diagnoseModuleChange(StringRef LeftPath, StringRef RightPath) {
  if (!fs::exists(LeftPath)) {
    llvm::errs() << LeftPath << " does not exist\n";
    return 1;
  }
  if (!fs::exists(RightPath)) {
    llvm::errs() << RightPath << " does not exist\n";
    return 1;
  }
  SDKContext Ctx;
  SwiftDeclCollector LeftCollector(Ctx);
  LeftCollector.deSerialize(LeftPath);
  SwiftDeclCollector RightCollector(Ctx);
  RightCollector.deSerialize(RightPath);
  auto LeftModule = LeftCollector.getSDKRoot();
  auto RightModule = RightCollector.getSDKRoot();
  TypeAliasDiffFinder(LeftModule, RightModule,
                      Ctx.getTypeAliasUpdateMap()).search();
  PrunePass Prune(Ctx.getNodeUpdateMap());
  Prune.pass(LeftModule, RightModule);
  ChangeRefinementPass RefinementPass(Ctx.getNodeUpdateMap());
  RefinementPass.pass(LeftModule, RightModule);
  // Find member hoist changes to help refine diagnostics.
  findTypeMemberDiffs(LeftModule, RightModule, Ctx.getTypeMemberDiffs());
  DiagnosisEmitter::diagnosis(LeftModule, RightModule, Ctx);
  return 0;
}

static int compareSDKs(StringRef LeftPath, StringRef RightPath,
                       StringRef DiffPath,
                       llvm::StringSet<> &IgnoredRemoveUsrs) {
  if (!fs::exists(LeftPath)) {
    llvm::errs() << LeftPath << " does not exist\n";
    return 1;
  }
  if (!fs::exists(RightPath)) {
    llvm::errs() << RightPath << " does not exist\n";
    return 1;
  }
  llvm::errs() << "Diffing: " << LeftPath << " and " << RightPath << "\n";
  SDKContext Ctx;
  SwiftDeclCollector LeftCollector(Ctx);
  LeftCollector.deSerialize(LeftPath);
  SwiftDeclCollector RightCollector(Ctx);
  RightCollector.deSerialize(RightPath);
  llvm::errs() << "Finished deserializing" << "\n";
  auto LeftModule = LeftCollector.getSDKRoot();
  auto RightModule = RightCollector.getSDKRoot();

  // Structural diffs: not merely name changes but changes in SDK tree
  // structure.
  llvm::errs() << "Detecting type member diffs" << "\n";
  findTypeMemberDiffs(LeftModule, RightModule, Ctx.getTypeMemberDiffs());

  PrunePass Prune(Ctx.getNodeUpdateMap());
  Prune.pass(LeftModule, RightModule);
  llvm::errs() << "Finished pruning" << "\n";
  ChangeRefinementPass RefinementPass(Ctx.getNodeUpdateMap());
  RefinementPass.pass(LeftModule, RightModule);
  DiffVector AllItems;
  DiffItemEmitter::collectDiffItems(LeftModule, AllItems);
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

static std::string getDumpFilePath(StringRef OutputDir, StringRef FileName) {
  std::string Path = OutputDir;
  Path += "/";
  Path += FileName;
  int Suffix = 0;
  auto ConstructPath = [&]() {
    return Path + (Suffix == 0 ? "" : std::to_string(Suffix)) + ".js";
  };
  for (; fs::exists(ConstructPath()); Suffix ++);
  return ConstructPath();
}

static int dumpSwiftModules(const CompilerInvocation &InitInvok,
                            const llvm::StringSet<> &ModuleNames,
                            StringRef OutputDir,
                            const std::vector<std::string> PrintApis) {
  if (!fs::exists(OutputDir)) {
    llvm::errs() << "Output directory '" << OutputDir << "' does not exist.\n";
    return 1;
  }

  std::vector<ModuleDecl*> Modules;
  CompilerInvocation Invocation(InitInvok);
  CompilerInstance CI;
  // Display diagnostics to stderr.
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);
  if (CI.setup(Invocation)) {
    llvm::errs() << "Failed to setup the compiler instance\n";
    return 1;
  }

  auto &Context = CI.getASTContext();

  for (auto &Entry : ModuleNames) {
    StringRef Name = Entry.first();
    if (options::Verbose)
      llvm::errs() << "Loading module: " << Name << "...\n";
    auto *M = Context.getModuleByName(Name);
    if (!M) {
      if (options::Verbose)
        llvm::errs() << "Failed to load module: " << Name << '\n';
      if (options::AbortOnModuleLoadFailure)
        return 1;
    }
    Modules.push_back(M);
  }

  SDKContext Ctx;
  for (auto M : Modules) {
    SwiftDeclCollector Collector(Ctx);
    SmallVector<Decl*, 256> Decls;
    M->getTopLevelDecls(Decls);
    for (auto D : Decls) {
      if (auto VD = dyn_cast<ValueDecl>(D))
        Collector.foundDecl(VD, DeclVisibilityKind::VisibleAtTopLevel);
    }
    std::string Path = getDumpFilePath(OutputDir, M->getName().str());
    Collector.serialize(Path);
    if (options::Verbose)
      llvm::errs() << "Dumped to "<< Path << "\n";
  }
  return 0;
}

static int dumpSDKContent(const CompilerInvocation &InitInvok,
                          const llvm::StringSet<> &ModuleNames,
                          StringRef OutputFile) {
  CompilerInvocation Invocation(InitInvok);

  CompilerInstance CI;
  // Display diagnostics to stderr.
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);
  if (CI.setup(Invocation)) {
    llvm::errs() << "Failed to setup the compiler instance\n";
    return 1;
  }

  auto &Ctx = CI.getASTContext();


  // Load standard library so that Clang importer can use it.
  auto *Stdlib = Ctx.getStdlibModule(/*loadIfAbsent=*/true);
  if (!Stdlib) {
    llvm::errs() << "Failed to load Swift stdlib\n";
    return 1;
  }

  std::vector<ModuleDecl *> Modules;
  for (auto &Entry : ModuleNames) {
    StringRef Name = Entry.getKey();
    if (options::Verbose)
      llvm::errs() << "Loading module: " << Name << "...\n";
    auto *M = Ctx.getModuleByName(Name);
    if (!M) {
      llvm::errs() << "Failed to load module: " << Name << '\n';
      if (options::AbortOnModuleLoadFailure)
        return 1;
    } else {
      Modules.push_back(M);
    }
  }
  if (options::Verbose)
    llvm::errs() << "Scanning symbols...\n";
  SDKContext SDKCtx;
  SwiftDeclCollector Collector(SDKCtx);
  Collector.lookupVisibleDecls(Modules);
  if (options::Verbose)
    llvm::errs() << "Dumping SDK...\n";
  Collector.serialize(OutputFile);
  if (options::Verbose)
    llvm::errs() << "Dumped to "<< OutputFile << "\n";
  return 0;
}

static int readFileLineByLine(StringRef Path, llvm::StringSet<> &Lines) {
  auto FileBufOrErr = llvm::MemoryBuffer::getFile(Path);
  if (!FileBufOrErr) {
    llvm::errs() << "error opening file: "
      << FileBufOrErr.getError().message() << '\n';
    return 1;
  }

  StringRef BufferText = FileBufOrErr.get()->getBuffer();
  while (!BufferText.empty()) {
    StringRef Line;
    std::tie(Line, BufferText) = BufferText.split('\n');
    Line = Line.trim();
    if (!Line.empty())
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

static int prepareForDump(const char *Main,
                          CompilerInvocation &InitInvok,
                          llvm::StringSet<> &Modules) {
  InitInvok.setMainExecutablePath(fs::getMainExecutable(Main,
    reinterpret_cast<void *>(&anchorForGetMainExecutable)));
  InitInvok.setModuleName("swift_ide_test");
  if (!options::SDK.empty()) {
    InitInvok.setSDKPath(options::SDK);
  } else if (const char *SDKROOT = getenv("SDKROOT")) {
    InitInvok.setSDKPath(SDKROOT);
  } else {
    llvm::errs() << "Provide '-sdk <path>' option or run with 'xcrun -sdk <..>\
    swift-api-digester'\n";
    return 1;
  }

  if (!options::Triple.empty())
    InitInvok.setTargetTriple(options::Triple);
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
      return 1;
    }
  }

  if (!options::ResourceDir.empty()) {
    InitInvok.setRuntimeResourcePath(options::ResourceDir);
  }
  std::vector<SearchPathOptions::FrameworkSearchPath> FramePaths;
  for (const auto &path : options::FrameworkPaths) {
    FramePaths.push_back({path, /*isSystem=*/false});
  }
  for (const auto &path : options::CCSystemFrameworkPaths) {
    FramePaths.push_back({path, /*isSystem=*/true});
  }
  InitInvok.setFrameworkSearchPaths(FramePaths);
  InitInvok.setImportSearchPaths(options::ModuleInputPaths);

  if (!options::ModuleList.empty()) {
    if (readFileLineByLine(options::ModuleList, Modules))
        return 1;
  }

  for (auto M : options::ModuleNames) {
    Modules.insert(M);
  }

  if (Modules.empty()) {
    llvm::errs() << "Need to specify -include-all or -module <name>\n";
    return 1;
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

static int deserializeDiffItems(StringRef DiffPath, StringRef OutputPath) {
  APIDiffItemStore Store;
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
        DeclNameViewer Viewer(NewName);
        auto HasUnderScore =
          [](StringRef S) { return S.find('_') != StringRef::npos; };
        auto Args = Viewer.args();
        if (HasUnderScore(Viewer.base()) ||
            std::any_of(Args.begin(), Args.end(), HasUnderScore)) {
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

/// Mostly for testing purposes, this function de-serializes the SDK dump in
/// dumpPath and re-serialize them to OutputPath. If the tool performs correctly,
/// the contents in dumpPath and OutputPath should be identical.
static int deserializeSDKDump(StringRef dumpPath, StringRef OutputPath) {
  std::error_code EC;
  llvm::raw_fd_ostream FS(OutputPath, EC, llvm::sys::fs::F_None);
  if (!fs::exists(dumpPath)) {
    llvm::errs() << dumpPath << " does not exist\n";
    return 1;
  }
  SDKContext Ctx;
  SwiftDeclCollector Collector(Ctx);
  Collector.deSerialize(dumpPath);
  Collector.serialize(OutputPath);
  return 0;
}

int main(int argc, char *argv[]) {
  PROGRAM_START(argc, argv);
  INITIALIZE_LLVM();

  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift SDK Digester\n");
  CompilerInvocation InitInvok;

  ClangImporterOptions &ImporterOpts = InitInvok.getClangImporterOptions();
  ImporterOpts.DetailedPreprocessingRecord = true;

  llvm::StringSet<> Modules;
  std::vector<std::string> PrintApis;
  llvm::StringSet<> IgnoredUsrs;
  readIgnoredUsrs(IgnoredUsrs);
  for (auto Name : options::ApisPrintUsrs)
    PrintApis.push_back(Name);
  switch (options::Action) {
  case ActionType::DumpSwiftModules:
    return (prepareForDump(argv[0], InitInvok, Modules)) ? 1 :
      dumpSwiftModules(InitInvok, Modules, options::OutputFile, PrintApis);
  case ActionType::DumpSDK:
    return (prepareForDump(argv[0], InitInvok, Modules)) ? 1 :
      dumpSDKContent(InitInvok, Modules, options::OutputFile);
  case ActionType::CompareSDKs:
  case ActionType::DiagnoseSDKs:
    if (options::SDKJsonPaths.size() != 2) {
      llvm::errs() << "Only two SDK versions can be compared\n";
      llvm::cl::PrintHelpMessage();
      return 1;
    }
    if (options::Action == ActionType::CompareSDKs)
      return compareSDKs(options::SDKJsonPaths[0], options::SDKJsonPaths[1],
                         options::OutputFile, IgnoredUsrs);
    else
      return diagnoseModuleChange(options::SDKJsonPaths[0],
                                  options::SDKJsonPaths[1]);
  case ActionType::DeserializeSDK:
  case ActionType::DeserializeDiffItems: {
    if (options::SDKJsonPaths.size() != 1) {
      llvm::cl::PrintHelpMessage();
      return 1;
    }
    if (options::Action == ActionType::DeserializeDiffItems)
      return deserializeDiffItems(options::SDKJsonPaths[0], options::OutputFile);
    else
      return deserializeSDKDump(options::SDKJsonPaths[0], options::OutputFile);
  }
  case ActionType::GenerateNameCorrectionTemplate: {
    APIDiffItemStore Store;
    auto &Paths = options::SDKJsonPaths;
    for (unsigned I = 0; I < Paths.size(); I ++)
      Store.addStorePath(Paths[I]);
    return deserializeNameCorrection(Store, options::OutputFile);
  }
  case ActionType::None:
    llvm::errs() << "Action required\n";
    llvm::cl::PrintHelpMessage();
    return 1;
  }
}
