#include <ModuleAnalyzerNodes.h>
#include <algorithm>

using namespace swift;
using namespace ide;
using namespace api;

namespace fs = llvm::sys::fs;
namespace path = llvm::sys::path;

namespace {
static StringRef getAttrName(DeclAttrKind Kind) {
  switch (Kind) {
#define DECL_ATTR(NAME, CLASS, ...)                                           \
  case DAK_##CLASS:                                                           \
      return DeclAttribute::isDeclModifier(DAK_##CLASS) ? #NAME : "@"#NAME;
#include "swift/AST/Attr.def"
  case DAK_Count:
    llvm_unreachable("unrecognized attribute kind.");
  }
}
} // End of anonymous namespace.

struct swift::ide::api::SDKNodeInitInfo {
  SDKContext &Ctx;
  DeclKind DKind;

#define KEY_STRING(X, Y) StringRef X;
#include "swift/IDE/DigesterEnums.def"
#define KEY_BOOL(X, Y) bool X = false;
#include "swift/IDE/DigesterEnums.def"
#define KEY_UINT(X, Y) Optional<uint8_t> X;
#include "swift/IDE/DigesterEnums.def"
#define KEY_STRING_ARR(X, Y) std::vector<StringRef> X;
#include "swift/IDE/DigesterEnums.def"

  ReferenceOwnership ReferenceOwnership = ReferenceOwnership::Strong;
  std::vector<DeclAttrKind> DeclAttrs;
  std::vector<TypeAttrKind> TypeAttrs;

  SDKNodeInitInfo(SDKContext &Ctx) : Ctx(Ctx) {}
  SDKNodeInitInfo(SDKContext &Ctx, Decl *D);
  SDKNodeInitInfo(SDKContext &Ctx, ValueDecl *VD);
  SDKNodeInitInfo(SDKContext &Ctx, OperatorDecl *D);
  SDKNodeInitInfo(SDKContext &Ctx, ProtocolConformance *Conform);
  SDKNodeInitInfo(SDKContext &Ctx, Type Ty, TypeInitInfo Info = TypeInitInfo());
  SDKNode* createSDKNode(SDKNodeKind Kind);
};

SDKContext::SDKContext(CheckerOptions Opts): Diags(SourceMgr), Opts(Opts) {
#define ADD(NAME) BreakingAttrs.push_back({DeclAttrKind::DAK_##NAME, \
      getAttrName(DeclAttrKind::DAK_##NAME)});
  // Add attributes that both break ABI and API.
  ADD(Final)
  if (checkingABI()) {
    // Add ABI-breaking-specific attributes.
    ADD(ObjC)
    ADD(FixedLayout)
    ADD(Frozen)
    ADD(Dynamic)
  }
#undef ADD
}

void SDKNodeRoot::registerDescendant(SDKNode *D) {
  // Operator doesn't have usr
  if (isa<SDKNodeDeclOperator>(D))
    return;
  if (auto DD = dyn_cast<SDKNodeDecl>(D)) {
    assert(!DD->getUsr().empty());
    DescendantDeclTable[DD->getUsr()].insert(DD);
  }
}

SDKNode::SDKNode(SDKNodeInitInfo Info, SDKNodeKind Kind): Ctx(Info.Ctx),
  Name(Info.Name), PrintedName(Info.PrintedName), TheKind(unsigned(Kind)) {}

SDKNodeRoot::SDKNodeRoot(SDKNodeInitInfo Info): SDKNode(Info, SDKNodeKind::Root) {}

SDKNodeDecl::SDKNodeDecl(SDKNodeInitInfo Info, SDKNodeKind Kind)
      : SDKNode(Info, Kind), DKind(Info.DKind), Usr(Info.Usr),
        Location(Info.Location), ModuleName(Info.ModuleName),
        DeclAttributes(Info.DeclAttrs), IsImplicit(Info.IsImplicit),
        IsStatic(Info.IsStatic), IsDeprecated(Info.IsDeprecated),
        IsProtocolReq(Info.IsProtocolReq),
        IsOverriding(Info.IsOverriding),
        IsOpen(Info.IsOpen),
        IsInternal(Info.IsInternal),
        ReferenceOwnership(uint8_t(Info.ReferenceOwnership)),
        GenericSig(Info.GenericSig), FixedBinaryOrder(Info.FixedBinaryOrder) {}

SDKNodeType::SDKNodeType(SDKNodeInitInfo Info, SDKNodeKind Kind):
  SDKNode(Info, Kind), TypeAttributes(Info.TypeAttrs),
  HasDefaultArg(Info.HasDefaultArg),
  ParamValueOwnership(Info.ParamValueOwnership) {}

SDKNodeTypeNominal::SDKNodeTypeNominal(SDKNodeInitInfo Info):
  SDKNodeType(Info, SDKNodeKind::TypeNominal), USR(Info.Usr) {}

SDKNodeTypeFunc::SDKNodeTypeFunc(SDKNodeInitInfo Info):
  SDKNodeType(Info, SDKNodeKind::TypeFunc) {}

SDKNodeTypeAlias::SDKNodeTypeAlias(SDKNodeInitInfo Info):
  SDKNodeType(Info, SDKNodeKind::TypeAlias) {}

SDKNodeDeclType::SDKNodeDeclType(SDKNodeInitInfo Info):
  SDKNodeDecl(Info, SDKNodeKind::DeclType), SuperclassUsr(Info.SuperclassUsr),
  SuperclassNames(Info.SuperclassNames),
  EnumRawTypeName(Info.EnumRawTypeName) {}

SDKNodeConformance::SDKNodeConformance(SDKNodeInitInfo Info):
  SDKNode(Info, SDKNodeKind::Conformance) {}

SDKNodeTypeWitness::SDKNodeTypeWitness(SDKNodeInitInfo Info):
  SDKNode(Info, SDKNodeKind::TypeWitness) {}

SDKNodeDeclOperator::SDKNodeDeclOperator(SDKNodeInitInfo Info):
  SDKNodeDecl(Info, SDKNodeKind::DeclOperator) {}

SDKNodeDeclTypeAlias::SDKNodeDeclTypeAlias(SDKNodeInitInfo Info):
  SDKNodeDecl(Info, SDKNodeKind::DeclTypeAlias) {}

SDKNodeDeclVar::SDKNodeDeclVar(SDKNodeInitInfo Info): 
  SDKNodeDecl(Info, SDKNodeKind::DeclVar), IsLet(Info.IsLet),
  HasStorage(Info.HasStorage), HasDidSet(Info.HasDidset),
  HasWillSet(Info.HasWillset) {}

SDKNodeDeclAbstractFunc::SDKNodeDeclAbstractFunc(SDKNodeInitInfo Info,
  SDKNodeKind Kind): SDKNodeDecl(Info, Kind), IsThrowing(Info.IsThrowing),
                     SelfIndex(Info.SelfIndex) {}

SDKNodeDeclFunction::SDKNodeDeclFunction(SDKNodeInitInfo Info):
  SDKNodeDeclAbstractFunc(Info, SDKNodeKind::DeclFunction),
  FuncSelfKind(Info.FuncSelfKind) {}

SDKNodeDeclConstructor::SDKNodeDeclConstructor(SDKNodeInitInfo Info):
  SDKNodeDeclAbstractFunc(Info, SDKNodeKind::DeclConstructor) {}

SDKNodeDeclGetter::SDKNodeDeclGetter(SDKNodeInitInfo Info): 
  SDKNodeDeclAbstractFunc(Info, SDKNodeKind::DeclGetter) {}

SDKNodeDeclSetter::SDKNodeDeclSetter(SDKNodeInitInfo Info):
  SDKNodeDeclAbstractFunc(Info, SDKNodeKind::DeclSetter) {}

SDKNodeDeclAssociatedType::SDKNodeDeclAssociatedType(SDKNodeInitInfo Info):
  SDKNodeDecl(Info, SDKNodeKind::DeclAssociatedType) {};

SDKNodeDeclSubscript::SDKNodeDeclSubscript(SDKNodeInitInfo Info):
  SDKNodeDeclAbstractFunc(Info, SDKNodeKind::DeclSubscript),
  HasSetter(Info.HasSetter), HasStorage(Info.HasStorage),
  HasDidSet(Info.HasDidset), HasWillSet(Info.HasWillset) {}

StringRef SDKNodeDecl::getHeaderName() const {
  if (Location.empty())
    return StringRef();
  return llvm::sys::path::filename(Location.split(":").first);
}

SDKNodeDeclGetter *SDKNodeDeclVar::getGetter() const {
  if (getChildrenCount() > 1)
    return cast<SDKNodeDeclGetter>(childAt(1));
  return nullptr;
}

SDKNodeDeclSetter *SDKNodeDeclVar::getSetter() const {
  if (getChildrenCount() > 2)
    return cast<SDKNodeDeclSetter>(childAt(2));
  return nullptr;
}

SDKNodeType *SDKNodeDeclVar::getType() const {
  return cast<SDKNodeType>(childAt(0));
}

NodePtr UpdatedNodesMap::findUpdateCounterpart(const SDKNode *Node) const {
  assert(Node->isAnnotatedAs(NodeAnnotation::Updated) && "Not update operation.");
  auto FoundPair = std::find_if(MapImpl.begin(), MapImpl.end(),
                      [&](std::pair<NodePtr, NodePtr> Pair) {
    return Pair.second == Node || Pair.first == Node;
  });
  assert(FoundPair != MapImpl.end() && "Cannot find update counterpart.");
  return Node == FoundPair->first ? FoundPair->second : FoundPair->first;
}

#define NODE_KIND_RANGE(ID, FIRST, LAST)                                      \
bool SDKNode##ID::classof(const SDKNode *N) {                                 \
  return N->getKind() >= SDKNodeKind::FIRST &&                                \
    N->getKind() <= SDKNodeKind::LAST;                                        \
}
#include "swift/IDE/DigesterEnums.def"

unsigned SDKNode::getChildIndex(const SDKNode* Child) const {
  auto It = std::find(Children.begin(), Children.end(), Child);
  assert(It != Children.end() && "cannot find the child");
  return It - Children.begin();
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

NodePtr SDKNode::childAt(unsigned I) const {
  assert(I < getChildrenCount());
  return getChildren()[I];
}

void SDKNode::removeChild(NodePtr C) {
  Children.erase(std::find(Children.begin(), Children.end(), C));
}

void SDKNode::annotate(NodeAnnotation Anno, StringRef Comment) {
  assert(!Comment.empty());
  if(isAnnotatedAs(Anno))
    return;
  annotate(Anno);
  AnnotateComments[Anno] = Comment;
}

void SDKNode::removeAnnotate(NodeAnnotation Anno) {
  assert(isAnnotatedAs(Anno));
  Annotations.erase(Anno);
  AnnotateComments.erase(Anno);
  assert(!isAnnotatedAs(Anno));
  assert(AnnotateComments.count(Anno) == 0);
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

StringRef SDKNodeType::getParamValueOwnership() const {
  return ParamValueOwnership.empty() ? "Default" : ParamValueOwnership;
}

StringRef SDKNodeType::getTypeRoleDescription() const {
  assert(isTopLevelType());
  auto P = cast<SDKNodeDecl>(getParent());
  switch(P->getKind()) {
  case SDKNodeKind::Root:
  case SDKNodeKind::TypeNominal:
  case SDKNodeKind::TypeFunc:
  case SDKNodeKind::TypeAlias:
  case SDKNodeKind::DeclType:
  case SDKNodeKind::DeclOperator:
  case SDKNodeKind::Conformance:
  case SDKNodeKind::TypeWitness:
    llvm_unreachable("Type Parent is wrong");
  case SDKNodeKind::DeclFunction:
  case SDKNodeKind::DeclConstructor:
  case SDKNodeKind::DeclGetter:
  case SDKNodeKind::DeclSetter:
  case SDKNodeKind::DeclSubscript:
    return SDKNodeDeclAbstractFunc::getTypeRoleDescription(Ctx,
      P->getChildIndex(this));
  case SDKNodeKind::DeclVar:
    return "declared";
  case SDKNodeKind::DeclTypeAlias:
    return "underlying";
  case SDKNodeKind::DeclAssociatedType:
    return "default";
  }
}

SDKNode *SDKNodeRoot::getInstance(SDKContext &Ctx) {
  SDKNodeInitInfo Info(Ctx);
  Info.Name = Ctx.buffer("TopLevel");
  Info.PrintedName = Ctx.buffer("TopLevel");
  return Info.createSDKNode(SDKNodeKind::Root);
}

StringRef SDKNodeDecl::getScreenInfo() const {
  auto ModuleName = getModuleName();
  auto HeaderName = getHeaderName();
  auto &Ctx = getSDKContext();
  llvm::SmallString<64> SS;
  llvm::raw_svector_ostream OS(SS);
  if (Ctx.getOpts().PrintModule)
    OS << ModuleName;
  if (!HeaderName.empty())
    OS << "(" << HeaderName << ")";
  if (!OS.str().empty())
    OS << ": ";
  OS << getDeclKind() << " " << getFullyQualifiedName();
  return Ctx.buffer(OS.str());
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

bool SDKNodeDecl::hasDeclAttribute(DeclAttrKind DAKind) const {
  return std::find(DeclAttributes.begin(), DeclAttributes.end(), DAKind) !=
    DeclAttributes.end();
}

ArrayRef<DeclAttrKind> SDKNodeDecl::getDeclAttributes() const {
  return llvm::makeArrayRef(DeclAttributes.data(), DeclAttributes.size());
}

bool SDKNodeDecl::hasAttributeChange(const SDKNodeDecl &Another) const {
  std::set<DeclAttrKind> Left(getDeclAttributes().begin(),
                              getDeclAttributes().end());
  std::set<DeclAttrKind> Right(Another.getDeclAttributes().begin(),
                               Another.getDeclAttributes().end());
  return Left != Right;
}

bool SDKNodeType::hasAttributeChange(const SDKNodeType &Another) const {
  std::set<TypeAttrKind> Left(getTypeAttributes().begin(),
                              getTypeAttributes().end());
  std::set<TypeAttrKind> Right(Another.getTypeAttributes().begin(),
                               Another.getTypeAttributes().end());
  return Left != Right;
}

SDKNodeDecl *SDKNodeType::getClosestParentDecl() const {
  auto *Result = getParent();
  for (; !isa<SDKNodeDecl>(Result); Result = Result->getParent());
  return Result->getAs<SDKNodeDecl>();
}

void SDKNodeDeclType::addConformance(SDKNode *Conf) {
  cast<SDKNodeConformance>(Conf)->TypeDecl = this;
  Conformances.push_back(Conf);
}

SDKNodeType *SDKNodeTypeWitness::getUnderlyingType() const {
  return getOnlyChild()->getAs<SDKNodeType>();
}

Optional<SDKNodeDeclType*> SDKNodeDeclType::getSuperclass() const {
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
Optional<SDKNodeDecl*>
SDKNodeDeclType::lookupChildByPrintedName(StringRef Name) const {
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

SDKNodeType *SDKNodeDeclType::getRawValueType() const {
  if (isConformingTo(KnownProtocolKind::RawRepresentable)) {
    if (auto RV = lookupChildByPrintedName("rawValue")) {
      return (*(*RV)->getChildBegin())->getAs<SDKNodeType>();
    }
  }
  return nullptr;
}

bool SDKNodeDeclType::isConformingTo(KnownProtocolKind Kind) const {
  switch (Kind) {
#define KNOWN_PROTOCOL(NAME)                                                \
    case KnownProtocolKind::NAME:                                           \
      return std::find_if(Conformances.begin(), Conformances.end(),         \
        [](SDKNode *Conf) { return Conf->getName() == #NAME; }) !=          \
          Conformances.end();
#include "swift/IDE/DigesterEnums.def"
  }
}

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

#define NODE_KIND(X, NAME)                                                 \
  bool SDKNode##X::classof(const SDKNode *N) {                             \
    return N->getKind() == SDKNodeKind::X;                                 \
  }
#include "swift/IDE/DigesterEnums.def"

static Optional<KeyKind> parseKeyKind(StringRef Content) {
  return llvm::StringSwitch<Optional<KeyKind>>(Content)
#define KEY(NAME) .Case(#NAME, KeyKind::KK_##NAME)
#include "swift/IDE/DigesterEnums.def"
    .Default(None)
  ;
}

static StringRef getKeyContent(SDKContext &Ctx, KeyKind Kind) {
  switch (Kind) {
#define KEY(NAME) case KeyKind::KK_##NAME: return Ctx.buffer(#NAME);
#include "swift/IDE/DigesterEnums.def"
  }
  llvm_unreachable("Unhandled KeyKind in switch.");
}

SDKNode* SDKNode::constructSDKNode(SDKContext &Ctx,
                                   llvm::yaml::MappingNode *Node) {
  static auto GetScalarString = [&](llvm::yaml::Node *N) -> StringRef {
    auto WithQuote = cast<llvm::yaml::ScalarNode>(N)->getRawValue();
    return WithQuote.substr(1, WithQuote.size() - 2);
  };

  static auto getAsInt = [&](llvm::yaml::Node *N) -> int {
    return std::stoi(cast<llvm::yaml::ScalarNode>(N)->getRawValue());
  };
  static auto getAsBool = [&](llvm::yaml::Node *N) -> bool {
    auto txt = cast<llvm::yaml::ScalarNode>(N)->getRawValue();
    assert(txt.startswith("false") || txt.startswith("true"));
    return txt.startswith("true");
  };
  SDKNodeKind Kind;
  SDKNodeInitInfo Info(Ctx);
  NodeVector Children;
  NodeVector Conformances;

  for (auto &Pair : *Node) {
    auto keyString = GetScalarString(Pair.getKey()); 
    if (auto keyKind = parseKeyKind(keyString)) {
      switch(*keyKind) {
      case KeyKind::KK_kind:
        if (auto parsedKind = parseSDKNodeKind(GetScalarString(Pair.getValue()))) {
          Kind = *parsedKind;
        } else {
          Ctx.diagnose(Pair.getValue(), diag::sdk_node_unrecognized_node_kind,
                       GetScalarString(Pair.getValue()));
        }
        break;
#define KEY_UINT(X, Y)                                                        \
        case KeyKind::KK_##Y: Info.X = getAsInt(Pair.getValue()); break;
#include "swift/IDE/DigesterEnums.def"
#define KEY_STRING(X, Y)                                                      \
  case KeyKind::KK_##Y: Info.X = GetScalarString(Pair.getValue()); break;
#include "swift/IDE/DigesterEnums.def"
#define KEY_BOOL(X, Y) case KeyKind::KK_##Y: Info.X = getAsBool(Pair.getValue()); break;
#include "swift/IDE/DigesterEnums.def"
      case KeyKind::KK_children:
        for (auto &Mapping : *cast<llvm::yaml::SequenceNode>(Pair.getValue())) {
          Children.push_back(constructSDKNode(Ctx,
                                        cast<llvm::yaml::MappingNode>(&Mapping)));
        }
        break;
      case KeyKind::KK_conformances:
        for (auto &Mapping : *cast<llvm::yaml::SequenceNode>(Pair.getValue())) {
          Conformances.push_back(constructSDKNode(Ctx,
                                    cast<llvm::yaml::MappingNode>(&Mapping)));
        }
        break;
#define KEY_STRING_ARR(X, Y)                                                  \
      case KeyKind::KK_##Y:                                                   \
        assert(Info.X.empty());                                               \
        for (auto &Name : *cast<llvm::yaml::SequenceNode>(Pair.getValue())) { \
          Info.X.push_back(GetScalarString(&Name));                           \
        }                                                                     \
        break;
#include "swift/IDE/DigesterEnums.def"
      case KeyKind::KK_ownership:
        Info.ReferenceOwnership =
            swift::ReferenceOwnership(getAsInt(Pair.getValue()));
        assert(Info.ReferenceOwnership != swift::ReferenceOwnership::Strong &&
               "Strong is implied.");
        break;

      case KeyKind::KK_typeAttributes: {
        auto *Seq = cast<llvm::yaml::SequenceNode>(Pair.getValue());
        std::transform(Seq->begin(), Seq->end(),
                       std::back_inserter(Info.TypeAttrs),
          [&](llvm::yaml::Node &N) {
            auto Result = llvm::StringSwitch<TypeAttrKind>(GetScalarString(&N))
  #define TYPE_ATTR(X) .Case(#X, TypeAttrKind::TAK_##X)
  #include "swift/AST/Attr.def"
            .Default(TypeAttrKind::TAK_Count);
            if (Result == TAK_Count)
              Ctx.diagnose(&N, diag::sdk_node_unrecognized_type_attr_kind,
                           GetScalarString(&N));
            return Result;
          });
        break;
      }
      case KeyKind::KK_declAttributes: {
        auto *Seq = cast<llvm::yaml::SequenceNode>(Pair.getValue());
        std::transform(Seq->begin(), Seq->end(), std::back_inserter(Info.DeclAttrs),
          [&](llvm::yaml::Node &N) {
            auto Result = llvm::StringSwitch<DeclAttrKind>(GetScalarString(&N))
  #define DECL_ATTR(_, NAME, ...) .Case(#NAME, DeclAttrKind::DAK_##NAME)
  #include "swift/AST/Attr.def"
            .Default(DeclAttrKind::DAK_Count);
            if (Result == DAK_Count)
              Ctx.diagnose(&N, diag::sdk_node_unrecognized_decl_attr_kind,
                           GetScalarString(&N));
            return Result;
          });
        break;
      }
      case KeyKind::KK_declKind: {
        auto dKind = llvm::StringSwitch<Optional<DeclKind>>(
          GetScalarString(Pair.getValue()))
  #define DECL(X, PARENT) .Case(#X, DeclKind::X)
  #include "swift/AST/DeclNodes.def"
        .Default(None);
        if (dKind)
          Info.DKind = *dKind;
        else
          Ctx.diagnose(Pair.getValue(), diag::sdk_node_unrecognized_decl_kind,
                       GetScalarString(Pair.getValue()));
        break;
      }
      }
    }
    else {
      Ctx.diagnose(Pair.getKey(), diag::sdk_node_unrecognized_key,
                              keyString);
      Pair.skip();
    }
  };
  SDKNode *Result = Info.createSDKNode(Kind);
  for (auto C : Children) {
    Result->addChild(C);
  }
  for (auto *Conf: Conformances) {
    cast<SDKNodeDeclType>(Result)->addConformance(Conf);
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

void swift::ide::api::nodeSetDifference(ArrayRef<SDKNode*> Left,
                                        ArrayRef<SDKNode*> Right,
                                        NodeVector &LeftMinusRight,
                                        NodeVector &RightMinusLeft) {
  llvm::SmallPtrSet<NodePtr, 16> LeftToRemove;
  llvm::SmallPtrSet<NodePtr, 16> RightToRemove;
  for (auto LC: Left) {
    for (auto RC: Right) {
      if (!RightToRemove.count(RC) && *LC == *RC) {
        LeftToRemove.insert(LC);
        RightToRemove.insert(RC);
        break;
      }
    }
  }
  std::for_each(Left.begin(), Left.end(), [&] (SDKNode *N) {
    if (!LeftToRemove.count(N))
      LeftMinusRight.push_back(N);
  });
  std::for_each(Right.begin(), Right.end(), [&] (SDKNode *N) {
    if (!RightToRemove.count(N))
      RightMinusLeft.push_back(N);
  });
}


static bool hasSameContents(ArrayRef<SDKNode*> Left,
                            ArrayRef<SDKNode*> Right) {
  NodeVector LeftMinusRight, RightMinusLeft;
  nodeSetDifference(Left, Right, LeftMinusRight, RightMinusLeft);
  return LeftMinusRight.empty() && RightMinusLeft.empty();
}

static bool hasSameParameterFlags(const SDKNodeType *Left, const SDKNodeType *Right) {
  if (Left->hasDefaultArgument() != Right->hasDefaultArgument())
    return false;
  if (Left->getParamValueOwnership() != Right->getParamValueOwnership())
    return false;

  return true;
}

static bool isSDKNodeEqual(SDKContext &Ctx, const SDKNode &L, const SDKNode &R) {
  auto *LeftAlias = dyn_cast<SDKNodeTypeAlias>(&L);
  auto *RightAlias = dyn_cast<SDKNodeTypeAlias>(&R);
  if (LeftAlias || RightAlias) {
    auto Left = L.getAs<SDKNodeType>();
    auto Right = R.getAs<SDKNodeType>();

    // First compare the parameter attributes.
    if (!hasSameParameterFlags(Left, Right))
      return false;

    // Comparing the underlying types if any of the inputs are alias.
    Left = LeftAlias ? LeftAlias->getUnderlyingType() : Left;
    Right = RightAlias ? RightAlias->getUnderlyingType() : Right;
    return *Left == *Right;
  }

  if (L.getKind() != R.getKind())
    return false;

  switch(L.getKind()) {
    case SDKNodeKind::TypeAlias:
      llvm_unreachable("Should be handled above.");
    case SDKNodeKind::TypeNominal:
    case SDKNodeKind::TypeFunc: {
      auto Left = L.getAs<SDKNodeType>();
      auto Right = R.getAs<SDKNodeType>();
      if (Left->hasAttributeChange(*Right))
        return false;
      if (!hasSameParameterFlags(Left, Right))
        return false;
      if (Left->getPrintedName() == Right->getPrintedName())
        return true;
      return Left->getName() == Right->getName() &&
        Left->hasSameChildren(*Right);
    }

    case SDKNodeKind::DeclFunction: {
      auto Left = L.getAs<SDKNodeDeclFunction>();
      auto Right = R.getAs<SDKNodeDeclFunction>();
      if (Left->getSelfAccessKind() != Right->getSelfAccessKind())
        return false;
      LLVM_FALLTHROUGH;
    }
    case SDKNodeKind::DeclConstructor:
    case SDKNodeKind::DeclGetter:
    case SDKNodeKind::DeclSetter: {
      auto Left = L.getAs<SDKNodeDeclAbstractFunc>();
      auto Right = R.getAs<SDKNodeDeclAbstractFunc>();
      if (Left->isThrowing() ^ Right->isThrowing())
        return false;
      LLVM_FALLTHROUGH;
    }
    case SDKNodeKind::DeclVar: {
      if (Ctx.checkingABI()) {
        // If we're checking ABI, the definition order matters.
        // If they're both members for fixed layout types, we never consider
        // them equal because we need to check definition orders.
        if (auto *LV = dyn_cast<SDKNodeDeclVar>(&L)) {
          if (auto *RV = dyn_cast<SDKNodeDeclVar>(&R)) {
            if (LV->isLet() != RV->isLet())
              return false;
            if (LV->hasStorage() != RV->hasStorage())
              return false;
          }
        }
      }
      LLVM_FALLTHROUGH;
    }
    case SDKNodeKind::DeclType: {
      auto *Left = dyn_cast<SDKNodeDeclType>(&L);
      auto *Right = dyn_cast<SDKNodeDeclType>(&R);
      if (Left && Right) {
        if (!hasSameContents(Left->getConformances(), Right->getConformances())) {
          return false;
        }
        if (Left->getSuperClassName() != Right->getSuperClassName()) {
          return false;
        }
        if (Left->getDeclKind() != Right->getDeclKind()) {
          return false;
        }
      }
      LLVM_FALLTHROUGH;
    }
    case SDKNodeKind::DeclAssociatedType:
    case SDKNodeKind::DeclSubscript: {
      if (auto *Left = dyn_cast<SDKNodeDeclSubscript>(&L)) {
        if (auto *Right = dyn_cast<SDKNodeDeclSubscript>(&R)) {
          if (Left->hasSetter() != Right->hasSetter())
            return false;
        }
      }
      LLVM_FALLTHROUGH;
    }
    case SDKNodeKind::DeclOperator:
    case SDKNodeKind::DeclTypeAlias: {
      auto Left = L.getAs<SDKNodeDecl>();
      auto Right = R.getAs<SDKNodeDecl>();
      if (Left->isStatic() ^ Right->isStatic())
        return false;
      if (Left->getReferenceOwnership() != Right->getReferenceOwnership())
        return false;
      if (Left->hasAttributeChange(*Right))
        return false;
      if (Left->getGenericSignature() != Right->getGenericSignature())
        return false;
      if (Left->isOpen() != Right->isOpen())
        return false;
      if (Left->isInternal() != Right->isInternal())
        return false;
      if (Left->hasFixedBinaryOrder() != Right->hasFixedBinaryOrder())
        return false;
      if (Left->hasFixedBinaryOrder()) {
        if (Left->getFixedBinaryOrder() != Right->getFixedBinaryOrder())
          return false;
      }
      LLVM_FALLTHROUGH;
    }
    case SDKNodeKind::Conformance:
    case SDKNodeKind::TypeWitness:
    case SDKNodeKind::Root: {
      return L.getPrintedName() == R.getPrintedName() &&
        L.hasSameChildren(R);
    }
  }

  llvm_unreachable("Unhandled SDKNodeKind in switch.");
}

bool SDKContext::isEqual(const SDKNode &Left, const SDKNode &Right) {
  if (!EqualCache[&Left].count(&Right)) {
    EqualCache[&Left][&Right] = isSDKNodeEqual(*this, Left, Right);
  }
  return EqualCache[&Left][&Right];
}

AccessLevel SDKContext::getAccessLevel(const ValueDecl *VD) const {
  return checkingABI() ? VD->getEffectiveAccess() : VD->getFormalAccess();
}

bool SDKNode::operator==(const SDKNode &Other) const {
  return Ctx.isEqual(*this, Other);
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
                                bool IsImplicitlyUnwrappedOptional = false) {
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

static StringRef calculateLocation(SDKContext &SDKCtx, Decl *D) {
  if (SDKCtx.getOpts().AvoidLocation)
    return StringRef();
  auto &Ctx = D->getASTContext();
  auto &Importer = static_cast<ClangImporter &>(*Ctx.getClangModuleLoader());

  clang::SourceManager &SM = Importer.getClangPreprocessor().getSourceManager();
  if (ClangNode CN = D->getClangNode()) {
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
  DeclName DM = VD->getFullName();
  if (isa<AbstractFunctionDecl>(VD) || isa<SubscriptDecl>(VD)) {
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
  Result.append(getEscapedName(DM.getBaseName()));
  return Ctx.buffer(Result.str());
}

static bool isFuncThrowing(ValueDecl *VD) {
  if (auto AF = dyn_cast<AbstractFunctionDecl>(VD)) {
    return AF->hasThrows();
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

// Get a requirement with all types canonicalized.
Requirement getCanonicalRequirement(Requirement &Req) {
  auto kind = Req.getKind();
  if (kind == RequirementKind::Layout) {
    return Requirement(kind, Req.getFirstType()->getCanonicalType(),
                       Req.getLayoutConstraint());
  } else {
    return Requirement(kind, Req.getFirstType()->getCanonicalType(),
                       Req.getSecondType()->getCanonicalType());
  }
}

static StringRef printGenericSignature(SDKContext &Ctx, Decl *D) {
  llvm::SmallString<32> Result;
  llvm::raw_svector_ostream OS(Result);
  if (auto *PD = dyn_cast<ProtocolDecl>(D)) {
    if (PD->getRequirementSignature().empty())
      return StringRef();
    OS << "<";
    bool First = true;
    for (auto Req: PD->getRequirementSignature()) {
      if (!First) {
        OS << ", ";
      } else {
        First = false;
      }
      if (Ctx.checkingABI())
        getCanonicalRequirement(Req).print(OS, PrintOptions::printInterface());
      else
        Req.print(OS, PrintOptions::printInterface());
    }
    OS << ">";
    return Ctx.buffer(OS.str());
  }

  if (auto *GC = D->getAsGenericContext()) {
    if (auto *Sig = GC->getGenericSignature()) {
      if (Ctx.checkingABI())
        Sig->getCanonicalSignature()->print(OS);
      else
        Sig->print(OS);
      return Ctx.buffer(OS.str());
    }
  }
  return StringRef();
}

static Optional<uint8_t> getSimilarMemberCount(NominalTypeDecl *NTD,
                                               ValueDecl *VD,
                                        llvm::function_ref<bool(Decl*)> Check) {
  if (!Check(VD))
    return None;
  auto Members = NTD->getMembers();
  auto End = std::find(Members.begin(), Members.end(), VD);
  assert(End != Members.end());
  return std::count_if(Members.begin(), End, Check);
}

Optional<uint8_t> SDKContext::getFixedBinaryOrder(ValueDecl *VD) const {
  // We don't need fixed binary order when checking API stability.
  if (!checkingABI())
    return None;
  auto *NTD = dyn_cast_or_null<NominalTypeDecl>(VD->getDeclContext()->
    getAsDecl());

  if (!NTD || isa<ProtocolDecl>(NTD) || NTD->isResilient())
    return None;

  // The relative order of stored properties matters for non-resilient type.
  auto isStored = [](Decl *M) {
    if (auto *STD = dyn_cast<AbstractStorageDecl>(M)) {
      return STD->hasStorage() && !STD->isStatic();
    }
    return false;
  };
  // The relative order of non-final instance functions matters for non-resilient
  // class.
  auto isNonfinalFunc = [](Decl *M) {
    if (auto *FD = dyn_cast<FuncDecl>(M)) {
      return !isa<AccessorDecl>(FD) && !FD->isFinal();
    }
    return false;
  };
  switch (NTD->getKind()) {
  case DeclKind::Enum: {
    return getSimilarMemberCount(NTD, VD, [](Decl *M) {
      return isa<EnumElementDecl>(M);
    });
  }
  case DeclKind::Struct: {
    return getSimilarMemberCount(NTD, VD, isStored);
  }
  case DeclKind::Class: {
    if (auto count = getSimilarMemberCount(NTD, VD, isStored)) {
      return count;
    } else {
      return getSimilarMemberCount(NTD, VD, isNonfinalFunc);
    }
  }
  default:
    llvm_unreachable("bad nominal type kind.");
  }
}

SDKNodeInitInfo::SDKNodeInitInfo(SDKContext &Ctx, Type Ty, TypeInitInfo Info) :
    Ctx(Ctx), Name(getTypeName(Ctx, Ty, Info.IsImplicitlyUnwrappedOptional)),
    PrintedName(getPrintedName(Ctx, Ty, Info.IsImplicitlyUnwrappedOptional)),
    ParamValueOwnership(Info.ValueOwnership),
    HasDefaultArg(Info.hasDefaultArgument) {
  if (isFunctionTypeNoEscape(Ty))
    TypeAttrs.push_back(TypeAttrKind::TAK_noescape);
  // If this is a nominal type, get its Usr.
  if (auto *ND = Ty->getAnyNominal()) {
    Usr = calculateUsr(Ctx, ND);
  }
}

SDKNodeInitInfo::SDKNodeInitInfo(SDKContext &Ctx, Decl *D):
      Ctx(Ctx), DKind(D->getKind()),
      Location(calculateLocation(Ctx, D)),
      ModuleName(D->getModuleContext()->getName().str()),
      GenericSig(printGenericSignature(Ctx, D)),
      IsImplicit(D->isImplicit()),
      IsDeprecated(D->getAttrs().getDeprecated(D->getASTContext())) {
  // Capture all attributes.
  auto AllAttrs = D->getAttrs();
  std::transform(AllAttrs.begin(), AllAttrs.end(), std::back_inserter(DeclAttrs),
                 [](DeclAttribute *attr) { return attr->getKind(); });
}

SDKNodeInitInfo::SDKNodeInitInfo(SDKContext &Ctx, OperatorDecl *OD):
    SDKNodeInitInfo(Ctx, cast<Decl>(OD)) {
  Name = OD->getName().str();
  PrintedName = OD->getName().str();
}

SDKNodeInitInfo::SDKNodeInitInfo(SDKContext &Ctx, ValueDecl *VD)
    : SDKNodeInitInfo(Ctx, cast<Decl>(VD)) {
  Name = VD->hasName() ? getEscapedName(VD->getBaseName()) : Ctx.buffer("_");
  PrintedName = getPrintedName(Ctx, VD);
  Usr = calculateUsr(Ctx, VD);
  IsThrowing = isFuncThrowing(VD);
  IsStatic = VD->isStatic();
  IsOverriding = VD->getOverriddenDecl();
  IsProtocolReq = isa<ProtocolDecl>(VD->getDeclContext()) && VD->isProtocolRequirement();
  IsOpen = Ctx.getAccessLevel(VD) == AccessLevel::Open;
  IsInternal = Ctx.getAccessLevel(VD) < AccessLevel::Public;
  SelfIndex = getSelfIndex(VD);
  FixedBinaryOrder = Ctx.getFixedBinaryOrder(VD);
  ReferenceOwnership = getReferenceOwnership(VD);

  // Calculate usr for its super class.
  if (auto *CD = dyn_cast_or_null<ClassDecl>(VD)) {
    if (auto *Super = CD->getSuperclassDecl()) {
      SuperclassUsr = calculateUsr(Ctx, Super);
      for (auto T = CD->getSuperclass(); T; T = T->getSuperclass()) {
        SuperclassNames.push_back(getPrintedName(Ctx, T->getCanonicalType()));
      }
    }
  }

#define CASE(BASE, KIND, KEY) case BASE::KIND: KEY = #KIND; break;
  if (auto *FD = dyn_cast<FuncDecl>(VD)) {
    switch(FD->getSelfAccessKind()) {
    CASE(SelfAccessKind, Mutating, FuncSelfKind)
    CASE(SelfAccessKind, __Consuming, FuncSelfKind)
    CASE(SelfAccessKind, NonMutating, FuncSelfKind)
    }
  }
#undef CASE

  // Get enum raw type name if this is an enum.
  if (auto *ED = dyn_cast<EnumDecl>(VD)) {
    if (auto RT = ED->getRawType()) {
      if (auto *D = RT->getNominalOrBoundGenericNominal()) {
        EnumRawTypeName = D->getName().str();
      }
    }
  }
  if (auto *VAD = dyn_cast<VarDecl>(VD)) {
    IsLet = VAD->isLet();
  }
  // Record whether a subscript has getter/setter.
  if (auto *SD = dyn_cast<SubscriptDecl>(VD)) {
    HasSetter = SD->getSetter();
  }

  if (auto *VAR = dyn_cast<AbstractStorageDecl>(VD)) {
    HasStorage = VAR->hasStorage();
    HasDidset = VAR->getDidSetFunc();
    HasWillset = VAR->getWillSetFunc();
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
SDKNode *swift::ide::api::
SwiftDeclCollector::constructTypeNode(Type T, TypeInitInfo Info) {
  if (Ctx.checkingABI()) {
    T = T->getCanonicalType();
  }

  if (auto NAT = dyn_cast<NameAliasType>(T.getPointer())) {
    SDKNode* Root = SDKNodeInitInfo(Ctx, T, Info).createSDKNode(SDKNodeKind::TypeAlias);
    Root->addChild(constructTypeNode(NAT->getSinglyDesugaredType()));
    return Root;
  }

  if (auto Fun = T->getAs<AnyFunctionType>()) {
    SDKNode* Root = SDKNodeInitInfo(Ctx, T, Info).createSDKNode(SDKNodeKind::TypeFunc);

    // Still, return type first
    Root->addChild(constructTypeNode(Fun->getResult()));

    auto Input = AnyFunctionType::composeInput(Fun->getASTContext(),
                                               Fun->getParams(),
                                               /*canonicalVararg=*/false);
    Root->addChild(constructTypeNode(Input));
    return Root;
  }

  SDKNode* Root = SDKNodeInitInfo(Ctx, T, Info).createSDKNode(SDKNodeKind::TypeNominal);

  // Keep paren type as a stand-alone level.
  if (auto *PT = dyn_cast<ParenType>(T.getPointer())) {
    Root->addChild(constructTypeNode(PT->getSinglyDesugaredType()));
    return Root;
  }

  // Handle the case where Type has sub-types.
  if (auto BGT = T->getAs<BoundGenericType>()) {
    for (auto Arg : BGT->getGenericArgs()) {
      Root->addChild(constructTypeNode(Arg));
    }
  } else if (auto Tup = T->getAs<TupleType>()) {
    for (auto Elt : Tup->getElementTypes())
      Root->addChild(constructTypeNode(Elt));
  } else if (auto MTT = T->getAs<AnyMetatypeType>()) {
    Root->addChild(constructTypeNode(MTT->getInstanceType()));
  } else if (auto ATT = T->getAs<ArchetypeType>()) {
    for (auto Pro : ATT->getConformsTo()) {
      Root->addChild(constructTypeNode(Pro->getDeclaredType()));
    }
  }
  return Root;
}

std::vector<SDKNode*> swift::ide::api::
SwiftDeclCollector::createParameterNodes(ParameterList *PL) {
  std::vector<SDKNode*> Result;
  for (auto param: *PL) {
    TypeInitInfo Info;
    Info.IsImplicitlyUnwrappedOptional = param->getAttrs().
      hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
    Info.hasDefaultArgument = param->getDefaultArgumentKind() !=
      DefaultArgumentKind::None;
    switch (param->getValueOwnership()) {
#define CASE(KIND) case ValueOwnership::KIND: Info.ValueOwnership = #KIND; break;
    CASE(Owned)
    CASE(InOut)
    CASE(Shared)
    case ValueOwnership::Default: break;
#undef CASE
    }
    Result.push_back(constructTypeNode(param->getInterfaceType(), Info));
  }
  return Result;
}

// Construct a node for a function decl. The first child of the function decl
// is guaranteed to be the return value type of this function.
// We sometimes skip the first parameter because it can be metatype of dynamic
// this if the function is a member function.
SDKNode *swift::ide::api::
SwiftDeclCollector::constructFunctionNode(FuncDecl* FD,
                                          SDKNodeKind Kind) {
  auto Func = SDKNodeInitInfo(Ctx, FD).createSDKNode(Kind);
  TypeInitInfo Info;
  Info.IsImplicitlyUnwrappedOptional = FD->getAttrs().
    hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
  Func->addChild(constructTypeNode(FD->getResultInterfaceType(), Info));
  for (auto *Node : createParameterNodes(FD->getParameters()))
    Func->addChild(Node);
  return Func;
}

SDKNode* swift::ide::api::
SwiftDeclCollector::constructInitNode(ConstructorDecl *CD) {
  auto Func = SDKNodeInitInfo(Ctx, CD).createSDKNode(SDKNodeKind::DeclConstructor);
  Func->addChild(constructTypeNode(CD->getResultInterfaceType()));
  for (auto *Node : createParameterNodes(CD->getParameters()))
    Func->addChild(Node);
  return Func;
}

bool swift::ide::api::
SDKContext::shouldIgnore(Decl *D, const Decl* Parent) const {
  // Exclude all clang nodes if we're comparing Swift decls specifically.
  if (Opts.SwiftOnly && isFromClang(D)) {
    return true;
  }
  if (checkingABI()) {
    if (auto *VD = dyn_cast<ValueDecl>(D)) {
      // Private vars with fixed binary orders can have ABI-impact, so we should
      // whitelist them if we're checking ABI.
      if (getFixedBinaryOrder(VD).hasValue())
        return false;
      // Typealias should have no impact on ABI.
      if (isa<TypeAliasDecl>(VD))
        return true;
    }
  } else {
    if (D->isPrivateStdlibDecl(false))
      return true;
    if (AvailableAttr::isUnavailable(D))
      return true;
  }
  if (auto VD = dyn_cast<ValueDecl>(D)) {
    if (VD->getBaseName().empty())
      return true;
    switch (getAccessLevel(VD)) {
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

SDKNode *swift::ide::api::
SwiftDeclCollector::constructTypeDeclNode(NominalTypeDecl *NTD) {
  auto TypeNode = SDKNodeInitInfo(Ctx, NTD).createSDKNode(SDKNodeKind::DeclType);
  addConformancesToTypeDecl(cast<SDKNodeDeclType>(TypeNode), NTD);
  addMembersToRoot(TypeNode, NTD);
  for (auto Ext : NTD->getExtensions()) {
    HandledExtensions.insert(Ext);
    addMembersToRoot(TypeNode, Ext);
  }
  return TypeNode;
}

/// Create a node for stand-alone extensions. In the sdk dump, we don't have
/// a specific node for extension. Members in extensions are inlined to the
/// extended types. If the extended types are from a different module, we have to
/// synthesize this type node to include those extension members, since these
/// extension members are legit members of the module.
SDKNode *swift::ide::api::
SwiftDeclCollector::constructExternalExtensionNode(NominalTypeDecl *NTD,
                                            ArrayRef<ExtensionDecl*> AllExts) {
  auto *TypeNode = SDKNodeInitInfo(Ctx, NTD).createSDKNode(SDKNodeKind::DeclType);
  addConformancesToTypeDecl(cast<SDKNodeDeclType>(TypeNode), NTD);
  // The members of the extensions are the only members of this synthesized type.
  for (auto *Ext: AllExts) {
    HandledExtensions.insert(Ext);
    addMembersToRoot(TypeNode, Ext);
  }
  return TypeNode;
}

SDKNode *swift::ide::api::
SwiftDeclCollector::constructVarNode(ValueDecl *VD) {
  auto Var = SDKNodeInitInfo(Ctx, VD).createSDKNode(SDKNodeKind::DeclVar);
  TypeInitInfo Info;
  Info.IsImplicitlyUnwrappedOptional = VD->getAttrs().
    hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
  Var->addChild(constructTypeNode(VD->getInterfaceType(), Info));
  if (auto VAD = dyn_cast<AbstractStorageDecl>(VD)) {
    if (auto Getter = VAD->getGetter())
      Var->addChild(constructFunctionNode(Getter, SDKNodeKind::DeclGetter));
    if (auto Setter = VAD->getSetter()) {
      if (Setter->getFormalAccess() > AccessLevel::Internal)
        Var->addChild(constructFunctionNode(Setter, SDKNodeKind::DeclSetter));
    }
  }
  return Var;
}

SDKNode *swift::ide::api::
SwiftDeclCollector::constructTypeAliasNode(TypeAliasDecl *TAD) {
  auto Alias = SDKNodeInitInfo(Ctx, TAD).createSDKNode(SDKNodeKind::DeclTypeAlias);
  Alias->addChild(constructTypeNode(TAD->getUnderlyingTypeLoc().getType()));
  return Alias;
}

SDKNode *swift::ide::api::
SwiftDeclCollector::constructAssociatedTypeNode(AssociatedTypeDecl *ATD) {
  auto Asso = SDKNodeInitInfo(Ctx, ATD).
    createSDKNode(SDKNodeKind::DeclAssociatedType);
  if (auto DT = ATD->getDefaultDefinitionType()) {
    Asso->addChild(constructTypeNode(DT));
  }
  return Asso;
}

SDKNode *swift::ide::api::
SwiftDeclCollector::constructSubscriptDeclNode(SubscriptDecl *SD) {
  auto Subs = SDKNodeInitInfo(Ctx, SD).createSDKNode(SDKNodeKind::DeclSubscript);
  Subs->addChild(constructTypeNode(SD->getElementInterfaceType()));
  for (auto *Node: createParameterNodes(SD->getIndices()))
    Subs->addChild(Node);
  return Subs;
}

void swift::ide::api::
SwiftDeclCollector::addMembersToRoot(SDKNode *Root, IterableDeclContext *Context) {
  for (auto *Member : Context->getMembers()) {
    if (Ctx.shouldIgnore(Member, Context->getDecl()))
      continue;
    if (auto Func = dyn_cast<FuncDecl>(Member)) {
      Root->addChild(constructFunctionNode(Func, SDKNodeKind::DeclFunction));
    } else if (auto CD = dyn_cast<ConstructorDecl>(Member)) {
      Root->addChild(constructInitNode(CD));
    } else if (auto VD = dyn_cast<VarDecl>(Member)) {
      Root->addChild(constructVarNode(VD));
    } else if (auto TAD = dyn_cast<TypeAliasDecl>(Member)) {
      Root->addChild(constructTypeAliasNode(TAD));
    } else if (auto EED = dyn_cast<EnumElementDecl>(Member)) {
      Root->addChild(constructVarNode(EED));
    } else if (auto NTD = dyn_cast<NominalTypeDecl>(Member)) {
      Root->addChild(constructTypeDeclNode(NTD));
    } else if (auto ATD = dyn_cast<AssociatedTypeDecl>(Member)) {
      Root->addChild(constructAssociatedTypeNode(ATD));
    } else if (auto SD = dyn_cast<SubscriptDecl>(Member)) {
      Root->addChild(constructSubscriptDeclNode(SD));
    } else if (isa<PatternBindingDecl>(Member)) {
      // All containing variables should have been handled.
    } else if (isa<DestructorDecl>(Member)) {
      // deinit has no impact.
    } else {
      llvm_unreachable("unhandled member decl kind.");
    }
  }
}

SDKNode *swift::ide::api::
SwiftDeclCollector::constructTypeWitnessNode(AssociatedTypeDecl *Assoc,
                                             Type Ty) {
  auto *Witness = SDKNodeInitInfo(Ctx, Assoc).createSDKNode(SDKNodeKind::TypeWitness);
  Witness->addChild(constructTypeNode(Ty));
  return Witness;
}

SDKNode *swift::ide::api::
SwiftDeclCollector::constructConformanceNode(ProtocolConformance *Conform) {
  if (Ctx.checkingABI())
    Conform = Conform->getCanonicalConformance();
  auto ConfNode = cast<SDKNodeConformance>(SDKNodeInitInfo(Ctx,
    Conform->getProtocol()).createSDKNode(SDKNodeKind::Conformance));
  Conform->forEachTypeWitness(nullptr,
    [&](AssociatedTypeDecl *assoc, Type ty, TypeDecl *typeDecl) -> bool {
      ConfNode->addChild(constructTypeWitnessNode(assoc, ty));
      return false;
    });
  return ConfNode;
}

void swift::ide::api::
SwiftDeclCollector::addConformancesToTypeDecl(SDKNodeDeclType *Root,
                                              NominalTypeDecl *NTD) {
  for (auto &Conf: NTD->getAllConformances()) {
    if (!Ctx.shouldIgnore(Conf->getProtocol()))
      Root->addConformance(constructConformanceNode(Conf));
  }
}

void SwiftDeclCollector::printTopLevelNames() {
  for (auto &Node : RootNode->getChildren()) {
    llvm::outs() << Node->getKind() << ": " << Node->getName() << '\n';
  }
}

void SwiftDeclCollector::lookupVisibleDecls(ArrayRef<ModuleDecl *> Modules) {
  for (auto M: Modules) {
    llvm::SmallVector<Decl*, 512> Decls;
    M->getDisplayDecls(Decls);
    for (auto D : Decls) {
      if (Ctx.shouldIgnore(D))
        continue;
      if (KnownDecls.count(D))
        continue;
      KnownDecls.insert(D);
      if (auto VD = dyn_cast<ValueDecl>(D))
        foundDecl(VD, DeclVisibilityKind::DynamicLookup);
      else
        processDecl(D);
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
    processValueDecl(VD);

  // Collect extensions to types from other modules and synthesize type nodes
  // for them.
  llvm::MapVector<NominalTypeDecl*, llvm::SmallVector<ExtensionDecl*, 4>> ExtensionMap;
  for (auto *D: KnownDecls) {
    if (auto *Ext = dyn_cast<ExtensionDecl>(D)) {
      if (HandledExtensions.find(Ext) == HandledExtensions.end()) {
        ExtensionMap[Ext->getExtendedNominal()].push_back(Ext);
      }
    }
  }
  for (auto Pair: ExtensionMap) {
    RootNode->addChild(constructExternalExtensionNode(Pair.first, Pair.second));
  }
}

SDKNode *SwiftDeclCollector::constructOperatorDeclNode(OperatorDecl *OD) {
  return SDKNodeInitInfo(Ctx, OD).createSDKNode(SDKNodeKind::DeclOperator);
}

void SwiftDeclCollector::processDecl(Decl *D) {
  assert(!isa<ValueDecl>(D));
  if (auto *OD = dyn_cast<OperatorDecl>(D)) {
    RootNode->addChild(constructOperatorDeclNode(OD));
  }
}

void SwiftDeclCollector::processValueDecl(ValueDecl *VD) {
  if (auto FD = dyn_cast<FuncDecl>(VD)) {
    RootNode->addChild(constructFunctionNode(FD, SDKNodeKind::DeclFunction));
  } else if (auto NTD = dyn_cast<NominalTypeDecl>(VD)) {
    RootNode->addChild(constructTypeDeclNode(NTD));
  } else if (auto VAD = dyn_cast<VarDecl>(VD)) {
    RootNode->addChild(constructVarNode(VAD));
  } else if (auto TAD = dyn_cast<TypeAliasDecl>(VD)) {
    RootNode->addChild(constructTypeAliasNode(TAD));
  } else {
    llvm_unreachable("unhandled value decl");
  }
}

void SwiftDeclCollector::foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) {
  if (VD->getClangMacro()) {
    // Collect macros, we will sort them afterwards.
    ClangMacros.push_back(VD);
    return;
  }

  processValueDecl(VD);
}

void SDKNode::output(json::Output &out, KeyKind Key, bool Value) {
  if (Value)
    out.mapRequired(getKeyContent(Ctx, Key).data(), Value);
}

void SDKNode::output(json::Output &out, KeyKind Key, StringRef Value) {
  if (!Value.empty())
    out.mapRequired(getKeyContent(Ctx, Key).data(), Value);
}

void SDKNode::jsonize(json::Output &out) {
  auto Kind = getKind();
  out.mapRequired(getKeyContent(Ctx, KeyKind::KK_kind).data(), Kind);
  output(out, KeyKind::KK_name, Name);
  output(out, KeyKind::KK_printedName, PrintedName);
  out.mapOptional(getKeyContent(Ctx, KeyKind::KK_children).data(), Children);
}

void SDKNodeDecl::jsonize(json::Output &out) {
  SDKNode::jsonize(out);
  out.mapRequired(getKeyContent(Ctx, KeyKind::KK_declKind).data(), DKind);
  output(out, KeyKind::KK_usr, Usr);
  output(out, KeyKind::KK_location, Location);
  output(out, KeyKind::KK_moduleName, ModuleName);
  output(out, KeyKind::KK_genericSig, GenericSig);
  output(out, KeyKind::KK_static, IsStatic);
  output(out, KeyKind::KK_deprecated,IsDeprecated);
  output(out, KeyKind::KK_protocolReq, IsProtocolReq);
  output(out, KeyKind::KK_overriding, IsOverriding);
  output(out, KeyKind::KK_implicit, IsImplicit);
  output(out, KeyKind::KK_isOpen, IsOpen);
  output(out, KeyKind::KK_isInternal, IsInternal);
  out.mapOptional(getKeyContent(Ctx, KeyKind::KK_declAttributes).data(), DeclAttributes);
  out.mapOptional(getKeyContent(Ctx, KeyKind::KK_fixedbinaryorder).data(), FixedBinaryOrder);
  // Strong reference is implied, no need for serialization.
  if (getReferenceOwnership() != ReferenceOwnership::Strong) {
    uint8_t Raw = uint8_t(getReferenceOwnership());
    out.mapRequired(getKeyContent(Ctx, KeyKind::KK_ownership).data(), Raw);
  }
}

void SDKNodeDeclAbstractFunc::jsonize(json::Output &out) {
  SDKNodeDecl::jsonize(out);
  output(out, KeyKind::KK_throwing, IsThrowing);
  out.mapOptional(getKeyContent(Ctx, KeyKind::KK_selfIndex).data(), SelfIndex);
}

void SDKNodeDeclFunction::jsonize(json::Output &out) {
  SDKNodeDeclAbstractFunc::jsonize(out);
  output(out, KeyKind::KK_funcSelfKind, FuncSelfKind);
}

void SDKNodeDeclType::jsonize(json::Output &out) {
  SDKNodeDecl::jsonize(out);
  output(out, KeyKind::KK_superclassUsr, SuperclassUsr);
  output(out, KeyKind::KK_enumRawTypeName, EnumRawTypeName);
  out.mapOptional(getKeyContent(Ctx, KeyKind::KK_superclassNames).data(), SuperclassNames);
  out.mapOptional(getKeyContent(Ctx, KeyKind::KK_conformances).data(), Conformances);
}

void SDKNodeType::jsonize(json::Output &out) {
  SDKNode::jsonize(out);
  out.mapOptional(getKeyContent(Ctx, KeyKind::KK_typeAttributes).data(), TypeAttributes);
  output(out, KeyKind::KK_hasDefaultArg, HasDefaultArg);
  output(out, KeyKind::KK_paramValueOwnership, ParamValueOwnership);
}

void SDKNodeTypeNominal::jsonize(json::Output &out) {
  SDKNodeType::jsonize(out);
  output(out, KeyKind::KK_usr, USR);
}

void SDKNodeDeclSubscript::jsonize(json::Output &out) {
  SDKNodeDeclAbstractFunc::jsonize(out);
  output(out, KeyKind::KK_hasSetter, HasSetter);
  output(out, KeyKind::KK_hasStorage, HasStorage);
  output(out, KeyKind::KK_hasDidset, HasDidSet);
  output(out, KeyKind::KK_hasWillset, HasWillSet);
}

void SDKNodeDeclVar::jsonize(json::Output &out) {
  SDKNodeDecl::jsonize(out);
  output(out, KeyKind::KK_isLet, IsLet);
  output(out, KeyKind::KK_hasStorage, HasStorage);
  output(out, KeyKind::KK_hasDidset, HasDidSet);
  output(out, KeyKind::KK_hasWillset, HasWillSet);
}

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
  }
};

template<>
struct ScalarEnumerationTraits<DeclAttrKind> {
  static void enumeration(Output &out, DeclAttrKind &value) {
#define DECL_ATTR(_, Name, ...) out.enumCase(value, #Name, DeclAttrKind::DAK_##Name);
#include "swift/AST/Attr.def"
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
    value->jsonize(out);
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
struct ArrayTraits<ArrayRef<DeclAttrKind>> {
  static size_t size(Output &out, ArrayRef<DeclAttrKind> &seq) {
    return seq.size();
  }
  static DeclAttrKind& element(Output &, ArrayRef<DeclAttrKind> &seq,
                               size_t index) {
    return const_cast<DeclAttrKind&>(seq[index]);
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

namespace  {// Anonymous namespace.
// Serialize a forest of SDKNode trees to the given stream.
static void emitSDKNodeRoot(llvm::raw_ostream &os, SDKNode *&Root) {
  json::Output yout(os);
  yout << Root;
}

// Deserialize an SDKNode tree.
std::pair<std::unique_ptr<llvm::MemoryBuffer>, SDKNode*>
static parseJsonEmit(SDKContext &Ctx, StringRef FileName) {
  namespace yaml = llvm::yaml;

  // Load the input file.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
    vfs::getFileOrSTDIN(*Ctx.getSourceMgr().getFileSystem(), FileName);
  if (!FileBufOrErr) {
    llvm_unreachable("Failed to read JSON file");
  }
  StringRef Buffer = FileBufOrErr->get()->getBuffer();
  yaml::Stream Stream(llvm::MemoryBufferRef(Buffer, FileName),
                      Ctx.getSourceMgr().getLLVMSourceMgr());
  SDKNode *Result = nullptr;
  for (auto DI = Stream.begin(); DI != Stream.end(); ++ DI) {
    assert(DI != Stream.end() && "Failed to read a document");
    yaml::Node *N = DI->getRoot();
    assert(N && "Failed to find a root");
    Result = SDKNode::constructSDKNode(Ctx, cast<yaml::MappingNode>(N));
    if (Ctx.getDiags().hadAnyError())
      exit(1);
  }
  return {std::move(FileBufOrErr.get()), Result};
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
} // End of anonymous namespace

// Construct all roots vector from a given file where a forest was
// previously dumped.
void SwiftDeclCollector::deSerialize(StringRef Filename) {
  auto Pair = parseJsonEmit(Ctx, Filename);
  OwnedBuffers.push_back(std::move(Pair.first));
  RootNode = std::move(Pair.second);
}

// Serialize the content of all roots to a given file using JSON format.
void SwiftDeclCollector::serialize(StringRef Filename) {
  std::error_code EC;
  llvm::raw_fd_ostream fs(Filename, EC, llvm::sys::fs::F_None);
  emitSDKNodeRoot(fs, RootNode);
}

int swift::ide::api::dumpSwiftModules(const CompilerInvocation &InitInvok,
                                      const llvm::StringSet<> &ModuleNames,
                                      StringRef OutputDir,
                                      const std::vector<std::string> PrintApis,
                                      CheckerOptions Opts) {
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
    if (Opts.Verbose)
      llvm::errs() << "Loading module: " << Name << "...\n";
    auto *M = Context.getModuleByName(Name);
    if (!M) {
      if (Opts.Verbose)
        llvm::errs() << "Failed to load module: " << Name << '\n';
      if (Opts.AbortOnModuleLoadFailure)
        return 1;
    }
    Modules.push_back(M);
  }

  PrintingDiagnosticConsumer PDC;
  SDKContext Ctx(Opts);
  Ctx.getDiags().addConsumer(PDC);

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
    if (Opts.Verbose)
      llvm::errs() << "Dumped to "<< Path << "\n";
  }
  return 0;
}

int swift::ide::api::dumpSDKContent(const CompilerInvocation &InitInvok,
                                    const llvm::StringSet<> &ModuleNames,
                                    StringRef OutputFile, CheckerOptions Opts) {
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
    if (Opts.Verbose)
      llvm::errs() << "Loading module: " << Name << "...\n";
    auto *M = Ctx.getModuleByName(Name);
    if (!M) {
      llvm::errs() << "Failed to load module: " << Name << '\n';
      if (Opts.AbortOnModuleLoadFailure)
        return 1;
    } else {
      Modules.push_back(M);
    }
  }
  if (Opts.Verbose)
    llvm::errs() << "Scanning symbols...\n";
  SDKContext SDKCtx(Opts);
  SwiftDeclCollector Collector(SDKCtx);
  Collector.lookupVisibleDecls(Modules);
  if (Opts.Verbose)
    llvm::errs() << "Dumping SDK...\n";
  Collector.serialize(OutputFile);
  if (Opts.Verbose)
    llvm::errs() << "Dumped to "<< OutputFile << "\n";
  return 0;
}

int swift::ide::api::deserializeSDKDump(StringRef dumpPath, StringRef OutputPath,
    CheckerOptions Opts) {
  std::error_code EC;
  llvm::raw_fd_ostream FS(OutputPath, EC, llvm::sys::fs::F_None);
  if (!fs::exists(dumpPath)) {
    llvm::errs() << dumpPath << " does not exist\n";
    return 1;
  }
  PrintingDiagnosticConsumer PDC;
  SDKContext Ctx(Opts);
  Ctx.getDiags().addConsumer(PDC);

  SwiftDeclCollector Collector(Ctx);
  Collector.deSerialize(dumpPath);
  Collector.serialize(OutputPath);
  return 0;
}

int swift::ide::api::findDeclUsr(StringRef dumpPath, CheckerOptions Opts) {
  std::error_code EC;
  if (!fs::exists(dumpPath)) {
    llvm::errs() << dumpPath << " does not exist\n";
    return 1;
  }
  PrintingDiagnosticConsumer PDC;
  SDKContext Ctx(Opts);
  Ctx.getDiags().addConsumer(PDC);

  SwiftDeclCollector Collector(Ctx);
  Collector.deSerialize(dumpPath);
  struct FinderByLocation: SDKNodeVisitor {
    StringRef Location;
    FinderByLocation(StringRef Location): Location(Location) {}
    void visit(SDKNode* Node) override {
      if (auto *D = dyn_cast<SDKNodeDecl>(Node)) {
        if (D->getLocation().find(Location) != StringRef::npos &&
            !D->getUsr().empty()) {
          llvm::outs() << D->getFullyQualifiedName() << ": " << D->getUsr() << "\n";
        }
      }
    }
  };
  if (!Opts.LocationFilter.empty()) {
    FinderByLocation Finder(Opts.LocationFilter);
    Collector.visitAllRoots(Finder);
  }
  return 0;
}
