#include <ModuleAnalyzerNodes.h>

using namespace swift;
using namespace ide;
using namespace api;

namespace fs = llvm::sys::fs;
namespace path = llvm::sys::path;

namespace {

enum class KeyKind {
#define KEY(NAME) KK_##NAME,
#include "swift/IDE/DigesterEnums.def"
};

/// The additional information we need to create a type node.
struct TypeInitInfo {
  bool IsImplicitlyUnwrappedOptional = false;
  /// When this type node represents a function parameter, this boolean value
  /// indicates whether the parameter has default argument.
  bool hasDefaultArgument = false;
};

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
  StringRef Name;
  StringRef PrintedName;
  DeclKind DKind;
  StringRef USR;
  StringRef Location;
  StringRef ModuleName;
  bool IsThrowing = false;
  bool IsMutating = false;
  bool IsStatic = false;
  bool IsDeprecated = false;
  Optional<uint8_t> SelfIndex;
  ReferenceOwnership ReferenceOwnership = ReferenceOwnership::Strong;
  std::vector<DeclAttrKind> DeclAttrs;
  std::vector<TypeAttrKind> TypeAttrs;
  std::vector<StringRef> ConformingProtocols;
  StringRef SuperclassUsr;
  StringRef EnumRawTypeName;
  TypeInitInfo TypeInfo;
  StringRef GenericSig;

  SDKNodeInitInfo(SDKContext &Ctx) : Ctx(Ctx) {}
  SDKNodeInitInfo(SDKContext &Ctx, ValueDecl *VD);
  SDKNodeInitInfo(SDKContext &Ctx, Type Ty, TypeInitInfo Info = TypeInitInfo());
  SDKNode* createSDKNode(SDKNodeKind Kind);
};

SDKContext::SDKContext(CheckerOptions Opts): Diags(SourceMgr), Opts(Opts) {
#define ADD(NAME) ABIAttrs.push_back({DeclAttrKind::DAK_##NAME, \
      getAttrName(DeclAttrKind::DAK_##NAME)});
  ADD(ObjC)
  ADD(FixedLayout)
  ADD(Frozen)
  ADD(Dynamic)
#undef ADD
}


SDKNode::SDKNode(SDKNodeInitInfo Info, SDKNodeKind Kind): Ctx(Info.Ctx),
  Name(Info.Name), PrintedName(Info.PrintedName), TheKind(unsigned(Kind)) {}

SDKNodeRoot::SDKNodeRoot(SDKNodeInitInfo Info): SDKNode(Info, SDKNodeKind::Root) {}

SDKNodeDecl::SDKNodeDecl(SDKNodeInitInfo Info, SDKNodeKind Kind)
      : SDKNode(Info, Kind), DKind(Info.DKind), Usr(Info.USR),
        Location(Info.Location), ModuleName(Info.ModuleName),
        DeclAttributes(Info.DeclAttrs), IsStatic(Info.IsStatic),
        IsDeprecated(Info.IsDeprecated),
        ReferenceOwnership(uint8_t(Info.ReferenceOwnership)),
        GenericSig(Info.GenericSig) {}

SDKNodeType::SDKNodeType(SDKNodeInitInfo Info, SDKNodeKind Kind):
  SDKNode(Info, Kind), TypeAttributes(Info.TypeAttrs),
  HasDefaultArg(Info.TypeInfo.hasDefaultArgument) {}

SDKNodeTypeNominal::SDKNodeTypeNominal(SDKNodeInitInfo Info):
  SDKNodeType(Info, SDKNodeKind::TypeNominal), USR(Info.USR) {}

SDKNodeTypeFunc::SDKNodeTypeFunc(SDKNodeInitInfo Info):
  SDKNodeType(Info, SDKNodeKind::TypeFunc) {}

SDKNodeTypeAlias::SDKNodeTypeAlias(SDKNodeInitInfo Info):
  SDKNodeType(Info, SDKNodeKind::TypeAlias) {}

SDKNodeDeclType::SDKNodeDeclType(SDKNodeInitInfo Info): 
  SDKNodeDecl(Info, SDKNodeKind::DeclType), SuperclassUsr(Info.SuperclassUsr),
  ConformingProtocols(Info.ConformingProtocols),
  EnumRawTypeName(Info.EnumRawTypeName) {}

SDKNodeDeclTypeAlias::SDKNodeDeclTypeAlias(SDKNodeInitInfo Info):
  SDKNodeDecl(Info, SDKNodeKind::DeclTypeAlias) {}

SDKNodeDeclVar::SDKNodeDeclVar(SDKNodeInitInfo Info): 
  SDKNodeDecl(Info, SDKNodeKind::DeclVar) {}

SDKNodeDeclAbstractFunc::SDKNodeDeclAbstractFunc(SDKNodeInitInfo Info,
  SDKNodeKind Kind): SDKNodeDecl(Info, Kind), IsThrowing(Info.IsThrowing),
                     IsMutating(Info.IsMutating), SelfIndex(Info.SelfIndex) {}

SDKNodeDeclFunction::SDKNodeDeclFunction(SDKNodeInitInfo Info):
  SDKNodeDeclAbstractFunc(Info, SDKNodeKind::DeclFunction) {}

SDKNodeDeclConstructor::SDKNodeDeclConstructor(SDKNodeInitInfo Info):
  SDKNodeDeclAbstractFunc(Info, SDKNodeKind::DeclConstructor) {}

SDKNodeDeclGetter::SDKNodeDeclGetter(SDKNodeInitInfo Info): 
  SDKNodeDeclAbstractFunc(Info, SDKNodeKind::DeclGetter) {}

SDKNodeDeclSetter::SDKNodeDeclSetter(SDKNodeInitInfo Info):
  SDKNodeDeclAbstractFunc(Info, SDKNodeKind::DeclSetter) {}

StringRef SDKNodeDecl::getHeaderName() const {
  if (Location.empty())
    return StringRef();
  return llvm::sys::path::filename(Location.split(":").first);
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

bool SDKNodeDecl::hasDeclAttribute(DeclAttrKind DAKind) const {
  return std::find(DeclAttributes.begin(), DeclAttributes.end(), DAKind) !=
    DeclAttributes.end();
}

ArrayRef<DeclAttrKind> SDKNodeDecl::getDeclAttributes() const {
  return llvm::makeArrayRef(DeclAttributes.data(), DeclAttributes.size());
}

bool SDKNodeDecl::hasAttributeChange(const SDKNodeDecl &Another) const {
  if (getDeclAttributes().size() != Another.getDeclAttributes().size())
    return true;
  for (auto K: getDeclAttributes()) {
    if (!Another.hasDeclAttribute(K))
      return true;
  }
  return false;
}

SDKNodeDecl *SDKNodeType::getClosestParentDecl() const {
  auto *Result = getParent();
  for (; !isa<SDKNodeDecl>(Result); Result = Result->getParent());
  return Result->getAs<SDKNodeDecl>();
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
      return std::find(ConformingProtocols.begin(),                         \
                       ConformingProtocols.end(),                           \
                       #NAME) != ConformingProtocols.end();
#include "swift/IDE/DigesterEnums.def"
  }
}

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

  SDKNodeKind Kind;
  SDKNodeInitInfo Info(Ctx);
  NodeVector Children;

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
      case KeyKind::KK_genericSig:
        Info.GenericSig = GetScalarString(Pair.getValue());
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
      case KeyKind::KK_deprecated:
        Info.IsDeprecated = true;
        break;
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
      if (Left->hasAttributeChange(*Right))
        return false;
      if (Left->getGenericSignature() != Right->getGenericSignature())
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
  if (SDKCtx.getOpts().AvoidLocation)
    return StringRef();
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

static StringRef printGenericSignature(SDKContext &Ctx, ValueDecl *VD) {
  llvm::SmallString<32> Result;
  llvm::raw_svector_ostream OS(Result);
  if (auto *PD = dyn_cast<ProtocolDecl>(VD)) {
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

  if (auto *GC = VD->getAsGenericContext()) {
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
      IsStatic(VD->isStatic()),
      IsDeprecated(VD->getAttrs().getDeprecated(VD->getASTContext())),
      SelfIndex(getSelfIndex(VD)), ReferenceOwnership(getReferenceOwnership(VD)),
      GenericSig(printGenericSignature(Ctx, VD)) {

  // Calculate usr for its super class.
  if (auto *CD = dyn_cast_or_null<ClassDecl>(VD)) {
    if (auto *Super = CD->getSuperclassDecl())
      SuperclassUsr = calculateUsr(Ctx, Super);
  }

  // Capture all attributes.
  auto AllAttrs = VD->getAttrs();
  std::transform(AllAttrs.begin(), AllAttrs.end(), std::back_inserter(DeclAttrs),
                 [](DeclAttribute *attr) { return attr->getKind(); });

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
  if (Ctx.checkingABI()) {
    T = T->getCanonicalType();
  }
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
createParameterNodes(SDKContext &Ctx, ParameterList *PL) {
  std::vector<SDKNode*> Result;
  for (auto param: *PL) {
    TypeInitInfo TypeInfo;
    TypeInfo.IsImplicitlyUnwrappedOptional = param->getAttrs().
      hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
    TypeInfo.hasDefaultArgument = param->getDefaultArgumentKind() !=
      DefaultArgumentKind::None;
    Result.push_back(constructTypeNode(Ctx, param->getInterfaceType(),
                                       TypeInfo));
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
  for (auto *Node : createParameterNodes(Ctx, FD->getParameters()))
    Func->addChild(Node);
  return Func;
}

static SDKNode* constructInitNode(SDKContext &Ctx, ConstructorDecl *CD) {
  auto Func = SDKNodeInitInfo(Ctx, CD).createSDKNode(SDKNodeKind::DeclConstructor);
  Func->addChild(constructTypeNode(Ctx, CD->getResultInterfaceType()));
  for (auto *Node : createParameterNodes(Ctx, CD->getParameters()))
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
                             IterableDeclContext *Context,
                             std::set<ExtensionDecl*> &HandledExts);

static SDKNode *constructTypeDeclNode(SDKContext &Ctx, NominalTypeDecl *NTD,
                                      std::set<ExtensionDecl*> &HandledExts) {
  auto TypeNode = SDKNodeInitInfo(Ctx, NTD).createSDKNode(SDKNodeKind::DeclType);
  addMembersToRoot(Ctx, TypeNode, NTD, HandledExts);
  for (auto Ext : NTD->getExtensions()) {
    HandledExts.insert(Ext);
    addMembersToRoot(Ctx, TypeNode, Ext, HandledExts);
  }
  return TypeNode;
}

/// Create a node for stand-alone extensions. In the sdk dump, we don't have
/// a specific node for extension. Members in extensions are inlined to the
/// extended types. If the extended types are from a different module, we have to
/// synthesize this type node to include those extension members, since these
/// extension members are legit members of the module.
static SDKNode *constructExternalExtensionNode(SDKContext &Ctx, SDKNode *Root,
                                               ExtensionDecl *Ext,
                                        std::set<ExtensionDecl*> &HandledExts) {
  auto *TypeNode = SDKNodeInitInfo(Ctx, Ext->getSelfNominalTypeDecl())
      .createSDKNode(SDKNodeKind::DeclType);

  // The members of the extension are the only members of this synthesized type.
  addMembersToRoot(Ctx, TypeNode, Ext, HandledExts);
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
                             IterableDeclContext *Context,
                             std::set<ExtensionDecl*> &HandledExts) {
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
      Root->addChild(constructTypeDeclNode(Ctx, NTD, HandledExts));
    }
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
      if (shouldIgnore(D, nullptr))
        continue;
      if (KnownDecls.count(D))
        continue;
      KnownDecls.insert(D);
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

  // For all known decls, collect those unhandled extensions and handle them
  // separately.
  for (auto *D: KnownDecls) {
    if (auto *Ext = dyn_cast<ExtensionDecl>(D)) {
      if (HandledExtensions.find(Ext) == HandledExtensions.end()) {
        RootNode->addChild(constructExternalExtensionNode(Ctx, RootNode, Ext,
                                                          HandledExtensions));
      }
    }
  }
}

void SwiftDeclCollector::processDecl(ValueDecl *VD) {
  if (auto FD = dyn_cast<FuncDecl>(VD)) {
    RootNode->addChild(constructFunctionNode(Ctx, FD, SDKNodeKind::DeclFunction));
  } else if (auto NTD = dyn_cast<NominalTypeDecl>(VD)) {
    RootNode->addChild(constructTypeDeclNode(Ctx, NTD, HandledExtensions));
  }
  if (auto VAD = dyn_cast<VarDecl>(VD)) {
    RootNode->addChild(constructVarNode(Ctx, VAD));
  }
  if (auto TAD = dyn_cast<TypeAliasDecl>(VD)) {
    RootNode->addChild(constructTypeAliasNode(Ctx, TAD));
  }
}

void SwiftDeclCollector::foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) {
  if (VD->getClangMacro()) {
    // Collect macros, we will sort them afterwards.
    ClangMacros.push_back(VD);
    return;
  }

  processDecl(VD);
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
      auto GSig = D->getGenericSignature();
      if (!GSig.empty())
        out.mapRequired(getKeyContent(Ctx, KeyKind::KK_genericSig), GSig);

      if (auto isStatic = D->isStatic())
        out.mapRequired(getKeyContent(Ctx, KeyKind::KK_static).data(), isStatic);
      if (bool isDeprecated = D->isDeprecated())
        out.mapRequired(getKeyContent(Ctx, KeyKind::KK_deprecated).data(),
                        isDeprecated);
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
