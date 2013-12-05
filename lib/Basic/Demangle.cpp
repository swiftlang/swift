//===--- Demangle.cpp - Swift Name Demangling -----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===---------------------------------------------------------------------===//
//
//  This file implements declaration name demangling in Swift.
//
//===---------------------------------------------------------------------===//

#include "swift/Basic/Demangle.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/LLVM.h"
#include "llvm/Support/raw_ostream.h"
#include <functional>
#include <tuple>
#include <vector>

using namespace swift;
using namespace Demangle;

Node::size_type Node::size () {
  return Children.size();
}
Node::iterator Node::begin () {
  return Children.begin();
}
Node::iterator Node::end () {
  return Children.end();
}
Node::const_iterator Node::begin () const {
  return Children.begin();
}
Node::const_iterator Node::end () const {
  return Children.end();
}

NodePointer Node::front () {
  return Children.front();
}
NodePointer Node::back () {
  return Children.back();
}

NodePointer Node::child_at (Node::size_type idx) {
  return Children[idx];
}

Node::Node (Node::Kind k, std::string t) : NodeKind(k), NodeText(t), Successor(nullptr), Children(), Parent(nullptr), Predecessor(nullptr) {}

Node::Node (const Node& other) : NodeKind(other.NodeKind), NodeText(other.NodeText), Successor(), Children(), Parent(), Predecessor() {
  for (NodePointer child : other)
    push_back_child(NodePointer(new Node(*child)));
}

Node* Node::getParent () {
  return Parent;
}

NodePointer Node::getNextNode () {
  return Successor;
}
Node* Node::getPreviousNode () {
  return Predecessor;
}

NodePointer Node::push_back_child (NodePointer child) {
  NodePointer ret(child);
  while (child) {
    if (size())
      back()->setSuccessorImpl(child);
    push_back_childImpl(child);
    child->setParent(this);
    child = child->getNextNode();
  }
  return ret;
}

void Node::setParent (Node *parent) {
  Node* ptr = this;
  while (ptr) {
    ptr->setParentImpl(parent);
    ptr = ptr->getNextNode().getPtr();
  }
}
void Node::setNextNode (NodePointer successor) {
  if (getParent())
    insertSiblingImpl(successor);
  else
    setSuccessorImpl(successor);
}

Node::Kind Node::getKind() {
  return NodeKind;
}
void Node::setKind (Node::Kind k) {
  NodeKind = k;
}

std::string Node::getText() {
  return NodeText;
}
void Node::setText (const std::string &t) {
  NodeText.assign(t);
}

void Node::setParentImpl (Node *parent) {
  Parent = parent;
}
void Node::setSuccessorImpl (NodePointer successor) {
  Successor = successor;
  successor->Predecessor = this;
}
void Node::push_back_childImpl (NodePointer child) {
  Children.push_back(child);
}

void Node::insertSiblingImpl (NodePointer child) {
  iterator pBegin = getParent()->begin();
  iterator pEnd = getParent()->end();
  iterator prev_pos = std::find_if(pBegin,pEnd,FindPtr(this));
  if (prev_pos == pEnd) {
    getParent()->push_back_child(child);
  }
  else {
    prev_pos++;
    if (prev_pos == pEnd) {
      getParent()->push_back_child(child);
    }
    else {
      child->setParentImpl(getParent());
      NodePointer old_next = getNextNode();
      getParent()->Children.insert(prev_pos,child);
      if (old_next) {
        NodePointer end_of_chain = child;
        while (end_of_chain->getNextNode())
          end_of_chain = end_of_chain->getNextNode();
        end_of_chain->setSuccessorImpl(old_next);
      }
      setSuccessorImpl(child);
    }
  }
}

class DemanglerPrinter {
public:
  DemanglerPrinter() : Buffer(), BackendPrinter(Buffer) {}

  DemanglerPrinter &operator<<(std::string s) {
    BackendPrinter << s;
    return *this;
  }

  DemanglerPrinter &operator<<(const char *cstr) {
    BackendPrinter << cstr;
    return *this;
  }

  DemanglerPrinter &operator<<(char c) {
    BackendPrinter << c;
    return *this;
  }

  DemanglerPrinter &operator<<(size_t sz) {
    BackendPrinter << sz;
    return *this;
  }

  std::string &str() { return BackendPrinter.str(); }

private:
  std::string Buffer;
  llvm::raw_string_ostream BackendPrinter;
};

static bool isStartOfIdentifier(char c) {
  if (c >= '0' && c <= '9')
    return true;
  return c == 'o';
}

static bool isStartOfNominalType(char c) {
  switch (c) {
  case 'C':
  case 'V':
  case 'O':
    return true;
  default:
    return false;
  }
}

static Node::Kind nominalTypeMarkerToNodeKind(char c) {
  if (c == 'C')
    return Node::Kind::Class;
  if (c == 'V')
    return Node::Kind::Structure;
  if (c == 'O')
    return Node::Kind::Enum;
  return Node::Kind::Identifier;
}

static std::string archetypeName(size_t i) {
  DemanglerPrinter name;
  do {
    name << (char)('A' + (i % 26));
    i /= 26;
  } while (i);
  return name.str();
}

NodePointer Node::makeNodePointer(Kind k, std::string t) {
  return NodePointer(new Node(k, t));
}

class Demangler {
public:
  
  Demangler(llvm::StringRef mangled)
      : Substitutions(), ArchetypeCounts(), ArchetypeCount(), Mangled(mangled),
        RootNode(), CurrentNode() {}
  ~Demangler() {}

  NodePointer appendNode(NodePointer n) {
    if (!RootNode) {
      RootNode = n;
      CurrentNode = RootNode;
    } else {
      CurrentNode->setNextNode(n);
      CurrentNode = n;
    }
    return CurrentNode;
  }

  NodePointer appendNode(Node::Kind k, std::string t = "") {
    if (!RootNode) {
      RootNode = NodePointer(new Node(k, t));
      CurrentNode = RootNode;
    } else {
      CurrentNode->setNextNode(NodePointer(new Node(k, t)));
      CurrentNode = CurrentNode->getNextNode();
    }
    return CurrentNode;
  }

  void insertChildNode(Node::Kind k, std::string t = "") {
    assert(CurrentNode.getPtr() && "Need a current node");
    CurrentNode->push_back_child(NodePointer(new Node(k, t)));
  }

  bool demangle() {
    if (!Mangled.hasAtLeast(2))
      return failure();
    if (Mangled.slice(2) != "_T")
      return failure();
    if (Mangled.hasAtLeast(4)) {
      if (Mangled.slice(4) == "_TTo") {
        Mangled.advanceOffset(4);
        appendNode(Node::Kind::ObjCAttribute);
        if (!demangleGlobal())
          return false;
        return true;
      }
    }
    Mangled.advanceOffset(2);
    if (!demangleGlobal())
      return false;
    return true;
  }

  NodePointer getDemangled() { return RootNode; }
  
private:

  enum class IsProtocol {
    yes = true, no = false
  };

  enum class IsVariadic {
    yes = true, no = false
  };

  typedef std::pair<NodePointer, IsProtocol> Substitution;

  enum class Directness {
    Direct, Indirect, Unkown
  };

  const char *toString(Directness d) {
    switch (d) {
    case Directness::Direct:
      return "direct";
    case Directness::Indirect:
      return "indirect";
    default:
      return "unknown";
    }
  }

  bool failure() {
    RootNode = NodePointer(new Node(Node::Kind::Failure, ""));
    CurrentNode = RootNode;
    return false;
  }

  Directness demangleDirectness() {
    if (Mangled.nextIf('d'))
      return Directness::Direct;
    if (Mangled.nextIf('i'))
      return Directness::Indirect;
    return Directness::Unkown;
  }

  bool demangleNatural(size_t &num) {
    if (!Mangled)
      return false;
    char c = Mangled.next();
    if (c < '0' || c > '9')
      return false;
    num = (c - '0');
    while (true) {
      if (!Mangled) {
        return true;
      }
      c = Mangled.peek();
      if (c < '0' || c > '9') {
        return true;
      } else {
        num = (10 * num) + (c - '0');
      }
      Mangled.next();
    }
  }

  bool demangleBuiltinSize(size_t &num) {
    if (!demangleNatural(num))
      return false;
    if (Mangled.nextIf('_'))
      return true;
    return false;
  }

  enum class ValueWitnessKind {
    AllocateBuffer,
    AssignWithCopy,
    AssignWithTake,
    DeallocateBuffer,
    Destroy,
    DestroyBuffer,
    InitializeBufferWithCopyOfBuffer,
    InitializeBufferWithCopy,
    InitializeWithCopy,
    InitializeBufferWithTake,
    InitializeWithTake,
    ProjectBuffer,
    Typeof,
    StoreExtraInhabitant,
    GetExtraInhabitantIndex,
    GetEnumTag,
    InplaceProjectEnumData,
    Unknown
  };

  const char *toString(ValueWitnessKind k) {
    switch (k) {
    case ValueWitnessKind::AllocateBuffer:
      return "allocateBuffer";
    case ValueWitnessKind::AssignWithCopy:
      return "assignWithCopy";
    case ValueWitnessKind::AssignWithTake:
      return "assignWithTake";
    case ValueWitnessKind::DeallocateBuffer:
      return "deallocateBuffer";
    case ValueWitnessKind::Destroy:
      return "destroy";
    case ValueWitnessKind::DestroyBuffer:
      return "destroyBuffer";
    case ValueWitnessKind::InitializeBufferWithCopyOfBuffer:
      return "initializeBufferWithCopyOfBuffer";
    case ValueWitnessKind::InitializeBufferWithCopy:
      return "initializeBufferWithCopy";
    case ValueWitnessKind::InitializeWithCopy:
      return "initializeWithCopy";
    case ValueWitnessKind::InitializeBufferWithTake:
      return "initializeBufferWithTake";
    case ValueWitnessKind::InitializeWithTake:
      return "initializeWithTake";
    case ValueWitnessKind::ProjectBuffer:
      return "projectBuffer";
    case ValueWitnessKind::Typeof:
      return "typeof";
    case ValueWitnessKind::StoreExtraInhabitant:
      return "storeExtraInhabitant";
    case ValueWitnessKind::GetExtraInhabitantIndex:
      return "getExtraInhabitantIndex";
    case ValueWitnessKind::GetEnumTag:
      return "getEnumTag";
    case ValueWitnessKind::InplaceProjectEnumData:
      return "inplaceProjectEnumData";
    default:
      return "unknown";
    }
  }

  ValueWitnessKind demangleValueWitnessKind() {
    if (!Mangled)
      return ValueWitnessKind::Unknown;
    char c1 = Mangled.next();
    if (!Mangled)
      return ValueWitnessKind::Unknown;
    char c2 = Mangled.next();
    if (c1 == 'a' && c2 == 'l')
      return ValueWitnessKind::AllocateBuffer;
    if (c1 == 'c' && c2 == 'a')
      return ValueWitnessKind::AssignWithCopy;
    if (c1 == 't' && c2 == 'a')
      return ValueWitnessKind::AssignWithTake;
    if (c1 == 'd' && c2 == 'e')
      return ValueWitnessKind::DeallocateBuffer;
    if (c1 == 'x' && c2 == 'x')
      return ValueWitnessKind::Destroy;
    if (c1 == 'X' && c2 == 'X')
      return ValueWitnessKind::DestroyBuffer;
    if (c1 == 'C' && c2 == 'P')
      return ValueWitnessKind::InitializeBufferWithCopyOfBuffer;
    if (c1 == 'C' && c2 == 'p')
      return ValueWitnessKind::InitializeBufferWithCopy;
    if (c1 == 'c' && c2 == 'p')
      return ValueWitnessKind::InitializeWithCopy;
    if (c1 == 'T' && c2 == 'k')
      return ValueWitnessKind::InitializeBufferWithTake;
    if (c1 == 't' && c2 == 'k')
      return ValueWitnessKind::InitializeWithTake;
    if (c1 == 'p' && c2 == 'r')
      return ValueWitnessKind::ProjectBuffer;
    if (c1 == 't' && c2 == 'y')
      return ValueWitnessKind::Typeof;
    if (c1 == 'x' && c2 == 's')
      return ValueWitnessKind::StoreExtraInhabitant;
    if (c1 == 'x' && c2 == 'g')
      return ValueWitnessKind::GetExtraInhabitantIndex;
    if (c1 == 'u' && c2 == 'g')
      return ValueWitnessKind::GetEnumTag;
    if (c1 == 'u' && c2 == 'p')
      return ValueWitnessKind::InplaceProjectEnumData;
    return ValueWitnessKind::Unknown;
  }

  bool demangleGlobal() {
    if (!Mangled)
      return failure();
    if (Mangled.nextIf('M')) {
      if (Mangled.nextIf('P')) {
        Directness d = demangleDirectness();
        if (d == Directness::Unkown)
          return failure();
        NodePointer type = demangleType();
        if (!type)
          return failure();
        appendNode(Node::Kind::Directness, toString(d));
        appendNode(Node::Kind::GenericTypeMetadataPattern)->push_back_child(type);
        return true;
      }
      if (Mangled.nextIf('m')) {
        NodePointer type = demangleType();
        if (!type)
          return failure();
        appendNode(Node::Kind::Metaclass)->push_back_child(type);
        return true;
      }
    if (Mangled.nextIf('n')) {
        NodePointer type = demangleType();
        if (!type)
            return failure();
        appendNode(Node::Kind::NominalTypeDescriptor)->push_back_child(type);
        return true;
    }
      Directness d = demangleDirectness();
      appendNode(Node::Kind::Directness, toString(d));
      NodePointer type = demangleType();
      if (!type)
        return failure();
      appendNode(Node::Kind::TypeMetadata)->push_back_child(type);
      return true;
    }
    if (Mangled.nextIf('n')) {
      if (Mangled.nextIf('k') && Mangled.nextIf('_')) {
        bool entity_ok = demangleEntity(appendNode(Node::Kind::ProtocolWitness));
        if (!entity_ok)
          return failure();
        return true;
      } else
        return failure();
    }
    if (Mangled.nextIf('t')) {
      NodePointer type = demangleType();
      if (!type)
        return failure();
      appendNode(type);
      return true;
    }
    if (Mangled.nextIf('w')) {
      ValueWitnessKind w = demangleValueWitnessKind();
      if (w == ValueWitnessKind::Unknown)
        return failure();
      NodePointer type = demangleType();
      if (!type)
        return failure();
      appendNode(Node::Kind::ValueWitnessKind, toString(w))->push_back_child(type);
      return true;
    }
    if (Mangled.nextIf('W')) {
      if (Mangled.nextIf('V')) {
        NodePointer type = demangleType();
        if (!type)
          return failure();
        appendNode(Node::Kind::ValueWitnessTable)->push_back_child(type);
        return true;
      }
      if (Mangled.nextIf('o')) {
        bool entity_ok = demangleEntity(appendNode(Node::Kind::WitnessTableOffset));
        if (!entity_ok)
          return failure();
        return true;
      }
      if (Mangled.nextIf('v')) {
        Directness d = demangleDirectness();
        appendNode(Node::Kind::Directness, toString(d));
        bool entity_ok = demangleEntity(appendNode(Node::Kind::FieldOffset));
        if (!entity_ok)
          return failure();
        return true;
      }
      if (Mangled.nextIf('P')) {
        NodePointer conformance = demangleProtocolConformance();
        if (!conformance)
          return failure();
        appendNode(Node::makeNodePointer(Node::Kind::ProtocolWitnessTable))->push_back_child(conformance);
        return true;
      }
      if (Mangled.nextIf('Z')) {
        NodePointer conformance = demangleProtocolConformance();
        if (!conformance)
          return failure();
        appendNode(Node::makeNodePointer(Node::Kind::LazyProtocolWitnessTableAccessor))->push_back_child(conformance);
        return true;
      }
      if (Mangled.nextIf('z')) {
        NodePointer conformance = demangleProtocolConformance();
        if (!conformance)
          return failure();
        appendNode(Node::makeNodePointer(Node::Kind::LazyProtocolWitnessTableTemplate))->push_back_child(conformance);
        return true;
      }
      if (Mangled.nextIf('D')) {
        NodePointer conformance = demangleProtocolConformance();
        if (!conformance)
          return failure();
        appendNode(Node::makeNodePointer(Node::Kind::DependentProtocolWitnessTableGenerator))->push_back_child(conformance);
        return true;
      }
      if (Mangled.nextIf('d')) {
        NodePointer conformance = demangleProtocolConformance();
        if (!conformance)
          return failure();
        appendNode(Node::makeNodePointer(Node::Kind::DependentProtocolWitnessTableTemplate))->push_back_child(conformance);
        return true;
      }
      return failure();
    }
    if (Mangled.nextIf('T')) {
      if (Mangled.nextIf('b')) {
        NodePointer type = demangleType();
        if (!type)
          return failure();
        appendNode(Node::Kind::BridgeToBlockFunction)->push_back_child(type);
        return true;
      }
      return failure();
    }
    if (Mangled.nextIf('L')) {
      bool entity_ok = demangleEntity(appendNode(Node::Kind::LocalEntity));
      if (!entity_ok)
        return failure();
      return true;
    }
    NodePointer decl = Node::makeNodePointer(Node::Kind::Declaration);
    appendNode(decl);
    if (!demangleEntity(decl))
      return failure();
    return true;
  }

  std::string demangleOperator() {
    static const char op_char_table[] = "& @/= >    <*!|+ %-~   ^ .";
    size_t length;
    if (demangleNatural(length)) {
      if (Mangled.hasAtLeast(length)) {
        std::string op_base = Mangled.slice(length);
        Mangled.advanceOffset(length);
        DemanglerPrinter op;
        size_t op_base_size = op_base.size();
        for (size_t idx = 0; idx < op_base_size; ++idx) {
          char c = op_base[idx];
          if (c < 'a' || c > 'z')
            return "";
          char o = op_char_table[c - 'a'];
          if (o == ' ')
            return "";
          op << o;
        }
        return op.str();
      } else
        return "";
    }
    return "";
  }

  NodePointer demangleIdentifier() {
    if (!Mangled)
      return nullptr;
    if (Mangled.nextIf('o')) {
      char op_mode = Mangled.next();
      if (op_mode != 'p' && op_mode != 'P' && op_mode != 'i')
        return nullptr;
      std::string operatr = demangleOperator();
      if (operatr.size()) {
        switch (op_mode) {
        case 'p':
          return Node::makeNodePointer(Node::Kind::PrefixOperator, operatr);
        case 'P':
          return Node::makeNodePointer(Node::Kind::PostfixOperator, operatr);
        case 'i':
          return Node::makeNodePointer(Node::Kind::InfixOperator, operatr);
        default:
          return nullptr;
        }
      }
    }
    size_t length;
    if (demangleNatural(length)) {
      if (Mangled.hasAtLeast(length)) {
        auto identifier = Mangled.slice(length);
        Mangled.advanceOffset(length);
        return Node::makeNodePointer(Node::Kind::Identifier, identifier);
      }
    }
    return nullptr;
  }

  bool demangleIndex(size_t &natural) {
    if (Mangled.nextIf('_')) {
      natural = 0;
      return true;
    }
    if (demangleNatural(natural)) {
      if (!Mangled.nextIf('_'))
        return false;
      natural++;
      return true;
    }
    return false;
  }

  NodePointer compactNode(NodePointer head, Node::Kind outputKind,
                          char separator = '.') {
    DemanglerPrinter printer;
    while (head) {
      printer << head->getText();
      head = head->getNextNode();
      if (head)
        printer << separator;
    }
    return Node::makeNodePointer(outputKind, printer.str());
  }

  Substitution demangleSubstitutionIndexWithProtocol() {
    if (!Mangled)
      return { Node::makeNodePointer(Node::Kind::Failure), IsProtocol::no };
    if (Mangled.nextIf('o'))
      return { Node::makeNodePointer(Node::Kind::Module,"ObjectiveC"), IsProtocol::no };
    if (Mangled.nextIf('C'))
      return { Node::makeNodePointer(Node::Kind::Module,"C"), IsProtocol::no };
    if (Mangled.nextIf('s'))
      return { Node::makeNodePointer(Node::Kind::Module,"swift"), IsProtocol::no };
    if (Mangled.nextIf('a')) {
      NodePointer type = Node::makeNodePointer(Node::Kind::Path);
      type->push_back_child(Node::makeNodePointer(Node::Kind::Module,"swift"))->setNextNode(
                            Node::makeNodePointer(Node::Kind::Structure,"Array"));
      return { type, IsProtocol::no };
    }
    if (Mangled.nextIf('b')) {
      NodePointer type = Node::makeNodePointer(Node::Kind::Path);
      type->push_back_child(Node::makeNodePointer(Node::Kind::Module,"swift"))->setNextNode(
                            Node::makeNodePointer(Node::Kind::Enum,"Bool"));
      return { type, IsProtocol::no };
    }
    if (Mangled.nextIf('c')) {
      NodePointer type = Node::makeNodePointer(Node::Kind::Path);
      type->push_back_child(Node::makeNodePointer(Node::Kind::Module,"swift"))->setNextNode(
                            Node::makeNodePointer(Node::Kind::Structure,"UnicodeScalar"));
      return { type, IsProtocol::no };
    }
    if (Mangled.nextIf('d')) {
      NodePointer type = Node::makeNodePointer(Node::Kind::Path);
      type->push_back_child(Node::makeNodePointer(Node::Kind::Module,"swift"))->setNextNode(
                            Node::makeNodePointer(Node::Kind::Structure,"Float64"));
      return { type, IsProtocol::no };
    }
    if (Mangled.nextIf('f')) {
      NodePointer type = Node::makeNodePointer(Node::Kind::Path);
      type->push_back_child(Node::makeNodePointer(Node::Kind::Module,"swift"))->setNextNode(
                            Node::makeNodePointer(Node::Kind::Structure,"Float32"));
      return { type, IsProtocol::no };
    }
    if (Mangled.nextIf('i')) {
      NodePointer type = Node::makeNodePointer(Node::Kind::Path);
      type->push_back_child(Node::makeNodePointer(Node::Kind::Module,"swift"))->setNextNode(
                            Node::makeNodePointer(Node::Kind::Structure,"Int64"));
      return { type, IsProtocol::no };
    }
    if (Mangled.nextIf('q')) {
      NodePointer type = Node::makeNodePointer(Node::Kind::Path);
      type->push_back_child(Node::makeNodePointer(Node::Kind::Module,"swift"))->setNextNode(
                            Node::makeNodePointer(Node::Kind::Enum,"Optional"));
      return { type, IsProtocol::no };
    }
    if (Mangled.nextIf('S')) {
      NodePointer type = Node::makeNodePointer(Node::Kind::Path);
      type->push_back_child(Node::makeNodePointer(Node::Kind::Module,"swift"))->setNextNode(
                            Node::makeNodePointer(Node::Kind::Structure,"String"));
      return { type, IsProtocol::no };
    }
    if (Mangled.nextIf('u')) {
      NodePointer type = Node::makeNodePointer(Node::Kind::Path);
      type->push_back_child(Node::makeNodePointer(Node::Kind::Module,"swift"))->setNextNode(
                            Node::makeNodePointer(Node::Kind::Structure,"UInt64"));
      return { type, IsProtocol::no };
    }
    size_t index_sub;
    if (!demangleIndex(index_sub))
      return { Node::makeNodePointer(Node::Kind::Failure), IsProtocol::no };
    if (index_sub >= Substitutions.size())
      return { Node::makeNodePointer(Node::Kind::Failure), IsProtocol::no };
    return Substitutions[index_sub];
  }

  NodePointer demangleSubstitutionIndex() {
    Substitution sub = demangleSubstitutionIndexWithProtocol();
    if (!sub.first)
      return nullptr;
    return NodePointer(new Node(*sub.first.getPtr()));
  }

  NodePointer demangleSubstitution() {
    if (Mangled.nextIf('S'))
      return demangleSubstitutionIndex();
    return nullptr;
  }

  NodePointer demangleModule() {
    char c = Mangled.peek();
    if (isStartOfIdentifier(c)) {
      NodePointer identifier = demangleIdentifier();
      if (!identifier)
        return nullptr;
      if (identifier->getKind() == Node::Kind::Identifier)
        identifier->setKind(Node::Kind::Module);
      NodePointer copy_identifier = Node::makeNodePointer(identifier->getKind(),identifier->getText());
      Substitutions.push_back( { copy_identifier, IsProtocol::no });
      return identifier;
    }
    if (c == 'S') {
      NodePointer identifier = demangleSubstitution();
      if (!identifier)
        return nullptr;
      if (identifier->getKind() != Node::Kind::Path)
        identifier->setKind(Node::Kind::Module);
      return identifier;
    }
    return nullptr;
  }

  NodePointer demangleDeclarationName(Node::Kind identifier_type) {
    Node::Kind context_type = Node::Kind::Unknown;
    NodePointer context;
    NodePointer identifier;
    context = demangleContextImpl(&context_type);
    if (!context)
      return nullptr;
    identifier = demangleIdentifier();
    if (!identifier)
      return nullptr;
    IsProtocol is_proto = IsProtocol::no;
    if (identifier_type != Node::Kind::Unknown) {
      identifier->setKind(identifier_type);
      if (identifier_type == Node::Kind::Protocol)
        is_proto = IsProtocol::yes;
    }
    else if (context_type != Node::Kind::Unknown) {
      identifier->setKind(context_type);
      if (context_type == Node::Kind::Protocol)
        is_proto = IsProtocol::yes;
    }
    if (context->getKind() == Node::Kind::Path) {
      context->push_back_child(identifier);
      NodePointer path = Node::makeNodePointer(Node::Kind::Path);
      straightenNestedDeclContext(context, path);
      context = path;
    }
    else {
      context->setNextNode(identifier);
      NodePointer path = Node::makeNodePointer(Node::Kind::Path);
      path->push_back_child(context);
      context = path;
    }
    NodePointer nominaltype = NodePointer(new Node(*context));
    Substitutions.push_back({ nominaltype, is_proto });
    return context;
  }

  NodePointer demangleProtocolName() {
    NodePointer proto = demangleProtocolNameImpl();
    if (proto)
    {
      NodePointer type = Node::makeNodePointer(Node::Kind::Type);
      type->push_back_child(proto);
      return type;
    }
    return nullptr;
  }
  
  NodePointer demangleProtocolNameImpl() {
    if (Mangled.nextIf('S')) {
      Substitution sub = demangleSubstitutionIndexWithProtocol();
      if (!sub.first)
        return nullptr;
      if (sub.second == IsProtocol::yes) {
        NodePointer subProto = Node::makeNodePointer(sub.first->getKind(),sub.first->getText());
        for (NodePointer child : *sub.first) {
          subProto->push_back_child(Node::makeNodePointer(child->getKind(),child->getText()));
        }
        return subProto;
      }
      NodePointer identifier = demangleIdentifier();
      if (!identifier)
        return nullptr;
      NodePointer nominaltype = Node::makeNodePointer(Node::Kind::Path);
      NodePointer copy_context = NodePointer(new Node(*sub.first.getPtr()));
      identifier->setKind(Node::Kind::Protocol);
      NodePointer copy_identifier = Node::makeNodePointer(identifier->getKind(),identifier->getText());
      nominaltype->push_back_child(copy_context)->setNextNode(copy_identifier);
      Substitutions.push_back({ nominaltype, IsProtocol::yes });
      return nominaltype;
    }
    NodePointer result;
    if ((result = demangleDeclarationName(Node::Kind::Protocol))) {
      return result;
    }
    return nullptr;
  }

  NodePointer demangleNominalType() {
    if (!Mangled)
      return nullptr;
    char c = Mangled.next();
    if (c == 'S')
      return demangleSubstitutionIndex();
    if (!isStartOfNominalType(c))
      return nullptr;
    NodePointer nominalType;
    if ((nominalType = demangleDeclarationName(nominalTypeMarkerToNodeKind(c))))
      return nominalType;
    return nullptr;
  }
  
  NodePointer demangleContextGreedy (NodePointer first) {
    NodePointer decl_ctx = Node::makeNodePointer(Node::Kind::DeclContext);
    NodePointer path = Node::makeNodePointer(Node::Kind::Path);
    path->push_back_child(first);
    decl_ctx->push_back_child(path);
    while (Mangled.hasAtLeast(1) && isStartOfIdentifier(Mangled.peek())) {
      path->push_back_child(demangleIdentifier());
    }
    MangledNameSource::Snapshot snapshot = Mangled.getSnapshot();
    NodePointer type = demangleType();
    if (!type)
      Mangled.resetToSnapshot(snapshot);
    else
      decl_ctx->push_back_child(type);
    return decl_ctx;
  }
  
  NodePointer demangleContextImpl(Node::Kind *inferred_context_type = nullptr,
                                  bool allow_greedy = false) {
    if (!Mangled)
      return nullptr;
    NodePointer demangled_ctx(nullptr);
    char c = Mangled.peek();
    if (c == 'F') {
      Mangled.next();
      demangled_ctx = Node::makeNodePointer(Node::Kind::Declaration);
      if (!demangleEntity(demangled_ctx))
        return NodePointer(new Node(Node::Kind::Failure, ""));
      else return demangled_ctx;
   }

    if (isStartOfIdentifier(c) || c == 'S') {
      demangled_ctx = demangleModule();
      if (allow_greedy && Mangled.hasAtLeast(1) && isStartOfIdentifier(Mangled.peek()))
        return demangleContextGreedy(demangled_ctx);
    }
    if (isStartOfNominalType(c)) {
      if (inferred_context_type) {
        *inferred_context_type = nominalTypeMarkerToNodeKind(c);
      }
      demangled_ctx = demangleNominalType();
    }
    if (c == 'P') {
      Mangled.next();
      demangled_ctx = demangleProtocolName();
    }
    return demangled_ctx;
  }
  
  bool straightenNestedDeclContext (NodePointer src, NodePointer& dest) {
    Node::Kind srcKind = src->getKind();
    switch (srcKind) {
      case swift::Demangle::Node::Kind::Class:
      case swift::Demangle::Node::Kind::Structure:
      case swift::Demangle::Node::Kind::Enum:
      case swift::Demangle::Node::Kind::Module:
      case swift::Demangle::Node::Kind::Protocol:
      case swift::Demangle::Node::Kind::Identifier: // should only happen for greedy context demanglings
        dest->push_back_child(Node::makeNodePointer(srcKind,src->getText()));
        break;
      case swift::Demangle::Node::Kind::Path:
        for (NodePointer child : *src) {
          straightenNestedDeclContext(child, dest);
        }
        break;
      default:
        return false;
    }
    return true;
  }
  
  NodePointer demangleContext(Node::Kind *inferred_context_type = nullptr,
                              bool allow_greedy = false) {
    if (!Mangled)
      return nullptr;
    NodePointer demangled_ctx(demangleContextImpl(inferred_context_type,allow_greedy));
    if (!demangled_ctx)
      return nullptr;
    NodePointer ret(Node::makeNodePointer(Node::Kind::Path));
    if (straightenNestedDeclContext(demangled_ctx, ret))
      return ret;
    else
      return demangled_ctx;
  }

  std::pair<NodePointer,NodePointer> demangleDecl() {
    std::pair<NodePointer,NodePointer> ret;
    NodePointer identifier = demangleIdentifier();
    if (!identifier)
      return ret;
    NodePointer type = demangleType();
    if (!type)
      return ret;
    ret.first = identifier;
    ret.second = type;
    return ret;
  }

  NodePointer demangleProtocolList() {
    NodePointer proto_list = Node::makeNodePointer(Node::Kind::ProtocolList);
    NodePointer type_list = Node::makeNodePointer(Node::Kind::TypeList);
    proto_list->push_back_child(type_list);
    if (Mangled.nextIf('_')) {
      return proto_list;
    }
    NodePointer proto = demangleProtocolName();
    if (!proto)
      return nullptr;
    type_list->push_back_child(proto);
    while (Mangled.nextIf('_') == false) {
      proto = demangleProtocolName();
      if (!proto)
        return nullptr;
      type_list->push_back_child(proto);
    }
    return proto_list;
  }

  NodePointer demangleProtocolConformance() {
    NodePointer type = demangleType();
    if (!type)
      return nullptr;
    NodePointer protocol = demangleProtocolName();
    if (!protocol)
      return nullptr;
    NodePointer context = demangleContext();
    if (!context)
      return nullptr;
    NodePointer proto_conformance =
        Node::makeNodePointer(Node::Kind::ProtocolConformance);
    proto_conformance->push_back_child(type);
    proto_conformance->push_back_child(protocol);
    proto_conformance->push_back_child(context);
    return proto_conformance;
  }

  Node::Kind getDeclContextType (NodePointer declContext) {
    if (declContext->getKind() != Node::Kind::Path)
      return Node::Kind::Unknown;
    for (NodePointer child : *declContext) {
      Node::Kind childKind = child->getKind();
      switch (childKind) {
        case Node::Kind::Class:
        case Node::Kind::Structure:
        case Node::Kind::Enum:
        case Node::Kind::Protocol:
          return childKind;
        default:
          continue;
      }
    }
    return Node::Kind::Unknown;
  }
    
  Node::Kind getTypeKind (NodePointer type) {
    if (type->getKind() != Node::Kind::Type)
      return Node::Kind::Unknown;
    if (type->size() == 0)
      return Node::Kind::Unknown;
    NodePointer child = type->child_at(0);
    if (child->getKind() == Node::Kind::Path)
      return getDeclContextType(child);
    return child->getKind();
  }
  
  bool demangleEntity(NodePointer decl) {
    NodePointer context = demangleContext();
    if (!context)
      return failure();
    Node::Kind idKind = getDeclContextType(context);
    if (Mangled.nextIf('D')) {
      if (idKind == Node::Kind::Class)
        context->push_back_child(Node::makeNodePointer(Node::Kind::Deallocator));
      else
        context->push_back_child(Node::makeNodePointer(Node::Kind::Destructor));
      decl->push_back_child(context);
      return true;
    }
    if (Mangled.nextIf('d')) {
      context->push_back_child(Node::makeNodePointer(Node::Kind::Destructor));
      decl->push_back_child(context);
      return true;
    }
    if (Mangled.nextIf('C')) {
      NodePointer type = demangleType();
      if (!type)
        return failure();
      if (idKind == Node::Kind::Class)
        context->push_back_child(Node::makeNodePointer(Node::Kind::Allocator));
      else
        context->push_back_child(Node::makeNodePointer(Node::Kind::Constructor));
      decl->push_back_child(context);
      decl->push_back_child(type);
      return true;
    }
    if (Mangled.nextIf('c')) {
      NodePointer type = demangleType();
      if (!type)
        return failure();
      context->push_back_child(Node::makeNodePointer(Node::Kind::Constructor));
      decl->push_back_child(context);
      decl->push_back_child(type);
      return true;
    }
    std::pair<NodePointer,NodePointer> demangledDecl = demangleDecl();
    if (demangledDecl.first.getPtr() == nullptr || demangledDecl.second.getPtr() == nullptr)
      return nullptr;
    if (Mangled.nextIf('a')) {
      context->push_back_child(demangledDecl.first);
      context->push_back_child(Node::makeNodePointer(Node::Kind::Addressor));
      decl->push_back_child(context);
      decl->push_back_child(demangledDecl.second);
    } else if (Mangled.nextIf('g')) {
      context->push_back_child(demangledDecl.first);
      context->push_back_child(Node::makeNodePointer(Node::Kind::Getter));
      decl->push_back_child(context);
      decl->push_back_child(demangledDecl.second);
    } else if (Mangled.nextIf('s')) {
      context->push_back_child(demangledDecl.first);
      context->push_back_child(Node::makeNodePointer(Node::Kind::Setter));
      decl->push_back_child(context);
      decl->push_back_child(demangledDecl.second);
    } else {
      context->push_back_child(demangledDecl.first);
      decl->push_back_child(context);
      decl->push_back_child(demangledDecl.second);
    }
    return true;
  }

  NodePointer demangleArchetypes() {
    DemanglerPrinter result_printer;
    NodePointer archetypes = Node::makeNodePointer(Node::Kind::ArchetypeList);
    while (true) {
      if (Mangled.nextIf('_')) {
        if (!Mangled)
          return nullptr;
        char c = Mangled.peek();
        if (c != '_' && c != 'S' && !isStartOfIdentifier(c))
          break;
        archetypes->push_back_child(Node::makeNodePointer(
            Node::Kind::ArchetypeRef, archetypeName(ArchetypeCount)));
      } else {
        NodePointer proto_list = demangleProtocolList();
        if (!proto_list)
          return nullptr;
        NodePointer arch_and_proto =
            Node::makeNodePointer(Node::Kind::ArchetypeAndProtocol);
        arch_and_proto->push_back_child(Node::makeNodePointer(
            Node::Kind::ArchetypeRef, archetypeName(ArchetypeCount)));
        arch_and_proto->push_back_child(proto_list);
        archetypes->push_back_child(arch_and_proto);
      }
      ++ArchetypeCount;
    }
    return archetypes;
  }

  NodePointer demangleArchetypeRef(size_t depth, size_t i) {
    if (depth == 0 && ArchetypeCount == 0)
      return Node::makeNodePointer(Node::Kind::ArchetypeRef, archetypeName(i));
    size_t length = ArchetypeCounts.size();
    if (depth >= length)
      return nullptr;
    size_t index = ArchetypeCounts[length - 1 - depth] + i;
    size_t max =
        (depth == 0) ? ArchetypeCount : ArchetypeCounts[length - depth];
    if (index >= max)
      return nullptr;
    return Node::makeNodePointer(Node::Kind::ArchetypeRef,
                                 archetypeName(index));
  }
  
  NodePointer demangleArchetypeType() {
    auto makeSelfType = [&](NodePointer proto) -> NodePointer {
      NodePointer selfType
        = Node::makeNodePointer(Node::Kind::SelfTypeRef);
      selfType->push_back_child(proto);
      Substitutions.push_back({ selfType, IsProtocol::no });
      return selfType;
    };
    
    auto makeAssociatedType = [&](NodePointer root) -> NodePointer {
      NodePointer name = demangleIdentifier();
      if (!name) return nullptr;
      NodePointer assocType
        = Node::makeNodePointer(Node::Kind::AssociatedTypeRef);
      assocType->push_back_child(root);
      assocType->push_back_child(name);
      Substitutions.push_back({ assocType, IsProtocol::no });
      return assocType;
    };
    
    if (Mangled.nextIf('P')) {
      NodePointer proto = demangleProtocolName();
      if (!proto) return nullptr;
      return makeSelfType(proto);
    }
    
    if (Mangled.nextIf('Q')) {
      NodePointer root = demangleArchetypeType();
      if (!root) return nullptr;
      return makeAssociatedType(root);
    }
    if (Mangled.nextIf('S')) {
      Substitution sub = demangleSubstitutionIndexWithProtocol();
      if (!sub.first) return nullptr;
      if (sub.second == IsProtocol::yes)
        return makeSelfType(sub.first);
      else
        return makeAssociatedType(sub.first);
    }
    if (Mangled.nextIf('d')) {
      size_t depth, index;
      if (!demangleIndex(depth))
        return nullptr;
      if (!demangleIndex(index))
        return nullptr;
      return demangleArchetypeRef(depth + 1, index);
    }
    if (Mangled.nextIf('q')) {
      size_t index;
      if (!demangleIndex(index))
        return nullptr;
      DemanglerPrinter printer;
      printer << index;
      NodePointer index_node = Node::makeNodePointer(Node::Kind::Number,printer.str());
      NodePointer decl_ctx = Node::makeNodePointer(Node::Kind::DeclContext);
      NodePointer ctx = demangleContext(nullptr,true);
      if (!ctx)
        return nullptr;
      decl_ctx->push_back_child(ctx);
      NodePointer qual_atype = Node::makeNodePointer(Node::Kind::QualifiedArchetype);
      qual_atype->push_back_child(index_node);
      qual_atype->push_back_child(decl_ctx);
      return qual_atype;
    }
    size_t index;
    if (!demangleIndex(index))
      return nullptr;
    return demangleArchetypeRef(0, index);
  }

  NodePointer demangleTuple(IsVariadic isV) {
    NodePointer tuple = Node::makeNodePointer(
        isV == IsVariadic::yes ? Node::Kind::VariadicTuple
                               : Node::Kind::NonVariadicTuple);
    while (Mangled.nextIf('_') == false) {
      if (!Mangled)
        return nullptr;
      NodePointer tuple_element =
          Node::makeNodePointer(Node::Kind::TupleElement);
      NodePointer identifier;
      NodePointer type;
      if (isStartOfIdentifier(Mangled.peek())) {
        identifier = demangleIdentifier();
        if (!identifier)
          return nullptr;
        identifier->setKind(Node::Kind::TupleElementName);
      }
      type = demangleType();
      if (!type)
        return nullptr;
      if (type->getNextNode() && type->getKind() != Node::Kind::ProtocolList)
        type = compactNode(type, Node::Kind::TupleElementType);
      if (identifier)
        tuple_element->push_back_child(identifier);
      tuple_element->push_back_child(type);
      tuple->push_back_child(tuple_element);
    }
    return tuple;
  }
  
  NodePointer postProcessReturnTypeNode (NodePointer out_args) {
    NodePointer out_node = Node::makeNodePointer(Node::Kind::ReturnType);
    out_node->push_back_child(out_args);
    return out_node;
  }

  NodePointer demangleType() {
    NodePointer type = demangleTypeImpl();
    if (!type)
      return nullptr;
    NodePointer nodeType = Node::makeNodePointer(Node::Kind::Type);
    nodeType->push_back_child(type);
    return nodeType;
  }
  
  NodePointer demangleTypeImpl() {
    if (!Mangled)
      return nullptr;
    char c = Mangled.next();
    if (c == 'A') {
      size_t size;
      if (demangleNatural(size)) {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer array = Node::makeNodePointer(Node::Kind::ArrayType);
        array->push_back_child(type);
        array->push_back_child(Node::makeNodePointer(
            Node::Kind::Number, (DemanglerPrinter() << size).str()));
        return array;
      }
      return nullptr;
    }
    if (c == 'B') {
      if (!Mangled)
        return nullptr;
      c = Mangled.next();
      if (c == 'f') {
        size_t size;
        if (demangleBuiltinSize(size)) {
          return Node::makeNodePointer(
              Node::Kind::BuiltinTypeName,
              (DemanglerPrinter() << "Builtin.Float" << size).str());
        }
      }
      if (c == 'i') {
        size_t size;
        if (demangleBuiltinSize(size)) {
          return Node::makeNodePointer(
              Node::Kind::BuiltinTypeName,
              (DemanglerPrinter() << "Builtin.Int" << size).str());
        }
      }
      if (c == 'v') {
        size_t elts;
        if (demangleNatural(elts)) {
          if (!Mangled.nextIf('B'))
            return nullptr;
          if (Mangled.nextIf('i')) {
            size_t size;
            if (!demangleBuiltinSize(size))
              return nullptr;
            return Node::makeNodePointer(
                Node::Kind::BuiltinTypeName,
                (DemanglerPrinter() << "Builtin.Vec" << elts << "xInt" << size)
                    .str());
          }
          if (Mangled.nextIf('f')) {
            size_t size;
            if (!demangleBuiltinSize(size))
              return nullptr;
            return Node::makeNodePointer(
                Node::Kind::BuiltinTypeName,
                (DemanglerPrinter() << "Builtin.Vec" << elts << "xFloat"
                                    << size).str());
          }
          if (Mangled.nextIf('p'))
            return Node::makeNodePointer(
                Node::Kind::BuiltinTypeName,
                (DemanglerPrinter() << "Builtin.Vec" << elts << "xRawPointer")
                    .str());
        }
      }
      if (c == 'O')
        return Node::makeNodePointer(Node::Kind::BuiltinTypeName,
                                     "Builtin.ObjCPointer");
      if (c == 'o')
        return Node::makeNodePointer(Node::Kind::BuiltinTypeName,
                                     "Builtin.ObjectPointer");
      if (c == 'p')
        return Node::makeNodePointer(Node::Kind::BuiltinTypeName,
                                     "Builtin.RawPointer");
      if (c == 'w')
        return Node::makeNodePointer(Node::Kind::BuiltinTypeName,
                                     "Builtin.Word");
      return nullptr;
    }
    if (c == 'a')
      return demangleDeclarationName(Node::Kind::Identifier);

    if (c == 'b') {
      NodePointer in_args = demangleType();
      if (!in_args)
        return nullptr;
      NodePointer out_args = demangleType();
      if (!out_args)
        return nullptr;
      NodePointer block = Node::makeNodePointer(Node::Kind::ObjCBlock);
      NodePointer in_node = Node::makeNodePointer(Node::Kind::ArgumentTuple);
      block->push_back_child(in_node);
      in_node->push_back_child(in_args);
      block->push_back_child(postProcessReturnTypeNode(out_args));
      return block;
    }
    if (c == 'E') {
      if (!Mangled.nextIf('R'))
        return nullptr;
      if (!Mangled.nextIf('R'))
        return nullptr;
      return Node::makeNodePointer(Node::Kind::ErrorType, std::string());
    }
    if (c == 'F') {
      NodePointer in_args = demangleType();
      if (!in_args)
        return nullptr;
      NodePointer out_args = demangleType();
      if (!out_args)
        return nullptr;
      NodePointer block = Node::makeNodePointer(Node::Kind::FunctionType);
      NodePointer in_node = Node::makeNodePointer(Node::Kind::ArgumentTuple);
      block->push_back_child(in_node);
      in_node->push_back_child(in_args);
      block->push_back_child(postProcessReturnTypeNode(out_args));
      return block;
    }
    if (c == 'f') {
      NodePointer in_args = demangleTypeImpl();
      if (!in_args)
        return nullptr;
      NodePointer out_args = demangleType();
      if (!out_args)
        return nullptr;
      NodePointer block =
          Node::makeNodePointer(Node::Kind::UncurriedFunctionType);
      block->push_back_child(in_args);
      block->push_back_child(postProcessReturnTypeNode(out_args));
      return block;
    }
    if (c == 'G') {
      NodePointer type_list = Node::makeNodePointer(Node::Kind::TypeList);
      NodePointer unboundType = demangleType();
      if (!unboundType)
        return nullptr;
      if (Mangled.isEmpty())
        return nullptr;
      while (Mangled.peek() != '_') {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        type_list->push_back_child(type);
        if (Mangled.isEmpty())
          return nullptr;
      }
      Mangled.next();
      Node::Kind bound_type_kind = Node::Kind::Unknown;
      switch (getTypeKind(unboundType)) {
        case Node::Kind::Class:
          bound_type_kind = Node::Kind::BoundGenericClass;
          break;
        case Node::Kind::Structure:
          bound_type_kind = Node::Kind::BoundGenericStructure;
          break;
        case Node::Kind::Enum:
          bound_type_kind = Node::Kind::BoundGenericEnum;
          break;
        default:
          assert(false && "trying to make a generic type application for a non class|struct|enum");
      }
      NodePointer type_application =
          Node::makeNodePointer(bound_type_kind);
      type_application->push_back_child(unboundType);
      type_application->push_back_child(type_list);
      return type_application;
    }
    if (c == 'M') {
      NodePointer type = demangleType();
      if (!type)
        return nullptr;
      NodePointer metatype = Node::makeNodePointer(Node::Kind::MetaType);
      metatype->push_back_child(type);
      return metatype;
    }
    if (c == 'P') {
      return demangleProtocolList();
    }
    if (c == 'Q') {
      return demangleArchetypeType();
    }
    if (c == 'R') {
      NodePointer inout = Node::makeNodePointer(Node::Kind::InOut);
      NodePointer type = demangleTypeImpl();
      if (!type)
        return nullptr;
      inout->push_back_child(type);
      return inout;
    }
    if (c == 'S') {
      return demangleSubstitutionIndex();
    }
    if (c == 'T') {
      return demangleTuple(IsVariadic::no);
    }
    if (c == 't') {
      return demangleTuple(IsVariadic::yes);
    }
    if (c == 'U') {
      ArchetypeCounts.push_back(ArchetypeCount);
      NodePointer archetypes = demangleArchetypes();
      if (!archetypes)
        return nullptr;
      NodePointer base = demangleType();
      if (!base)
        return nullptr;
      ArchetypeCount = ArchetypeCounts.back();
      ArchetypeCounts.pop_back();
      NodePointer generics = Node::makeNodePointer(Node::Kind::GenericType);
      generics->push_back_child(archetypes);
      generics->push_back_child(base);
      return generics;
    }
    if (c == 'X') {
      if (Mangled.nextIf('o')) {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer unowned = Node::makeNodePointer(Node::Kind::Unowned);
        unowned->push_back_child(type);
        return unowned;
      }
      if (Mangled.nextIf('w')) {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer weak = Node::makeNodePointer(Node::Kind::Weak);
        weak->push_back_child(type);
        return weak;
      }
      return nullptr;
    }
    if (isStartOfNominalType(c)) {
      NodePointer nominal_type = demangleDeclarationName(nominalTypeMarkerToNodeKind(c));
      return nominal_type;
    }
    return nullptr;
  }

  class MangledNameSource {
  public:
    
    typedef void* Snapshot;
    
    MangledNameSource(StringRef mangled);

    char peek();

    bool nextIf(char c);

    char next();

    bool isEmpty();

    explicit operator bool();

    std::string slice(size_t size);

    std::string getString();

    size_t getOffset();

    size_t getSize();

    bool hasAtLeast(size_t n);

    void advanceOffset(size_t by);

    Snapshot getSnapshot();
    
    void resetToSnapshot(Snapshot snap);
    
  private:
    StringRef Mangled;
    size_t Offset;
  };

  std::vector<Substitution> Substitutions;
  std::vector<int> ArchetypeCounts;
  int ArchetypeCount;
  MangledNameSource Mangled;
  NodePointer RootNode;
  NodePointer CurrentNode;
};

Demangler::MangledNameSource::MangledNameSource(StringRef Mangled)
    : Mangled(Mangled), Offset(0) {}

char Demangler::MangledNameSource::peek() { return Mangled.front(); }

bool Demangler::MangledNameSource::nextIf(char c) {
  if (isEmpty())
    return false;
  char real_c = peek();
  if (real_c == c) {
    advanceOffset(1);
    return true;
  }
  return false;
}

char Demangler::MangledNameSource::next() {
  char c = peek();
  advanceOffset(1);
  return c;
}

bool Demangler::MangledNameSource::isEmpty() { return Mangled.empty(); }

Demangler::MangledNameSource::operator bool() { return isEmpty() == false; }

std::string Demangler::MangledNameSource::slice(size_t size) {
  return Mangled.substr(0, size);
}

std::string Demangler::MangledNameSource::getString() { return Mangled.data(); }

size_t Demangler::MangledNameSource::getOffset() { return Offset; }

size_t Demangler::MangledNameSource::getSize() { return Mangled.size(); }

bool Demangler::MangledNameSource::hasAtLeast(size_t n) {
  if (n > getSize())
    return false;
  return true;
}

void Demangler::MangledNameSource::advanceOffset(size_t by) {
  Offset += by;
  Mangled = Mangled.substr(by);
}

Demangler::MangledNameSource::Snapshot Demangler::MangledNameSource::getSnapshot() {
  return (void*)Offset;
}

void Demangler::MangledNameSource::resetToSnapshot(Snapshot snap) {
  Offset = reinterpret_cast<size_t>(snap);
}

NodePointer swift::Demangle::demangleSymbolAsNode(llvm::StringRef mangled, const DemangleOptions& options) {
  Demangler demangler(mangled);
  demangler.demangle();
  return demangler.getDemangled();
}

class NodePrinter
{
public:
  
  NodePrinter (NodePointer root, DemangleOptions options) :
  Printer(), Root (root), Options(options)
  {
  }
  
  std::string
  Print ()
  {
    toString(Root);
    return Printer.str();
  }
  
private:
  DemanglerPrinter Printer;
  NodePointer Root;
  DemangleOptions Options;
  
  void toStringChildren (Node::iterator begin, Node::iterator end, const char *sep = nullptr) {
    for (; begin != end;) {
      toString(*begin);
      ++begin;
      if (sep && begin != end)
        Printer << sep;
    }
  }
  
  void toStringChildren (NodePointer pointer, const char *sep = nullptr) {
    if (!pointer)
      return;
    Node::iterator begin = pointer->begin(), end = pointer->end();
    toStringChildren(begin, end, sep);
  }
  
  void toStringLifeCycleEntity (NodePointer pointer, const char* name) {
    NodePointer child = pointer->child_at(0);
    if (child->getKind() == Node::Kind::Path) {
      toString(child);
      Printer << ".";
    }
    else {
      while (child) {
        Printer << child->getText();
        child = child->getNextNode();
        if (child)
          Printer << ".";
      }
    }
    Printer << name;
    if (pointer->size() > 1) {
      child = pointer->child_at(1);
      if (child) {
        Printer << " : ";
        toString(child);
      }
    }
  }
  
  NodePointer getFirstChildOfKind (NodePointer pointer, Node::Kind kind) {
    if (!pointer)
      return nullptr;
    for (NodePointer child : *pointer) {
      if (child && child->getKind() == kind)
        return child;
    }
    return nullptr;
  }
  
  bool typeNeedsColonForDecl (NodePointer type) {
    if (!type)
      return false;
    if (type->size() == 0)
      return false;
    NodePointer child = type->child_at(0);
    if (!child)
      return false;
    Node::Kind child_kind = child->getKind();
    switch (child_kind) {
      case Node::Kind::UncurriedFunctionType:
      case Node::Kind::FunctionType:
        return false;
      case Node::Kind::GenericType:
        return typeNeedsColonForDecl(getFirstChildOfKind(type, Node::Kind::UncurriedFunctionType));
      default:
        return true;
    }
  }
  
  void toStringBoundGenericNoSugar (NodePointer pointer)
  {
    if (pointer->size() < 2)
      return;
    NodePointer typelist = pointer->child_at(1);
    if (!typelist)
      return;
    NodePointer type0 = pointer->child_at(0);
    if (!type0)
      return;
    toString(type0);
    Printer << "<";
    toStringChildren(typelist, ", ");
    Printer << ">";
  }
  
  enum class SugarType
  {
    None,
    Optional,
    Slice
  };
  
  SugarType
  findSugar (NodePointer pointer)
  {
    if (pointer->size() == 1 && pointer->getKind() == Node::Kind::Type)
      return findSugar(pointer->child_at(0));
    
    if (pointer->size() != 2)
      return SugarType::None;
    
    if (pointer->getKind() != Node::Kind::BoundGenericEnum &&
        pointer->getKind() != Node::Kind::BoundGenericStructure)
      return SugarType::None;
    
    if (pointer->getKind() == Node::Kind::BoundGenericEnum)
    {
      bool is_optional = false;
      // swift.Optional
      NodePointer child0 = pointer->child_at(0);
      NodePointer child1 = pointer->child_at(1);
      if (child0->getKind() == Node::Kind::Type && child0->size() == 1)
      {
        if (child1->getKind() == Node::Kind::TypeList && child1->size() == 1)
        {
          child0 = child0->child_at(0);
          if (child0->getKind() == Node::Kind::Path && child0->size() == 2)
          {
            NodePointer module = child0->child_at(0);
            NodePointer name = child0->child_at(1);
            if (module->getKind() == Node::Kind::Module &&
                name->getKind() == Node::Kind::Enum &&
                module->getText() == "swift" &&
                name->getText() == "Optional")
            {
              is_optional = true;
            }
          }
        }
      }
      
      return (is_optional ? SugarType::Optional : SugarType::None);
    }
    else /*if (pointer->getKind() == Node::Kind::BoundGenericStructure)*/
    {
      bool is_slice = false;
      // swift.Slice
      NodePointer child0 = pointer->child_at(0);
      NodePointer child1 = pointer->child_at(1);
      if (child0->getKind() == Node::Kind::Type && child0->size() == 1)
      {
        if (child1->getKind() == Node::Kind::TypeList && child1->size() == 1)
        {
          child0 = child0->child_at(0);
          if (child0->getKind() == Node::Kind::Path && child0->size() == 2)
          {
            NodePointer module = child0->child_at(0);
            NodePointer name = child0->child_at(1);
            if (module->getKind() == Node::Kind::Module &&
                name->getKind() == Node::Kind::Structure &&
                module->getText() == "swift" &&
                name->getText() == "Array")
            {
              is_slice = true;
            }
          }
        }
      }
      return (is_slice ? SugarType::Slice : SugarType::None);
    }

    return SugarType::None;
    
  }
  
  void toStringBoundGeneric (NodePointer pointer) {
    if (pointer->size() < 2)
      return;
    if (pointer->size() != 2)
    {
      toStringBoundGenericNoSugar(pointer);
      return;
    }

    if (Options.SynthesizeSugarOnTypes == false || pointer->getKind() == Node::Kind::BoundGenericClass)
    {
      // no sugar here
      toStringBoundGenericNoSugar(pointer);
      return;
    }

    SugarType sugarType = findSugar(pointer);
    
    switch (sugarType)
    {
      case SugarType::None:
        toStringBoundGenericNoSugar(pointer);
        break;
      case SugarType::Optional: {
        NodePointer type = pointer->child_at(1)->child_at(0);
        bool needs_parens = false;
        if (findSugar(type) != SugarType::None)
          needs_parens = true;
        if (needs_parens)
          Printer << "(";
        toString(type);
        if (needs_parens)
          Printer << ")";
        Printer << "?";
      }
        break;
      case SugarType::Slice: {
        NodePointer type = pointer->child_at(1)->child_at(0);
        bool needs_parens = false;
        if (findSugar(type) != SugarType::None)
          needs_parens = true;
        if (needs_parens)
          Printer << "(";
        toString(type);
        if (needs_parens)
          Printer << ")";
        Printer << "[]";
      }
        break;
    }
  }
  
  void toString(NodePointer pointer) {
    while (pointer) {
      Node::Kind kind = pointer->getKind();
      switch (kind) {
        case swift::Demangle::Node::Kind::Failure:
          return;
        case swift::Demangle::Node::Kind::Directness:
          Printer << pointer->getText() << " ";
          pointer = pointer->getNextNode(); continue;
        case swift::Demangle::Node::Kind::LocalEntity:
          Printer << "local ";
          SWIFT_FALLTHROUGH;
        case swift::Demangle::Node::Kind::Declaration:
        {
          NodePointer path = getFirstChildOfKind(pointer, Node::Kind::Path);
          NodePointer type = getFirstChildOfKind(pointer, Node::Kind::Type);
          toString(path);
          if (typeNeedsColonForDecl(type))
            Printer << " : ";
          else
            Printer << " ";
          toString(type);
          break;
        }
        case swift::Demangle::Node::Kind::DeclContext:
        {
          toString(pointer->child_at(0));
          break;
        }
        case swift::Demangle::Node::Kind::Path:
        {
          toStringChildren(pointer, ".");
          break;
        }
        case swift::Demangle::Node::Kind::Type:
        {
          toString(pointer->child_at(0));
          break;
        }
        case swift::Demangle::Node::Kind::Module:
        case swift::Demangle::Node::Kind::Class:
        case swift::Demangle::Node::Kind::Structure:
        case swift::Demangle::Node::Kind::Enum:
          Printer << pointer->getText();
          break;
        case swift::Demangle::Node::Kind::Identifier:
          Printer << pointer->getText();
          break;
        case swift::Demangle::Node::Kind::FunctionType:
          toStringChildren(pointer);
          break;
        case swift::Demangle::Node::Kind::UncurriedFunctionType: {
          NodePointer metatype = pointer->child_at(0);
          if (!metatype)
            break;
          Printer << "(";
          toString(metatype);
          Printer << ")";
          NodePointer real_func = pointer->child_at(1);
          if (!real_func)
            break;
          real_func = real_func->child_at(0);
          toStringChildren(real_func);
          break;
        }
        case swift::Demangle::Node::Kind::ArgumentTuple: {
          bool need_parens = false;
          if (pointer->size() > 1)
            need_parens = true;
          else {
            if (pointer->size() == 0)
              need_parens = true;
            else {
              Node::Kind child0_kind = pointer->child_at(0)->child_at(0)->getKind();
              if (child0_kind != Node::Kind::VariadicTuple &&
                  child0_kind != Node::Kind::NonVariadicTuple)
                need_parens = true;
            }
          }
          if (need_parens)
            Printer << "(";
          toStringChildren(pointer);
          if (need_parens)
            Printer << ")";
          break;
        }
        case swift::Demangle::Node::Kind::NonVariadicTuple:
        case swift::Demangle::Node::Kind::VariadicTuple: {
          Printer << "(";
          toStringChildren(pointer, ", ");
          if (pointer->getKind() == swift::Demangle::Node::Kind::VariadicTuple)
            Printer << "...";
          Printer << ")";
          break;
        }
        case swift::Demangle::Node::Kind::TupleElement: {
          if (pointer->size() == 1) {
            NodePointer type = pointer->child_at(0);
            toString(type);
          } else if (pointer->size() == 2) {
            NodePointer id = pointer->child_at(0);
            NodePointer type = pointer->child_at(1);
            toString(id);
            toString(type);
          }
          break;
        }
        case swift::Demangle::Node::Kind::TupleElementName:
          Printer << pointer->getText() << " : ";
          break;
        case swift::Demangle::Node::Kind::TupleElementType:
          Printer << pointer->getText();
          break;
        case swift::Demangle::Node::Kind::ReturnType: {
          if (pointer->size() == 0)
            Printer << " -> " << pointer->getText();
          else {
            Printer << " -> ";
            toStringChildren(pointer);
          }
          break;
        }
        case swift::Demangle::Node::Kind::Weak:
          Printer << "[weak] ";
          toStringChildren(pointer);
          break;
        case swift::Demangle::Node::Kind::Unowned:
          Printer << "[unowned] ";
          toStringChildren(pointer);
          break;
        case swift::Demangle::Node::Kind::InOut:
          Printer << "@inout ";
          pointer = pointer->child_at(0); continue;
        case swift::Demangle::Node::Kind::ObjCAttribute:
          Printer << "[objc] ";
          pointer = pointer->getNextNode(); continue;
        case swift::Demangle::Node::Kind::BuiltinTypeName:
        case swift::Demangle::Node::Kind::Number:
          Printer << pointer->getText();
          break;
        case swift::Demangle::Node::Kind::ArrayType: {
          NodePointer type = pointer->child_at(0);
          NodePointer size = pointer->child_at(1);
          toString(type);
          Printer << "[";
          toString(size);
          Printer << "]";
          break;
        }
        case swift::Demangle::Node::Kind::InfixOperator:
          Printer << pointer->getText() << " [infix]";
          pointer = pointer->getNextNode(); continue;
        case swift::Demangle::Node::Kind::PrefixOperator:
          Printer << pointer->getText() << " [prefix]";
          pointer = pointer->getNextNode(); continue;
        case swift::Demangle::Node::Kind::PostfixOperator:
          Printer << pointer->getText() << " [postfix]";
          pointer = pointer->getNextNode(); continue;
        case swift::Demangle::Node::Kind::DependentProtocolWitnessTableGenerator:
          Printer << "dependent protocol witness table generator for ";
          pointer = pointer->child_at(0); continue;
        case swift::Demangle::Node::Kind::DependentProtocolWitnessTableTemplate:
          Printer << "dependent protocol witness table template for ";
          pointer = pointer->child_at(0); continue;
        case swift::Demangle::Node::Kind::LazyProtocolWitnessTableAccessor:
          Printer << "lazy protocol witness table accessor for ";
          pointer = pointer->child_at(0); continue;
        case swift::Demangle::Node::Kind::LazyProtocolWitnessTableTemplate:
          Printer << "lazy protocol witness table template for ";
          pointer = pointer->child_at(0); continue;
        case swift::Demangle::Node::Kind::ProtocolWitnessTable:
          Printer << "protocol witness table for ";
          pointer = pointer->child_at(0); continue;
        case swift::Demangle::Node::Kind::ProtocolWitness:
          Printer << "protocol witness for ";
        {
          NodePointer path = getFirstChildOfKind(pointer, Node::Kind::Path);
          NodePointer type = getFirstChildOfKind(pointer, Node::Kind::Type);
          toString(path);
          if (typeNeedsColonForDecl(type))
            Printer << " : ";
          else
            Printer << " ";
          toString(type);
          break;
        }
        case swift::Demangle::Node::Kind::FieldOffset:
          Printer << "field offset for ";
        {
          NodePointer path = getFirstChildOfKind(pointer, Node::Kind::Path);
          NodePointer type = getFirstChildOfKind(pointer, Node::Kind::Type);
          toString(path);
          if (Options.DisplayTypeOfIVarFieldOffset == false)
            break;
          if (typeNeedsColonForDecl(type))
            Printer << " : ";
          else
            Printer << " ";
          toString(type);
          break;
        }
        case swift::Demangle::Node::Kind::BridgeToBlockFunction:
          Printer << "bridge-to-block function for ";
          pointer = pointer->child_at(0); continue;
        case swift::Demangle::Node::Kind::GenericTypeMetadataPattern:
          Printer << "generic type metadata pattern for ";
          pointer = pointer->child_at(0); continue;
        case swift::Demangle::Node::Kind::Metaclass:
          Printer << "metaclass for ";
          pointer = pointer->child_at(0); continue;
        case swift::Demangle::Node::Kind::TypeMetadata:
          Printer << "type metadata for ";
          pointer = pointer->child_at(0); continue;
        case swift::Demangle::Node::Kind::NominalTypeDescriptor:
          Printer << "nominal type descriptor for ";
          pointer = pointer->child_at(0); continue;
        case swift::Demangle::Node::Kind::ValueWitnessKind:
          Printer << pointer->getText() << " value witness for ";
          pointer = pointer->child_at(0); continue;
        case swift::Demangle::Node::Kind::ValueWitnessTable:
          Printer << "value witness table for ";
          pointer = pointer->child_at(0); continue;
        case swift::Demangle::Node::Kind::WitnessTableOffset:
          Printer << "witness table offset for ";
        {
          NodePointer path = getFirstChildOfKind(pointer, Node::Kind::Path);
          NodePointer type = getFirstChildOfKind(pointer, Node::Kind::Type);
          toString(path);
          if (typeNeedsColonForDecl(type))
            Printer << " : ";
          else
            Printer << " ";
          toString(type);
          break;
        }
        case swift::Demangle::Node::Kind::BoundGenericClass:
        case swift::Demangle::Node::Kind::BoundGenericStructure:
        case swift::Demangle::Node::Kind::BoundGenericEnum:
          toStringBoundGeneric(pointer);
          break;
        case swift::Demangle::Node::Kind::ObjCBlock: {
          Printer << "@objc_block ";
          NodePointer tuple = pointer->child_at(0);
          NodePointer rettype = pointer->child_at(1);
          toString(tuple);
          toString(rettype);
          break;
        }
        case swift::Demangle::Node::Kind::MetaType: {
          NodePointer type = pointer->child_at(0);
          toString(type);
          Printer << ".metatype";
          break;
        }
        case swift::Demangle::Node::Kind::Protocol:
        case swift::Demangle::Node::Kind::ArchetypeRef:
          Printer << pointer->getText();
          break;
        case swift::Demangle::Node::Kind::AssociatedTypeRef:
          toString(pointer->child_at(0));
          Printer << '.' << pointer->child_at(1)->getText();
          break;
        case swift::Demangle::Node::Kind::SelfTypeRef:
          toString(pointer->child_at(0));
          Printer << ".Self";
          break;
        case swift::Demangle::Node::Kind::ProtocolList: {
          NodePointer type_list = pointer->child_at(0);
          if (!type_list)
            break;
          bool needs_proto_marker = (type_list->size() != 1);
          if (needs_proto_marker)
            Printer << "protocol<";
          toStringChildren(type_list, ", ");
          if (needs_proto_marker)
            Printer << ">";
          break;
        }
        case swift::Demangle::Node::Kind::ArchetypeList: {
          if (pointer->size() == 0)
            break;
          Printer << "<";
          toStringChildren(pointer, ", ");
          Printer << ">";
          break;
        }
        case swift::Demangle::Node::Kind::QualifiedArchetype: {
          if (pointer->size() < 2)
            break;
          NodePointer number = pointer->child_at(0);
          NodePointer decl_ctx = pointer->child_at(1);
          Printer << "(archetype " << number->getText() << " of ";
          toString(decl_ctx);
          Printer << ")";
          break;
        }
        case swift::Demangle::Node::Kind::GenericType: {
          NodePointer atype_list = pointer->child_at(0);
          NodePointer fct_type = pointer->child_at(1)->child_at(0);
          toString(atype_list);
          toString(fct_type);
          break;
        }
        case swift::Demangle::Node::Kind::Addressor: {
          Printer << "addressor";
          break;
        }
        case swift::Demangle::Node::Kind::Getter: {
          Printer << "getter";
          break;
        }
        case swift::Demangle::Node::Kind::Setter: {
          Printer << "setter";
          break;
        }
        case swift::Demangle::Node::Kind::Allocator:
          Printer << "__allocating_init";
          break;
        case swift::Demangle::Node::Kind::Constructor:
          Printer << "init";
          break;
        case swift::Demangle::Node::Kind::Destructor:
          Printer << "destructor";
          break;
        case swift::Demangle::Node::Kind::Deallocator:
          Printer << "__deallocating_destructor";
          break;
        case swift::Demangle::Node::Kind::ProtocolConformance: {
          NodePointer child0 = pointer->child_at(0);
          NodePointer child1 = pointer->child_at(1);
          NodePointer child2 = pointer->child_at(2);
          if (!child0 || !child1 || !child2)
            break;
          toString(child0);
          Printer << " : ";
          toString(child1);
          Printer << " in ";
          toString(child2);
          break;
        }
        case swift::Demangle::Node::Kind::TypeList:
          toStringChildren(pointer);
          break;
        case swift::Demangle::Node::Kind::ArchetypeAndProtocol: {
          NodePointer child0 = pointer->child_at(0);
          NodePointer child1 = pointer->child_at(1);
          toString(child0);
          Printer << " : ";
          toString(child1);
          break;
        }
        case swift::Demangle::Node::Kind::Unknown:
          break;
        case swift::Demangle::Node::Kind::ErrorType:
          Printer << "<ERROR TYPE>";
          break;
      }
      pointer.reset();
    }
  }
};

std::string swift::Demangle::nodeToString(NodePointer pointer, const DemangleOptions& options) {
  if (!pointer)
    return "";
  NodePrinter printer(pointer, options);
  return printer.Print();
}

std::string swift::Demangle::demangleSymbolAsString(llvm::StringRef mangled, const DemangleOptions& options) {
  std::string demangling = nodeToString(demangleSymbolAsNode(mangled, options), options);
  if (demangling.empty())
    return mangled.str();
  return demangling;
}
