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
#include "swift/Basic/LLVM.h"
#include "llvm/Support/raw_ostream.h"
#include <functional>
#include <tuple>
#include <vector>

using namespace swift;
using namespace Demangle;

swift::Demangle::Node::Node(Kind k, std::string t)
    : ParentNode(nullptr), TextContent(t), NodeKind(k), NextNode(nullptr),
      Children() {}

llvm::StringRef swift::Demangle::Node::getText() { return TextContent; }
void swift::Demangle::Node::setText(std::string t) { TextContent.assign(t); }

swift::Demangle::Node::Kind swift::Demangle::Node::getKind() {
  return NodeKind;
}
void swift::Demangle::Node::setKind(swift::Demangle::Node::Kind k) {
  NodeKind = k;
}

NodePointer swift::Demangle::Node::getNextNode() { return NextNode; }
void swift::Demangle::Node::setNextNode(NodePointer n) {
  NextNode = n;
  if (ParentNode)
    ParentNode->push_back_child(n);
}
void swift::Demangle::Node::insertNextNode(NodePointer n) {
  if (!n || !NextNode) {
    setNextNode(n);
    return;
  }
  NodePointer prev_next = NextNode;
  NextNode = n;
  NodePointer end_of_chain = NextNode;
  while (end_of_chain->getNextNode())
    end_of_chain = end_of_chain->getNextNode();
  end_of_chain->setNextNode(prev_next);
}

void swift::Demangle::Node::push_back_child(NodePointer c) {
  Children.push_back(c);
  c->ParentNode = this;
}
void swift::Demangle::Node::push_front_child(NodePointer c) {
  Children.insert(begin(), c);
}
void swift::Demangle::Node::insert_child(swift::Demangle::Node::iterator i,
                                         NodePointer c) {
  Children.insert(i, c);
}

NodePointer
swift::Demangle::Node::child_at(swift::Demangle::Node::size_type pos) {
  if (pos < size())
    return Children[pos];
  return NodePointer(nullptr);
}

swift::Demangle::Node::iterator swift::Demangle::Node::begin() {
  return Children.begin();
}
swift::Demangle::Node::iterator swift::Demangle::Node::end() {
  return Children.end();
}

swift::Demangle::Node::const_iterator swift::Demangle::Node::begin() const {
  return Children.begin();
}
swift::Demangle::Node::const_iterator swift::Demangle::Node::end() const {
  return Children.end();
}

swift::Demangle::Node::size_type swift::Demangle::Node::size() {
  return Children.size();
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
    return Node::Kind::Union;
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

  typedef std::pair<std::string, IsProtocol> Substitution;

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
    AllocateBuffer, AssignWithCopy, AssignWithTake, DeallocateBuffer, Destroy,
        DestroyBuffer, InitializeBufferWithCopyOfBuffer,
        InitializeBufferWithCopy, InitializeWithCopy, InitializeBufferWithTake,
        InitializeWithTake, ProjectBuffer, Typeof, StoreExtraInhabitant,
        GetExtraInhabitantIndex, GetUnionTag, InplaceProjectUnionData, Unknown
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
    case ValueWitnessKind::GetUnionTag:
      return "getUnionTag";
    case ValueWitnessKind::InplaceProjectUnionData:
      return "inplaceProjectUnionData";
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
      return ValueWitnessKind::GetUnionTag;
    if (c1 == 'u' && c2 == 'p')
      return ValueWitnessKind::InplaceProjectUnionData;
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
        appendNode(Node::Kind::Directness, toString(d));
        appendNode(Node::Kind::GenericTypeMetadataPattern);
        NodePointer type = demangleType();
        if (!type)
          return failure();
        appendNode(type);
        return true;
      }
      if (Mangled.nextIf('m')) {
        appendNode(Node::Kind::Metaclass);
        NodePointer type = demangleType();
        if (!type)
          return failure();
        appendNode(type);
        return true;
      }
      Directness d = demangleDirectness();
      appendNode(Node::Kind::Directness, toString(d));
      NodePointer type = demangleType();
      if (!type)
        return failure();
      appendNode(Node::Kind::TypeMetadata);
      appendNode(type);
      return true;
    }
    if (Mangled.nextIf('n')) {
      if (Mangled.nextIf('k') && Mangled.nextIf('_')) {
        NodePointer entity = demangleEntity();
        if (!entity)
          return failure();
        appendNode(Node::Kind::ProtocolWitness);
        appendNode(entity);
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
      appendNode(Node::Kind::ValueWitnessKind, toString(w));
      appendNode(type);
      return true;
    }
    if (Mangled.nextIf('W')) {
      if (Mangled.nextIf('V')) {
        NodePointer type = demangleType();
        if (!type)
          return failure();
        appendNode(Node::Kind::ValueWitnessTable);
        appendNode(type);
        return true;
      }
      if (Mangled.nextIf('o')) {
        NodePointer entity = demangleEntity();
        if (!entity)
          return failure();
        appendNode(Node::Kind::WitnessTableOffset);
        appendNode(entity);
        return true;
      }
      if (Mangled.nextIf('v')) {
        Directness d = demangleDirectness();
        appendNode(Node::Kind::Directness, toString(d));
        NodePointer entity = demangleEntity();
        if (!entity)
          return failure();
        appendNode(Node::Kind::FieldOffset);
        appendNode(entity);
        return true;
      }
      if (Mangled.nextIf('P')) {
        NodePointer conformance = demangleProtocolConformance();
        if (!conformance)
          return failure();
        appendNode(Node::makeNodePointer(Node::Kind::ProtocolWitnessTable));
        appendNode(conformance);
        return true;
      }
      if (Mangled.nextIf('Z')) {
        NodePointer conformance = demangleProtocolConformance();
        if (!conformance)
          return failure();
        appendNode(Node::makeNodePointer(
            Node::Kind::LazyProtocolWitnessTableAccessor));
        appendNode(conformance);
        return true;
      }
      if (Mangled.nextIf('z')) {
        NodePointer conformance = demangleProtocolConformance();
        if (!conformance)
          return failure();
        appendNode(Node::makeNodePointer(
            Node::Kind::LazyProtocolWitnessTableTemplate));
        appendNode(conformance);
        return true;
      }
      if (Mangled.nextIf('D')) {
        NodePointer conformance = demangleProtocolConformance();
        if (!conformance)
          return failure();
        appendNode(Node::makeNodePointer(
            Node::Kind::DependentProtocolWitnessTableGenerator));
        appendNode(conformance);
        return true;
      }
      if (Mangled.nextIf('d')) {
        NodePointer conformance = demangleProtocolConformance();
        if (!conformance)
          return failure();
        appendNode(Node::makeNodePointer(
            Node::Kind::DependentProtocolWitnessTableTemplate));
        appendNode(conformance);
        return true;
      }
      return failure();
    }
    if (Mangled.nextIf('T')) {
      if (Mangled.nextIf('b')) {
        NodePointer type = demangleType();
        if (!type)
          return failure();
        appendNode(Node::Kind::BridgeToBlockFunction);
        appendNode(type);
        return true;
      }
      return failure();
    }
    if (Mangled.nextIf('L')) {
      NodePointer entity = demangleEntity();
      if (!entity)
        return failure();
      appendNode(Node::Kind::LocalEntity);
      appendNode(entity);
      return true;
    }
    NodePointer entity = demangleEntity();
    if (!entity)
      return failure();
    appendNode(entity);
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
      return { "", IsProtocol::no };
    if (Mangled.nextIf('o'))
      return { "ObjectiveC", IsProtocol::no };
    if (Mangled.nextIf('C'))
      return { "C", IsProtocol::no };
    if (Mangled.nextIf('s'))
      return { "swift", IsProtocol::no };
    if (Mangled.nextIf('a'))
      return { "swift.Slice", IsProtocol::no };
    if (Mangled.nextIf('b'))
      return { "swift.Bool", IsProtocol::no };
    if (Mangled.nextIf('c'))
      return { "swift.Char", IsProtocol::no };
    if (Mangled.nextIf('d'))
      return { "swift.Float64", IsProtocol::no };
    if (Mangled.nextIf('f'))
      return { "swift.Float32", IsProtocol::no };
    if (Mangled.nextIf('i'))
      return { "swift.Int64", IsProtocol::no };
    if (Mangled.nextIf('S'))
      return { "swift.String", IsProtocol::no };
    if (Mangled.nextIf('u'))
      return { "swift.UInt64", IsProtocol::no };
    size_t index_sub;
    if (!demangleIndex(index_sub))
      return { "", IsProtocol::no };
    if (index_sub >= Substitutions.size())
      return { "", IsProtocol::no };
    return Substitutions[index_sub];
  }

  NodePointer demangleSubstitutionIndex() {
    Substitution sub = demangleSubstitutionIndexWithProtocol();
    if (sub.first.empty())
      return nullptr;
    if (sub.second == IsProtocol::yes) {
      return Node::makeNodePointer(Node::Kind::Protocol, sub.first);
    } else {
      return Node::makeNodePointer(Node::Kind::Identifier, sub.first);
    }
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
      Substitutions.push_back({ identifier->getText(), IsProtocol::no });
      if (identifier->getKind() == Node::Kind::Identifier)
        identifier->setKind(Node::Kind::Module);
      return identifier;
    }
    if (c == 'S') {
      NodePointer identifier = demangleSubstitution();
      if (!identifier)
        return nullptr;
      identifier->setKind(Node::Kind::Module);
      return identifier;
    }
    return nullptr;
  }

  bool demangleDeclarationName(NodePointer &context, NodePointer &identifier,
                               IsProtocol isA) {
    Node::Kind context_type = Node::Kind::Unknown;
    context = demangleContext(&context_type);
    if (!context)
      return false;
    identifier = demangleIdentifier();
    if (!identifier)
      return false;
    DemanglerPrinter printer;
    printer << context->getText() << "." << identifier->getText();
    Substitutions.push_back({ printer.str(), isA });
    if (context_type != Node::Kind::Unknown)
      identifier->setKind(context_type);
    return true;
  }

  NodePointer demangleProtocolName() {
    if (Mangled.nextIf('S')) {
      Substitution sub = demangleSubstitutionIndexWithProtocol();
      if (sub.first.empty())
        return nullptr;
      if (sub.second == IsProtocol::yes)
        return Node::makeNodePointer(Node::Kind::Protocol, sub.first);
      NodePointer identifier = demangleIdentifier();
      if (!identifier)
        return nullptr;
      DemanglerPrinter printer;
      printer << sub.first << "." << identifier->getText();
      NodePointer result =
          Node::makeNodePointer(Node::Kind::Protocol, printer.str());
      Substitutions.push_back({ result->getText(), IsProtocol::yes });
      return result;
    }
    NodePointer context, identifier;
    if (demangleDeclarationName(context, identifier, IsProtocol::yes)) {
      DemanglerPrinter printer;
      while (context) {
        printer << context->getText() << ".";
        context = context->getNextNode();
      }
      printer << identifier->getText();
      NodePointer result =
          Node::makeNodePointer(Node::Kind::Protocol, printer.str());
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
    NodePointer context, identifier;
    if (!demangleDeclarationName(context, identifier, IsProtocol::no))
      return nullptr;
    identifier->setKind(nominalTypeMarkerToNodeKind(c));
    context->setNextNode(identifier);
    return context;
  }

  NodePointer demangleContext(Node::Kind *inferred_context_type = nullptr) {
    if (!Mangled)
      return nullptr;
    char c = Mangled.peek();
    if (isStartOfIdentifier(c) || c == 'S') {
      return demangleModule();
    }
    if (isStartOfNominalType(c)) {
      if (inferred_context_type) {
        *inferred_context_type = nominalTypeMarkerToNodeKind(c);
      }
      return demangleNominalType();
    }
    if (c == 'P') {
      Mangled.next();
      return demangleProtocolName();
    }
    return nullptr;
  }

  NodePointer demangleDeclWithContext(NodePointer context) {
    NodePointer identifier = demangleIdentifier();
    if (!identifier)
      return nullptr;
    return demangleDeclTypeWithContextAndName(context, identifier);
  }

  NodePointer demangleDeclTypeWithContextAndName(NodePointer context,
                                                 NodePointer identifier) {
    NodePointer type = demangleType();
    if (!type)
      return nullptr;
    NodePointer decl =
        Node::makeNodePointer(swift::Demangle::Node::Kind::Declaration);
    Node::Kind id_kind = identifier->getKind();
    if (id_kind != Node::Kind::InfixOperator &&
        id_kind != Node::Kind::PrefixOperator &&
        id_kind != Node::Kind::PostfixOperator)
      identifier->setKind(Node::Kind::DeclIdentifier);
    decl->push_back_child(context);
    decl->push_back_child(identifier);
    decl->push_back_child(type);
    return decl;
  }

  NodePointer demangleProtocolList() {
    NodePointer proto_list = Node::makeNodePointer(Node::Kind::ProtocolList);
    if (Mangled.nextIf('_')) {
      return proto_list;
    }
    NodePointer proto = demangleProtocolName();
    if (!proto)
      return nullptr;
    proto_list->push_back_child(proto);
    while (Mangled.nextIf('_') == false) {
      proto = demangleProtocolName();
      if (!proto)
        return nullptr;
      proto_list->push_back_child(proto);
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

  NodePointer demangleEntity() {
    NodePointer context = demangleContext();
    if (!context)
      return nullptr;
    NodePointer identifier = context->getNextNode();
    NodePointer decl;
    if (Mangled.nextIf('D')) {
      if (identifier && identifier->getKind() == Node::Kind::Class)
        decl = Node::makeNodePointer(Node::Kind::Deallocator);
      else
        decl = Node::makeNodePointer(Node::Kind::Destructor);
      decl->push_back_child(context);
      return decl;
    }
    if (Mangled.nextIf('d')) {
      decl = Node::makeNodePointer(Node::Kind::Destructor);
      decl->push_back_child(context);
      return decl;
    }
    if (Mangled.nextIf('C')) {
      NodePointer type = demangleType();
      if (!type)
        return nullptr;
      if (identifier && identifier->getKind() == Node::Kind::Class)
        decl = Node::makeNodePointer(Node::Kind::Allocator);
      else
        decl = Node::makeNodePointer(Node::Kind::Constructor);
      decl->push_back_child(context);
      decl->push_back_child(type);
      return decl;
    }
    if (Mangled.nextIf('c')) {
      NodePointer type = demangleType();
      if (!type)
        return nullptr;
      decl = Node::makeNodePointer(Node::Kind::Constructor);
      decl->push_back_child(context);
      decl->push_back_child(type);
      return decl;
    }
    NodePointer decl_with_ctx = demangleDeclWithContext(context);
    if (!decl_with_ctx)
      return nullptr;
    if (Mangled.nextIf('a')) {
      decl = Node::makeNodePointer(Node::Kind::Addressor);
      decl->push_back_child(decl_with_ctx);
    } else if (Mangled.nextIf('g')) {
      decl = Node::makeNodePointer(Node::Kind::Getter);
      decl->push_back_child(decl_with_ctx);
    } else if (Mangled.nextIf('s')) {
      decl = Node::makeNodePointer(Node::Kind::Setter);
      decl->push_back_child(decl_with_ctx);
    } else {
      decl = decl_with_ctx;
    }
    return decl;
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

  NodePointer demangleType() {
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
      return nullptr;
    }
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
      Node::Kind out_args_kind = out_args->getKind();
      if (out_args_kind == Node::Kind::Identifier ||
          out_args_kind == Node::Kind::Class ||
          out_args_kind == Node::Kind::Module ||
          out_args_kind == Node::Kind::Structure ||
          out_args_kind == Node::Kind::Union) {
        NodePointer out_node = compactNode(out_args, Node::Kind::ReturnType);
        block->push_back_child(out_node);
      } else {
        NodePointer out_node = Node::makeNodePointer(Node::Kind::ReturnType);
        out_node->push_back_child(out_args);
        block->push_back_child(out_node);
      }
      return block;
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
      Node::Kind out_args_kind = out_args->getKind();
      if (out_args_kind == Node::Kind::Identifier ||
          out_args_kind == Node::Kind::Class ||
          out_args_kind == Node::Kind::Module ||
          out_args_kind == Node::Kind::Structure ||
          out_args_kind == Node::Kind::Union) {
        NodePointer out_node = compactNode(out_args, Node::Kind::ReturnType);
        block->push_back_child(out_node);
      } else {
        NodePointer out_node = Node::makeNodePointer(Node::Kind::ReturnType);
        out_node->push_back_child(out_args);
        block->push_back_child(out_node);
      }
      return block;
    }
    if (c == 'f') {
      NodePointer in_args = demangleType();
      if (!in_args)
        return nullptr;
      NodePointer out_args = demangleType();
      if (!out_args)
        return nullptr;
      NodePointer block =
          Node::makeNodePointer(Node::Kind::UncurriedFunctionType);
      NodePointer in_node =
          Node::makeNodePointer(Node::Kind::UncurriedFunctionMetaType);
      block->push_back_child(in_node);
      in_node->push_back_child(in_args);
      Node::Kind out_args_kind = out_args->getKind();
      if (out_args_kind == Node::Kind::Identifier ||
          out_args_kind == Node::Kind::Class ||
          out_args_kind == Node::Kind::Module ||
          out_args_kind == Node::Kind::Structure ||
          out_args_kind == Node::Kind::Union) {
        NodePointer out_node = compactNode(out_args, Node::Kind::ReturnType);
        block->push_back_child(out_node);
      } else {
        NodePointer out_node = Node::makeNodePointer(Node::Kind::ReturnType);
        out_node->push_back_child(out_args);
        block->push_back_child(out_node);
      }
      return block;
    }
    if (c == 'G') {
      NodePointer type_list = Node::makeNodePointer(Node::Kind::TypeList);
      NodePointer type = demangleType();
      if (!type)
        return nullptr;
      NodePointer type_list_entry =
          Node::makeNodePointer(Node::Kind::TypeListEntry);
      type_list_entry->push_back_child(type);
      type_list->push_back_child(type_list_entry);
      while (Mangled.peek() != '_') {
        type = demangleType();
        if (!type)
          return nullptr;
        type_list_entry = Node::makeNodePointer(Node::Kind::TypeListEntry);
        type_list_entry->push_back_child(type);
        type_list->push_back_child(type_list_entry);
      }
      Mangled.next();
      NodePointer type_application =
          Node::makeNodePointer(Node::Kind::GenericTypeApplication);
      type_application->push_back_child(type_list);
      return type_application;
    }
    if (c == 'M') {
      NodePointer type = demangleType();
      if (!type)
        return nullptr;
      NodePointer metatype = Node::makeNodePointer(Node::Kind::MetaType);
      NodePointer type_entry = Node::makeNodePointer(Node::Kind::TypeListEntry);
      type_entry->push_back_child(type);
      metatype->push_back_child(type_entry);
      return metatype;
    }
    if (c == 'P') {
      return demangleProtocolList();
    }
    if (c == 'Q') {
      if (Mangled.nextIf('d')) {
        size_t depth, index;
        if (!demangleIndex(depth))
          return nullptr;
        if (!demangleIndex(index))
          return nullptr;
        return demangleArchetypeRef(depth + 1, index);
      }
      size_t index;
      if (!demangleIndex(index))
        return nullptr;
      return demangleArchetypeRef(0, index);
    }
    if (c == 'R') {
      NodePointer byref = Node::makeNodePointer(Node::Kind::ByRef);
      byref->setNextNode(demangleType());
      if (byref->getNextNode())
        return byref;
      return nullptr;
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
      if (c == 'o') {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer unowned = Node::makeNodePointer(Node::Kind::Unowned);
        unowned->setNextNode(type);
        return unowned;
      }
      if (c == 'w') {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer unowned = Node::makeNodePointer(Node::Kind::Weak);
        unowned->setNextNode(type);
        return unowned;
      }
      return nullptr;
    }
    if (isStartOfNominalType(c)) {
      NodePointer context, identifier;
      if (!demangleDeclarationName(context, identifier, IsProtocol::no))
        return nullptr;
      context->setNextNode(identifier);
      identifier->setKind(nominalTypeMarkerToNodeKind(c));
      NodePointer nominal_type = Node::makeNodePointer(Node::Kind::NominalType);
      nominal_type->push_back_child(context);
      return nominal_type;
    }
    return nullptr;
  }

  class MangledNameSource {
  public:
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

NodePointer swift::Demangle::demangleSymbolAsNode(llvm::StringRef mangled) {
  Demangler demangler(mangled);
  demangler.demangle();
  return demangler.getDemangled();
}

void toString(NodePointer pointer, DemanglerPrinter &printer) {
  if (!pointer)
    return;
  Node::Kind kind = pointer->getKind();
  switch (kind) {
  case swift::Demangle::Node::Kind::Failure:
    return;
  case swift::Demangle::Node::Kind::Directness:
    printer << pointer->getText() << " ";
    break;
  case swift::Demangle::Node::Kind::Declaration: {
    Node::iterator begin = pointer->begin(), end = pointer->end();
    for (; begin != end; begin++) {
      toString(*begin, printer);
    }
    break;
  }
  case swift::Demangle::Node::Kind::Module:
  case swift::Demangle::Node::Kind::Class:
  case swift::Demangle::Node::Kind::Structure:
  case swift::Demangle::Node::Kind::Union:
    printer << pointer->getText() << ".";
    break;
  case swift::Demangle::Node::Kind::Identifier:
    printer << pointer->getText();
    break;
  case swift::Demangle::Node::Kind::DeclIdentifier:
    printer << pointer->getText() << " : ";
  case swift::Demangle::Node::Kind::FunctionName:
    break;
  case swift::Demangle::Node::Kind::FunctionType: {
    Node::iterator begin = pointer->begin(), end = pointer->end();
    for (; begin != end; begin++) {
      toString(*begin, printer);
    }
    break;
  }
  case swift::Demangle::Node::Kind::UncurriedFunctionType: {
    NodePointer metatype = pointer->child_at(0);
    if (!metatype)
      break;
    DemanglerPrinter sub_printer;
    toString(metatype, sub_printer);
    printer << sub_printer.str();
    NodePointer real_func = pointer->child_at(1);
    if (!real_func)
      break;
    real_func = real_func->child_at(0);
    Node::iterator begin = real_func->begin(), end = real_func->end();
    for (; begin != end; begin++) {
      toString(*begin, printer);
    }
    break;
  }
  case swift::Demangle::Node::Kind::UncurriedFunctionMetaType: {
    printer << "(";
    Node::iterator begin = pointer->begin(), end = pointer->end();
    for (; begin != end; begin++) {
      toString(*begin, printer);
    }
    printer << ")";
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
        Node::Kind child0_kind = pointer->child_at(0)->getKind();
        if (child0_kind != Node::Kind::VariadicTuple &&
            child0_kind != Node::Kind::NonVariadicTuple)
          need_parens = true;
      }
    }
    if (need_parens)
      printer << "(";
    Node::iterator begin = pointer->begin(), end = pointer->end();
    for (; begin != end; begin++) {
      toString(*begin, printer);
    }
    if (need_parens)
      printer << ")";
    break;
  }
  case swift::Demangle::Node::Kind::NonVariadicTuple:
  case swift::Demangle::Node::Kind::VariadicTuple: {
    printer << "(";
    Node::iterator begin = pointer->begin(), end = pointer->end();
    for (; begin != end;) {
      toString(*begin, printer);
      if (++begin != end)
        printer << ", ";
    }
    if (pointer->getKind() == swift::Demangle::Node::Kind::VariadicTuple)
      printer << "...";
    printer << ")";
    break;
  }
  case swift::Demangle::Node::Kind::TupleElement: {
    if (pointer->size() == 1) {
      NodePointer type = pointer->child_at(0);
      toString(type, printer);
    } else if (pointer->size() == 2) {
      NodePointer id = pointer->child_at(0);
      NodePointer type = pointer->child_at(1);
      toString(id, printer);
      toString(type, printer);
    }
    break;
  }
  case swift::Demangle::Node::Kind::TupleElementName:
    printer << pointer->getText() << " : ";
    break;
  case swift::Demangle::Node::Kind::TupleElementType:
    printer << pointer->getText();
    break;
  case swift::Demangle::Node::Kind::ReturnType: {
    if (pointer->size() == 0)
      printer << " -> " << pointer->getText();
    else {
      printer << " -> ";
      Node::iterator begin = pointer->begin(), end = pointer->end();
      for (; begin != end; begin++) {
        toString(*begin, printer);
      }
    }
    break;
  }
  case swift::Demangle::Node::Kind::Weak:
    printer << "[weak] ";
    break;
  case swift::Demangle::Node::Kind::Unowned:
    printer << "[unowned] ";
    break;
  case swift::Demangle::Node::Kind::ByRef:
    printer << "[byref] ";
    break;
  case swift::Demangle::Node::Kind::ObjCAttribute:
    printer << "[objc] ";
    break;
  case swift::Demangle::Node::Kind::LocalEntity:
    printer << "local ";
    break;
  case swift::Demangle::Node::Kind::BuiltinTypeName:
  case swift::Demangle::Node::Kind::BaseName:
  case swift::Demangle::Node::Kind::Number:
    printer << pointer->getText();
    break;
  case swift::Demangle::Node::Kind::ArrayType: {
    NodePointer type = pointer->child_at(0);
    NodePointer size = pointer->child_at(1);
    toString(type, printer);
    printer << "[";
    toString(size, printer);
    printer << "]";
    break;
  }
  case swift::Demangle::Node::Kind::InfixOperator:
    printer << pointer->getText() << " [infix] : ";
    break;
  case swift::Demangle::Node::Kind::PrefixOperator:
    printer << pointer->getText() << " [prefix] : ";
    break;
  case swift::Demangle::Node::Kind::PostfixOperator:
    printer << pointer->getText() << " [postfix] : ";
    break;
  case swift::Demangle::Node::Kind::DependentProtocolWitnessTableGenerator:
    printer << "dependent protocol witness table generator for ";
    break;
  case swift::Demangle::Node::Kind::DependentProtocolWitnessTableTemplate:
    printer << "dependent protocol witness table template for ";
    break;
  case swift::Demangle::Node::Kind::LazyProtocolWitnessTableAccessor:
    printer << "lazy protocol witness table accessor for ";
    break;
  case swift::Demangle::Node::Kind::LazyProtocolWitnessTableTemplate:
    printer << "lazy protocol witness table template for ";
    break;
  case swift::Demangle::Node::Kind::ProtocolWitnessTable:
    printer << "protocol witness table for ";
    break;
  case swift::Demangle::Node::Kind::ProtocolWitness:
    printer << "protocol witness for ";
    break;
  case swift::Demangle::Node::Kind::FieldOffset:
    printer << "field offset for ";
    break;
  case swift::Demangle::Node::Kind::BridgeToBlockFunction:
    printer << "bridge-to-block function for ";
    break;
  case swift::Demangle::Node::Kind::GenericTypeMetadataPattern:
    printer << "generic type metadata pattern for ";
    break;
  case swift::Demangle::Node::Kind::Metaclass:
    printer << "metaclass for ";
    break;
  case swift::Demangle::Node::Kind::TypeMetadata:
    printer << "type metadata for ";
    break;
  case swift::Demangle::Node::Kind::ValueWitnessKind:
    printer << pointer->getText() << " value witness for ";
    break;
  case swift::Demangle::Node::Kind::ValueWitnessTable:
    printer << "value witness table for ";
    break;
  case swift::Demangle::Node::Kind::WitnessTableOffset:
    printer << "witness table offset for ";
    break;
  case swift::Demangle::Node::Kind::GenericTypeApplication: {
    NodePointer typelist = pointer->child_at(0);
    if (!typelist)
      break;
    NodePointer type0 = typelist->child_at(0);
    toString(type0, printer);
    printer << "<";
    Node::size_type count = typelist->size();
    for (Node::size_type i = 1; i < count;) {
      toString(typelist->child_at(i), printer);
      if (++i < count)
        printer << ", ";
    }
    printer << ">";
    break;
  }
  case swift::Demangle::Node::Kind::NominalType: {
    NodePointer first = pointer->child_at(0);
    while (first) {
      printer << first->getText();
      first = first->getNextNode();
      if (first)
        printer << ".";
    }
    break;
  }
  case swift::Demangle::Node::Kind::TypeListEntry: {
    Node::iterator begin = pointer->begin(), end = pointer->end();
    while (begin != end) {
      toString(*begin, printer);
      ++begin;
    }
    break;
  }
  case swift::Demangle::Node::Kind::ObjCBlock: {
    printer << "[objc_block] ";
    NodePointer tuple = pointer->child_at(0);
    NodePointer rettype = pointer->child_at(1);
    toString(tuple, printer);
    toString(rettype, printer);
    break;
  }
  case swift::Demangle::Node::Kind::MetaType: {
    NodePointer type = pointer->child_at(0);
    toString(type, printer);
    printer << ".metatype";
    break;
  }
  case swift::Demangle::Node::Kind::Protocol:
    printer << pointer->getText();
    break;
  case swift::Demangle::Node::Kind::ProtocolList: {
    if (pointer->size() == 1) {
      toString(pointer->child_at(0), printer);
      break;
    }
    Node::iterator begin = pointer->begin(), end = pointer->end();
    printer << "protocol<";
    while (begin != end) {
      toString(*begin, printer);
      if (++begin != end)
        printer << ", ";
    }
    printer << ">";
    break;
  }
  case swift::Demangle::Node::Kind::ArchetypeRef:
    printer << pointer->getText();
    break;
  case swift::Demangle::Node::Kind::ArchetypeList: {
    if (pointer->size() == 0)
      break;
    Node::iterator begin = pointer->begin(), end = pointer->end();
    printer << "<";
    while (begin != end) {
      toString(*begin, printer);
      if (++begin != end)
        printer << ", ";
    }
    printer << ">";
    break;
  }
  case swift::Demangle::Node::Kind::GenericType: {
    NodePointer atype_list = pointer->child_at(0);
    NodePointer fct_type = pointer->child_at(1);
    toString(atype_list, printer);
    NodePointer args = fct_type->child_at(0);
    NodePointer ret = fct_type->child_at(1);
    toString(args, printer);
    toString(ret, printer);
    break;
  }
  case swift::Demangle::Node::Kind::Addressor: {
    NodePointer decl = pointer->child_at(0);
    toString(decl, printer);
    printer << " addressor";
    break;
  }
  case swift::Demangle::Node::Kind::Getter: {
    NodePointer decl = pointer->child_at(0);
    toString(decl, printer);
    printer << " getter";
    break;
  }
  case swift::Demangle::Node::Kind::Setter: {
    NodePointer decl = pointer->child_at(0);
    toString(decl, printer);
    printer << " setter";
    break;
  }
  case swift::Demangle::Node::Kind::Allocator: {
    NodePointer child = pointer->child_at(0);
    while (child) {
      printer << child->getText();
      child = child->getNextNode();
      if (child)
        printer << ".";
    }
    printer << ".__allocating_constructor";
    child = pointer->child_at(1);
    if (child) {
      printer << " : ";
      toString(child, printer);
    }
    break;
  }
  case swift::Demangle::Node::Kind::Constructor: {
    NodePointer child = pointer->child_at(0);
    while (child) {
      printer << child->getText();
      child = child->getNextNode();
      if (child)
        printer << ".";
    }
    printer << ".constructor";
    child = pointer->child_at(1);
    if (child) {
      printer << " : ";
      toString(child, printer);
    }
    break;
  }
  case swift::Demangle::Node::Kind::Destructor: {
    NodePointer child = pointer->child_at(0);
    while (child) {
      printer << child->getText();
      child = child->getNextNode();
      if (child)
        printer << ".";
    }
    printer << ".destructor";
    child = pointer->child_at(1);
    if (child) {
      printer << " : ";
      toString(child, printer);
    }
    break;
  }
  case swift::Demangle::Node::Kind::Deallocator: {
    NodePointer child = pointer->child_at(0);
    while (child) {
      printer << child->getText();
      child = child->getNextNode();
      if (child)
        printer << ".";
    }
    printer << ".__deallocating_destructor";
    child = pointer->child_at(1);
    if (child) {
      printer << " : ";
      toString(child, printer);
    }
    break;
  }
  case swift::Demangle::Node::Kind::ProtocolConformance: {
    NodePointer child0 = pointer->child_at(0);
    NodePointer child1 = pointer->child_at(1);
    NodePointer child2 = pointer->child_at(2);
    if (!child0 || !child1 || !child2)
      break;
    toString(child0, printer);
    printer << " : ";
    toString(child1, printer);
    printer << " in " << child2->getText();
    break;
  }
  case swift::Demangle::Node::Kind::Substitution:
  case swift::Demangle::Node::Kind::TypeName:
  case swift::Demangle::Node::Kind::UncurriedFunctionFunctionType:
  case swift::Demangle::Node::Kind::TypeList:
  case swift::Demangle::Node::Kind::ArchetypeAndProtocol: {
    NodePointer child0 = pointer->child_at(0);
    NodePointer child1 = pointer->child_at(1);
    toString(child0, printer);
    printer << " : ";
    toString(child1, printer);
  }
  case swift::Demangle::Node::Kind::Unknown:
    break;
  }
  pointer = pointer->getNextNode();
  toString(pointer, printer);
}

std::string swift::Demangle::nodeToString(NodePointer pointer) {
  if (!pointer)
    return "";
  DemanglerPrinter printer;
  toString(pointer, printer);
  return printer.str();
}

std::string swift::Demangle::demangleSymbolAsString(llvm::StringRef mangled) {
  std::string demangling = nodeToString(demangleSymbolAsNode(mangled));
  if (demangling.empty())
    return mangled.str();
  return demangling;
}
