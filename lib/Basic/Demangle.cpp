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
#include "swift/Basic/Optional.h"
#include "swift/Basic/PrettyStackTrace.h"
#include "swift/Basic/QuotedString.h"
#include "llvm/Support/raw_ostream.h"
#include <functional>
#include <tuple>
#include <vector>

using namespace swift;
using namespace Demangle;

static StringRef getNodeKindString(swift::Demangle::Node::Kind k) {
  switch(k) {
#define NODE(ID) case Node::Kind::ID: return #ID;
#include "swift/Basic/DemangleNodes.def"
  }
  llvm_unreachable("bad node kind");
}


static void printNode(llvm::raw_ostream &out, const Node *node,
                      unsigned depth) {
  // Indent two spaces per depth.
  for (unsigned i = 0; i < depth; i++) {
    out << "  ";
  }
  out << "kind=" << getNodeKindString(node->getKind());
  if (node->hasText()) {
    out << ", text=\"" << node->getText() << '\"';
  }
  if (node->hasIndex()) {
    out << ", index=" << node->getIndex();
  }
  out << '\n';
  for (auto &child : *node) {
    printNode(out, child.getPtr(), depth + 1);
  }
}

void Node::dump() const {
  print(llvm::errs());
}

void Node::print(llvm::raw_ostream &out) const {
  printNode(out, this, 0);
}

Node::~Node() {
  switch (NodePayloadKind) {
  case PayloadKind::None: return;
  case PayloadKind::Index: return;
  case PayloadKind::Text: TextPayload.~basic_string(); return;
  }
  llvm_unreachable("bad payload kind");
}

namespace {
  struct FindPtr {
    FindPtr(Node *v) : Target(v) {}
    bool operator()(NodePointer sp) const {
      return sp.getPtr() == Target;
    }
  private:
    Node *Target;
  };

/// A class for printing to a std::string.
class DemanglerPrinter {
public:
  DemanglerPrinter() : Stream(Buffer) {}

  template <class T>
  DemanglerPrinter &operator<<(T &&value) {
    Stream << std::forward<T>(value);
    return *this;
  }

  /// Destructively take the contents of this stream.
  std::string str() { return std::move(Stream.str()); }

private:
  std::string Buffer;
  llvm::raw_string_ostream Stream;
};
} // end anonymous namespace

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

static bool isStartOfEntity(char c) {
  switch (c) {
  case 'F':
  case 'I':
  case 'v':
  case 'P':
    return true;
  default:
    return isStartOfNominalType(c);
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

static std::string archetypeName(Node::IndexType i) {
  DemanglerPrinter name;
  do {
    name << (char)('A' + (i % 26));
    i /= 26;
  } while (i);
  return name.str();
}

namespace {

/// A convenient class for parsing characters out of a string.
class NameSource {
  StringRef Text;
public:
  NameSource(StringRef text) : Text(text) {}

  /// Return whether there are at least len characters remaining.
  bool hasAtLeast(size_t len) { return (len <= Text.size()); }

  bool isEmpty() { return Text.empty(); }
  explicit operator bool() { return !isEmpty(); }

  /// Return the next character without claiming it.  Asserts that
  /// there is at least one remaining character.
  char peek() { return Text.front(); }

  /// Claim and return the next character.  Asserts that there is at
  /// least one remaining character.
  char next() {
    char c = peek();
    advanceOffset(1);
    return c;
  }

  /// Claim the next character if it exists and equals the given
  /// character.
  bool nextIf(char c) {
    if (isEmpty() || peek() != c) return false;
    advanceOffset(1);
    return true;
  }

  /// Return the next len characters without claiming them.  Asserts
  /// that there are at least so many characters.
  StringRef slice(size_t len) { return Text.substr(0, len); }

  /// Claim the next len characters.
  void advanceOffset(size_t len) {
    Text = Text.substr(len);
  }

  /// Claim and return all the rest of the characters.
  StringRef getString() {
    auto result = Text;
    advanceOffset(Text.size());
    return result;
  }
};

/// The main class for parsing a demangling tree out of a mangled string.
class Demangler {
  SmallVector<NodePointer, 10> Substitutions;
  SmallVector<unsigned, 4> ArchetypeCounts;
  unsigned ArchetypeCount = 0;
  NameSource Mangled;
  NodePointer RootNode;
public:  
  Demangler(llvm::StringRef mangled) : Mangled(mangled) {}

  /// Attempt to demangle the source string.  The root node will
  /// always be a Global.  Extra characters at the end will be
  /// tolerated (and included as a Suffix node as a child of the
  /// Global).
  ///
  /// \return true if the mangling succeeded
  bool demangle() {
    if (!Mangled.hasAtLeast(2))
      return failure();
    if (Mangled.slice(2) != "_T")
      return failure();
    if (Mangled.hasAtLeast(4) && Mangled.slice(4) == "_TTo") {
      Mangled.advanceOffset(4);
      appendNode(Node::Kind::ObjCAttribute);
    } else {
      Mangled.advanceOffset(2);
    }
    if (!demangleGlobal())
      return false;

    // Add a suffix node if there's anything left unmangled.
    if (!Mangled.isEmpty()) {
      appendNode(Node::Kind::Suffix, Mangled.getString());
    }

    return true;
  }

  NodePointer getDemangled() { return RootNode; }
  
private:
  Node *getRootNode() {
    if (!RootNode) {
      RootNode = Node::create(Node::Kind::Global);
    }
    return RootNode.getPtr();
  }

  Node *appendNode(NodePointer n) {
    return getRootNode()->addChild(std::move(n));
  }

  Node *appendNode(Node::Kind k, std::string &&t = "") {
    return appendNode(Node::create(k, std::move(t)));
  }

  enum class IsProtocol {
    yes = true, no = false
  };

  enum class IsVariadic {
    yes = true, no = false
  };

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
    RootNode = Node::create(Node::Kind::Failure);
    return false;
  }

  Directness demangleDirectness() {
    if (Mangled.nextIf('d'))
      return Directness::Direct;
    if (Mangled.nextIf('i'))
      return Directness::Indirect;
    return Directness::Unkown;
  }

  bool demangleNatural(Node::IndexType &num) {
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

  bool demangleBuiltinSize(Node::IndexType &num) {
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
        appendNode(Node::Kind::GenericTypeMetadataPattern)->addChild(type);
        return true;
      }
      if (Mangled.nextIf('m')) {
        NodePointer type = demangleType();
        if (!type)
          return failure();
        appendNode(Node::Kind::Metaclass)->addChild(type);
        return true;
      }
    if (Mangled.nextIf('n')) {
        NodePointer type = demangleType();
        if (!type)
            return failure();
        appendNode(Node::Kind::NominalTypeDescriptor)->addChild(type);
        return true;
    }
      Directness d = demangleDirectness();
      appendNode(Node::Kind::Directness, toString(d));
      NodePointer type = demangleType();
      if (!type)
        return failure();
      appendNode(Node::Kind::TypeMetadata)->addChild(type);
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
      appendNode(Node::Kind::ValueWitnessKind, toString(w))->addChild(type);
      return true;
    }
    if (Mangled.nextIf('W')) {
      if (Mangled.nextIf('V')) {
        NodePointer type = demangleType();
        if (!type)
          return failure();
        appendNode(Node::Kind::ValueWitnessTable)->addChild(type);
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
        appendNode(Node::Kind::ProtocolWitnessTable)->addChild(conformance);
        return true;
      }
      if (Mangled.nextIf('Z')) {
        NodePointer conformance = demangleProtocolConformance();
        if (!conformance)
          return failure();
        appendNode(Node::Kind::LazyProtocolWitnessTableAccessor)->addChild(conformance);
        return true;
      }
      if (Mangled.nextIf('z')) {
        NodePointer conformance = demangleProtocolConformance();
        if (!conformance)
          return failure();
        appendNode(Node::Kind::LazyProtocolWitnessTableTemplate)->addChild(conformance);
        return true;
      }
      if (Mangled.nextIf('D')) {
        NodePointer conformance = demangleProtocolConformance();
        if (!conformance)
          return failure();
        appendNode(Node::Kind::DependentProtocolWitnessTableGenerator)->addChild(conformance);
        return true;
      }
      if (Mangled.nextIf('d')) {
        NodePointer conformance = demangleProtocolConformance();
        if (!conformance)
          return failure();
        appendNode(Node::Kind::DependentProtocolWitnessTableTemplate)->addChild(conformance);
        return true;
      }
      return failure();
    }
    if (Mangled.nextIf('T')) {
      if (Mangled.nextIf('b')) {
        NodePointer type = demangleType();
        if (!type)
          return failure();
        appendNode(Node::Kind::BridgeToBlockFunction)->addChild(type);
        return true;
      }
      if (Mangled.nextIf('R')) {
        NodePointer thunk = appendNode(Node::Kind::ReabstractionThunkHelper);
        return demangleReabstractSignature(thunk);
      }
      if (Mangled.nextIf('r')) {
        NodePointer thunk = appendNode(Node::Kind::ReabstractionThunk);
        return demangleReabstractSignature(thunk);
      }
      return failure();
    }
    if (!demangleEntity(getRootNode()))
      return failure();
    return true;
  }

  std::string demangleOperator() {
    static const char op_char_table[] = "& @/= >    <*!|+ %-~   ^ .";
    Node::IndexType length;
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

  NodePointer demangleDeclName() {
    // decl-name ::= local-decl-name
    // local-decl-name ::= 'L' index identifier
    if (Mangled.nextIf('L')) {
      NodePointer discriminator = demangleIndexAsNode();
      if (!discriminator) return nullptr;

      NodePointer name = demangleIdentifier();
      if (!name) return nullptr;

      NodePointer localName = Node::create(Node::Kind::LocalDeclName);
      localName->addChild(std::move(discriminator));
      localName->addChild(std::move(name));
      return localName;
    }

    // decl-name ::= identifier
    return demangleIdentifier();
  }

  NodePointer demangleIdentifier(Node::Kind kind = Node::Kind::Unknown) {
    if (!Mangled)
      return nullptr;
    if (Mangled.nextIf('o')) {
      // Operator identifiers aren't valid in the contexts that are
      // building more specific identifiers.
      if (kind != Node::Kind::Unknown) return nullptr;

      char op_mode = Mangled.next();
      if (op_mode != 'p' && op_mode != 'P' && op_mode != 'i')
        return nullptr;
      std::string operatr = demangleOperator();
      if (operatr.size()) {
        switch (op_mode) {
        case 'p':
          return Node::create(Node::Kind::PrefixOperator, operatr);
        case 'P':
          return Node::create(Node::Kind::PostfixOperator, operatr);
        case 'i':
          return Node::create(Node::Kind::InfixOperator, operatr);
        default:
          return nullptr;
        }
      }
    }

    if (kind == Node::Kind::Unknown) kind = Node::Kind::Identifier;

    Node::IndexType length;
    if (demangleNatural(length)) {
      if (Mangled.hasAtLeast(length)) {
        auto identifier = Mangled.slice(length);
        Mangled.advanceOffset(length);
        return Node::create(kind, identifier);
      }
    }
    return nullptr;
  }

  bool demangleIndex(Node::IndexType &natural) {
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

  /// Demangle an <index> and package it as a node of some kind.
  NodePointer demangleIndexAsNode(Node::Kind kind = Node::Kind::Number) {
    Node::IndexType index;
    if (!demangleIndex(index))
      return nullptr;
    return Node::create(kind, index);
  }

  NodePointer createSwiftType(Node::Kind typeKind, StringRef name) {
    NodePointer type = Node::create(typeKind);
    type->addChild(Node::create(Node::Kind::Module, "swift"));
    type->addChild(Node::create(Node::Kind::Identifier, name));
    return type;
  }

  /// Demangle a <substitution>, given that we've already consumed the 'S'.
  NodePointer demangleSubstitutionIndex() {
    if (!Mangled)
      return Node::create(Node::Kind::Failure);
    if (Mangled.nextIf('o'))
      return Node::create(Node::Kind::Module, "ObjectiveC");
    if (Mangled.nextIf('C'))
      return Node::create(Node::Kind::Module, "C");
    if (Mangled.nextIf('s'))
      return Node::create(Node::Kind::Module, "swift");
    if (Mangled.nextIf('a'))
      return createSwiftType(Node::Kind::Structure, "Array");
    if (Mangled.nextIf('b'))
      return createSwiftType(Node::Kind::Structure, "Bool");
    if (Mangled.nextIf('c'))
      return createSwiftType(Node::Kind::Structure, "UnicodeScalar");
    if (Mangled.nextIf('d'))
      return createSwiftType(Node::Kind::Structure, "Float64");
    if (Mangled.nextIf('f'))
      return createSwiftType(Node::Kind::Structure, "Float32");
    if (Mangled.nextIf('i'))
      return createSwiftType(Node::Kind::Structure, "Int64");
    if (Mangled.nextIf('q'))
      return createSwiftType(Node::Kind::Enum, "Optional");
    if (Mangled.nextIf('S'))
      return createSwiftType(Node::Kind::Structure, "String");
    if (Mangled.nextIf('u'))
      return createSwiftType(Node::Kind::Structure, "UInt64");
    Node::IndexType index_sub;
    if (!demangleIndex(index_sub))
      return Node::create(Node::Kind::Failure);
    if (index_sub >= Substitutions.size())
      return Node::create(Node::Kind::Failure);
    return Substitutions[index_sub];
  }

  NodePointer demangleModule() {
    if (Mangled.nextIf('S')) {
      NodePointer module = demangleSubstitutionIndex();
      if (!module)
        return nullptr;
      if (module->getKind() != Node::Kind::Module)
        return nullptr;
      return module;
    }

    NodePointer module = demangleIdentifier(Node::Kind::Module);
    if (!module) return nullptr;
    Substitutions.push_back(module);
    return module;
  }

  NodePointer demangleDeclarationName(Node::Kind kind) {
    NodePointer context = demangleContext();
    if (!context) return nullptr;

    auto name = demangleDeclName();
    if (!name) return nullptr;

    auto decl = Node::create(kind);
    decl->addChild(context);
    decl->addChild(name);
    Substitutions.push_back(decl);
    return decl;
  }

  NodePointer demangleProtocolName() {
    NodePointer proto = demangleProtocolNameImpl();
    if (!proto) return nullptr;

    NodePointer type = Node::create(Node::Kind::Type);
    type->addChild(proto);
    return type;
  }
  
  NodePointer demangleProtocolNameImpl() {
    // There's an ambiguity in <protocol> between a substitution of
    // the protocol and a substitution of the protocol's context, so
    // we have to duplicate some of the logic from
    // demangleDeclarationName.
    if (Mangled.nextIf('S')) {
      NodePointer sub = demangleSubstitutionIndex();
      if (!sub) return nullptr;
      if (sub->getKind() == Node::Kind::Protocol)
        return sub;

      if (sub->getKind() != Node::Kind::Module)
        return nullptr;

      NodePointer name = demangleDeclName();
      if (!name) return nullptr;

      auto proto = Node::create(Node::Kind::Protocol);
      proto->addChild(std::move(sub));
      proto->addChild(std::move(name));
      Substitutions.push_back(proto);
      return proto;
    }

    return demangleDeclarationName(Node::Kind::Protocol);
  }

  NodePointer demangleNominalType() {
    if (Mangled.nextIf('S'))
      return demangleSubstitutionIndex();
    if (Mangled.nextIf('V'))
      return demangleDeclarationName(Node::Kind::Structure);
    if (Mangled.nextIf('O'))
      return demangleDeclarationName(Node::Kind::Enum);
    if (Mangled.nextIf('C'))
      return demangleDeclarationName(Node::Kind::Class);
    if (Mangled.nextIf('P'))
      return demangleDeclarationName(Node::Kind::Protocol);
    return nullptr;
  }

  NodePointer demangleContext() {
    // context ::= module
    // context ::= entity
    if (!Mangled) return nullptr;
    if (Mangled.nextIf('S'))
      return demangleSubstitutionIndex();
    if (isStartOfEntity(Mangled.peek()))
      return demangleEntity();
    return demangleModule();
  }
  
  NodePointer demangleProtocolList() {
    NodePointer proto_list = Node::create(Node::Kind::ProtocolList);
    NodePointer type_list = Node::create(Node::Kind::TypeList);
    proto_list->addChild(type_list);
    if (Mangled.nextIf('_')) {
      return proto_list;
    }
    NodePointer proto = demangleProtocolName();
    if (!proto)
      return nullptr;
    type_list->addChild(proto);
    while (Mangled.nextIf('_') == false) {
      proto = demangleProtocolName();
      if (!proto)
        return nullptr;
      type_list->addChild(proto);
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
        Node::create(Node::Kind::ProtocolConformance);
    proto_conformance->addChild(type);
    proto_conformance->addChild(protocol);
    proto_conformance->addChild(context);
    return proto_conformance;
  }

  /// Demangle an <entity> and add it to the given node.
  bool demangleEntity(NodePointer parent) {
    NodePointer entity = demangleEntity();
    if (!entity) return failure();
    parent->addChild(entity);
    return true;
  }

  // entity ::= entity-kind context entity-name  
  // entity ::= nominal-type
  NodePointer demangleEntity() {
    // entity-kind
    Node::Kind entityBasicKind;
    if (Mangled.nextIf('F')) {
      entityBasicKind = Node::Kind::Function;
    } else if (Mangled.nextIf('v')) {
      entityBasicKind = Node::Kind::Variable;
    } else if (Mangled.nextIf('I')) {
      entityBasicKind = Node::Kind::Initializer;
    } else {
      return demangleNominalType();
    }

    NodePointer context = demangleContext();
    if (!context) return nullptr;

    // entity-name
    Node::Kind entityKind;
    bool hasType = true;
    NodePointer name;
    if (Mangled.nextIf('D')) {
      if (context->getKind() == Node::Kind::Class)
        entityKind = Node::Kind::Deallocator;
      else
        entityKind = Node::Kind::Destructor;
      hasType = false;
    } else if (Mangled.nextIf('d')) {
      entityKind = Node::Kind::Destructor;
      hasType = false;
    } else if (Mangled.nextIf('C')) {
      if (context->getKind() == Node::Kind::Class)
        entityKind = Node::Kind::Allocator;
      else
        entityKind = Node::Kind::Constructor;
    } else if (Mangled.nextIf('c')) {
      entityKind = Node::Kind::Constructor;
    } else if (Mangled.nextIf('a')) {
      entityKind = Node::Kind::Addressor;
      name = demangleDeclName();
      if (!name) return nullptr;
    } else if (Mangled.nextIf('g')) {
      entityKind = Node::Kind::Getter;
      name = demangleDeclName();
      if (!name) return nullptr;
    } else if (Mangled.nextIf('s')) {
      entityKind = Node::Kind::Setter;
      name = demangleDeclName();
      if (!name) return nullptr;
    } else if (Mangled.nextIf('U')) {
      entityKind = Node::Kind::ExplicitClosure;
      name = demangleIndexAsNode();
      if (!name) return nullptr;
    } else if (Mangled.nextIf('u')) {
      entityKind = Node::Kind::ImplicitClosure;
      name = demangleIndexAsNode();
      if (!name) return nullptr;
    } else if (entityBasicKind == Node::Kind::Initializer) {
      // entity-name ::= 'A' index
      if (Mangled.nextIf('A')) {
        entityKind = Node::Kind::DefaultArgumentInitializer;
        name = demangleIndexAsNode();
        if (!name) return nullptr;
      // entity-name ::= 'i'
      } else if (Mangled.nextIf('i')) {
        entityKind = Node::Kind::Initializer;
      } else {
        return nullptr;
      }
      hasType = false;
    } else {
      entityKind = entityBasicKind;
      name = demangleDeclName();
      if (!name) return nullptr;
    }

    NodePointer entity = Node::create(entityKind);
    entity->addChild(context);

    if (name) entity->addChild(name);

    if (hasType) {
      auto type = demangleType();
      if (!type) return nullptr;
      entity->addChild(type);
    }

    return entity;
  }

  /// A RAII object designed for parsing generic signatures.
  class GenericContext {
    Demangler &D;
  public:
    GenericContext(Demangler &D) : D(D) {
      D.ArchetypeCounts.push_back(D.ArchetypeCount);
    }
    ~GenericContext() {
      D.ArchetypeCount = D.ArchetypeCounts.pop_back_val();
    }
  };

  /// Demangle a generic clause.
  ///
  /// \param C - not really required; just a token to prove that the caller
  ///   has thought to enter a generic context
  NodePointer demangleGenerics(GenericContext &C) {
    DemanglerPrinter result_printer;
    NodePointer archetypes = Node::create(Node::Kind::Generics);
    while (true) {
      if (Mangled.nextIf('_')) {
        if (!Mangled)
          return nullptr;
        char c = Mangled.peek();
        if (c != '_' && c != 'S' && !isStartOfIdentifier(c))
          break;
        archetypes->addChild(Node::create(
            Node::Kind::ArchetypeRef, archetypeName(ArchetypeCount)));
      } else {
        NodePointer proto_list = demangleProtocolList();
        if (!proto_list)
          return nullptr;
        NodePointer arch_and_proto =
            Node::create(Node::Kind::ArchetypeAndProtocol);
        arch_and_proto->addChild(Node::create(
            Node::Kind::ArchetypeRef, archetypeName(ArchetypeCount)));
        arch_and_proto->addChild(proto_list);
        archetypes->addChild(arch_and_proto);
      }
      ++ArchetypeCount;
    }
    return archetypes;
  }

  NodePointer demangleArchetypeRef(Node::IndexType depth, Node::IndexType i) {
    if (depth == 0 && ArchetypeCount == 0)
      return Node::create(Node::Kind::ArchetypeRef, archetypeName(i));
    size_t length = ArchetypeCounts.size();
    if (depth >= length)
      return nullptr;
    size_t index = ArchetypeCounts[length - 1 - depth] + i;
    size_t max =
        (depth == 0) ? ArchetypeCount : ArchetypeCounts[length - depth];
    if (index >= max)
      return nullptr;
    return Node::create(Node::Kind::ArchetypeRef,
                                 archetypeName(index));
  }
  
  NodePointer demangleArchetypeType() {
    auto makeSelfType = [&](NodePointer proto) -> NodePointer {
      NodePointer selfType
        = Node::create(Node::Kind::SelfTypeRef);
      selfType->addChild(proto);
      Substitutions.push_back(selfType);
      return selfType;
    };
    
    auto makeAssociatedType = [&](NodePointer root) -> NodePointer {
      NodePointer name = demangleIdentifier();
      if (!name) return nullptr;
      NodePointer assocType
        = Node::create(Node::Kind::AssociatedTypeRef);
      assocType->addChild(root);
      assocType->addChild(name);
      Substitutions.push_back(assocType);
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
      NodePointer sub = demangleSubstitutionIndex();
      if (!sub) return nullptr;
      if (sub->getKind() == Node::Kind::Protocol)
        return makeSelfType(sub);
      else
        return makeAssociatedType(sub);
    }
    if (Mangled.nextIf('d')) {
      Node::IndexType depth, index;
      if (!demangleIndex(depth))
        return nullptr;
      if (!demangleIndex(index))
        return nullptr;
      return demangleArchetypeRef(depth + 1, index);
    }
    if (Mangled.nextIf('q')) {
      NodePointer index = demangleIndexAsNode();
      if (!index)
        return nullptr;
      NodePointer decl_ctx = Node::create(Node::Kind::DeclContext);
      NodePointer ctx = demangleContext();
      if (!ctx)
        return nullptr;
      decl_ctx->addChild(ctx);
      NodePointer qual_atype = Node::create(Node::Kind::QualifiedArchetype);
      qual_atype->addChild(index);
      qual_atype->addChild(decl_ctx);
      return qual_atype;
    }
    Node::IndexType index;
    if (!demangleIndex(index))
      return nullptr;
    return demangleArchetypeRef(0, index);
  }

  NodePointer demangleTuple(IsVariadic isV) {
    NodePointer tuple = Node::create(
        isV == IsVariadic::yes ? Node::Kind::VariadicTuple
                               : Node::Kind::NonVariadicTuple);
    while (!Mangled.nextIf('_')) {
      if (!Mangled)
        return nullptr;
      NodePointer elt = Node::create(Node::Kind::TupleElement);

      if (isStartOfIdentifier(Mangled.peek())) {
        NodePointer label = demangleIdentifier(Node::Kind::TupleElementName);
        if (!label)
          return nullptr;
        elt->addChild(label);
      }

      NodePointer type = demangleType();
      if (!type)
        return nullptr;
      elt->addChild(type);

      tuple->addChild(elt);
    }
    return tuple;
  }
  
  NodePointer postProcessReturnTypeNode (NodePointer out_args) {
    NodePointer out_node = Node::create(Node::Kind::ReturnType);
    out_node->addChild(out_args);
    return out_node;
  }

  NodePointer demangleType() {
    NodePointer type = demangleTypeImpl();
    if (!type)
      return nullptr;
    NodePointer nodeType = Node::create(Node::Kind::Type);
    nodeType->addChild(type);
    return nodeType;
  }
  
  NodePointer demangleTypeImpl() {
    if (!Mangled)
      return nullptr;
    char c = Mangled.next();
    if (c == 'A') {
      Node::IndexType size;
      if (demangleNatural(size)) {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer array = Node::create(Node::Kind::ArrayType);
        array->addChild(type);
        array->addChild(Node::create(Node::Kind::Number, size));
        return array;
      }
      return nullptr;
    }
    if (c == 'B') {
      if (!Mangled)
        return nullptr;
      c = Mangled.next();
      if (c == 'f') {
        Node::IndexType size;
        if (demangleBuiltinSize(size)) {
          return Node::create(
              Node::Kind::BuiltinTypeName,
              (DemanglerPrinter() << "Builtin.Float" << size).str());
        }
      }
      if (c == 'i') {
        Node::IndexType size;
        if (demangleBuiltinSize(size)) {
          return Node::create(
              Node::Kind::BuiltinTypeName,
              (DemanglerPrinter() << "Builtin.Int" << size).str());
        }
      }
      if (c == 'v') {
        Node::IndexType elts;
        if (demangleNatural(elts)) {
          if (!Mangled.nextIf('B'))
            return nullptr;
          if (Mangled.nextIf('i')) {
            Node::IndexType size;
            if (!demangleBuiltinSize(size))
              return nullptr;
            return Node::create(
                Node::Kind::BuiltinTypeName,
                (DemanglerPrinter() << "Builtin.Vec" << elts << "xInt" << size)
                    .str());
          }
          if (Mangled.nextIf('f')) {
            Node::IndexType size;
            if (!demangleBuiltinSize(size))
              return nullptr;
            return Node::create(
                Node::Kind::BuiltinTypeName,
                (DemanglerPrinter() << "Builtin.Vec" << elts << "xFloat"
                                    << size).str());
          }
          if (Mangled.nextIf('p'))
            return Node::create(
                Node::Kind::BuiltinTypeName,
                (DemanglerPrinter() << "Builtin.Vec" << elts << "xRawPointer")
                    .str());
        }
      }
      if (c == 'O')
        return Node::create(Node::Kind::BuiltinTypeName,
                                     "Builtin.ObjCPointer");
      if (c == 'o')
        return Node::create(Node::Kind::BuiltinTypeName,
                                     "Builtin.ObjectPointer");
      if (c == 'p')
        return Node::create(Node::Kind::BuiltinTypeName,
                                     "Builtin.RawPointer");
      if (c == 'w')
        return Node::create(Node::Kind::BuiltinTypeName,
                                     "Builtin.Word");
      return nullptr;
    }
    if (c == 'a')
      return demangleDeclarationName(Node::Kind::TypeAlias);

    if (c == 'b') {
      NodePointer in_args = demangleType();
      if (!in_args)
        return nullptr;
      NodePointer out_args = demangleType();
      if (!out_args)
        return nullptr;
      NodePointer block = Node::create(Node::Kind::ObjCBlock);
      NodePointer in_node = Node::create(Node::Kind::ArgumentTuple);
      block->addChild(in_node);
      in_node->addChild(in_args);
      block->addChild(postProcessReturnTypeNode(out_args));
      return block;
    }
    if (c == 'E') {
      if (!Mangled.nextIf('R'))
        return nullptr;
      if (!Mangled.nextIf('R'))
        return nullptr;
      return Node::create(Node::Kind::ErrorType, std::string());
    }
    if (c == 'F') {
      NodePointer in_args = demangleType();
      if (!in_args)
        return nullptr;
      NodePointer out_args = demangleType();
      if (!out_args)
        return nullptr;
      NodePointer block = Node::create(Node::Kind::FunctionType);
      NodePointer in_node = Node::create(Node::Kind::ArgumentTuple);
      block->addChild(in_node);
      in_node->addChild(in_args);
      block->addChild(postProcessReturnTypeNode(out_args));
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
          Node::create(Node::Kind::UncurriedFunctionType);
      block->addChild(in_args);
      block->addChild(postProcessReturnTypeNode(out_args));
      return block;
    }
    if (c == 'G') {
      NodePointer type_list = Node::create(Node::Kind::TypeList);
      NodePointer unboundType = demangleType();
      if (!unboundType)
        return nullptr;
      if (Mangled.isEmpty())
        return nullptr;
      while (Mangled.peek() != '_') {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        type_list->addChild(type);
        if (Mangled.isEmpty())
          return nullptr;
      }
      Mangled.next();
      Node::Kind bound_type_kind = Node::Kind::Unknown;
      switch (unboundType->getChild(0)->getKind()) { // look through Type node
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
          Node::create(bound_type_kind);
      type_application->addChild(unboundType);
      type_application->addChild(type_list);
      return type_application;
    }
    if (c == 'M') {
      NodePointer type = demangleType();
      if (!type)
        return nullptr;
      NodePointer metatype = Node::create(Node::Kind::Metatype);
      metatype->addChild(type);
      return metatype;
    }
    if (c == 'P') {
      return demangleProtocolList();
    }
    if (c == 'Q') {
      return demangleArchetypeType();
    }
    if (c == 'R') {
      NodePointer inout = Node::create(Node::Kind::InOut);
      NodePointer type = demangleTypeImpl();
      if (!type)
        return nullptr;
      inout->addChild(type);
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
      GenericContext genericContext(*this);
      NodePointer generics = demangleGenerics(genericContext);
      if (!generics)
        return nullptr;
      NodePointer base = demangleType();
      if (!base)
        return nullptr;
      NodePointer genericType = Node::create(Node::Kind::GenericType);
      genericType->addChild(generics);
      genericType->addChild(base);
      return genericType;
    }
    if (c == 'X') {
      if (Mangled.nextIf('o')) {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer unowned = Node::create(Node::Kind::Unowned);
        unowned->addChild(type);
        return unowned;
      }
      if (Mangled.nextIf('w')) {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer weak = Node::create(Node::Kind::Weak);
        weak->addChild(type);
        return weak;
      }

      // type ::= 'XF' impl-function-type
      if (Mangled.nextIf('F')) {
        return demangleImplFunctionType();
      }

      return nullptr;
    }
    if (isStartOfNominalType(c)) {
      NodePointer nominal_type = demangleDeclarationName(nominalTypeMarkerToNodeKind(c));
      return nominal_type;
    }
    return nullptr;
  }

  bool demangleReabstractSignature(NodePointer signature) {
    Optional<GenericContext> genericContext;

    if (Mangled.nextIf('G')) {
      genericContext.emplace(*this);
      NodePointer generics = demangleGenerics(genericContext.getValue());
      if (!generics) return failure();
      signature->addChild(std::move(generics));
    }

    NodePointer srcType = demangleType();
    if (!srcType) return failure();
    signature->addChild(std::move(srcType));

    NodePointer destType = demangleType();
    if (!destType) return failure();
    signature->addChild(std::move(destType));

    return true;
  }

  // impl-function-type ::= impl-callee-convention impl-function-attribute*
  //                        generics? '_' impl-parameter* '_' impl-result* '_'
  // impl-function-attribute ::= 'Cb'            // compatible with C block invocation function
  // impl-function-attribute ::= 'Cc'            // compatible with C global function
  // impl-function-attribute ::= 'Cm'            // compatible with Swift method
  // impl-function-attribute ::= 'CO'            // compatible with ObjC method
  // impl-function-attribute ::= 'Cw'            // compatible with protocol witness
  // impl-function-attribute ::= 'N'             // noreturn
  // impl-function-attribute ::= 'G'             // generic
  NodePointer demangleImplFunctionType() {
    NodePointer type = Node::create(Node::Kind::ImplFunctionType);

    if (!demangleImplCalleeConvention(type))
      return nullptr;

    if (Mangled.nextIf('C')) {
      if (Mangled.nextIf('b'))
        addImplFunctionAttribute(type, "@objc_block");
      else if (Mangled.nextIf('c'))
        addImplFunctionAttribute(type, "@cc(cdecl)");
      else if (Mangled.nextIf('m'))
        addImplFunctionAttribute(type, "@cc(method)");
      else if (Mangled.nextIf('O'))
        addImplFunctionAttribute(type, "@cc(objc_method)");
      else if (Mangled.nextIf('w'))
        addImplFunctionAttribute(type, "@cc(witness_method)");
      else
        return nullptr;
    }

    if (Mangled.nextIf('N'))
      addImplFunctionAttribute(type, "@noreturn");

    // Enter a new generic context if this type is generic.
    Optional<GenericContext> genericContext;
    if (Mangled.nextIf('G')) {
      genericContext.emplace(*this);
      NodePointer generics = demangleGenerics(genericContext.getValue());
      if (!generics)
        return nullptr;
      type->addChild(generics);
    }

    // Expect the attribute terminator.
    if (!Mangled.nextIf('_'))
      return nullptr;

    // Demangle the parameters.
    if (!demangleImplParameters(type.getPtr()))
      return nullptr;

    // Demangle the result type.
    if (!demangleImplResults(type.getPtr()))
      return nullptr;

    return type;
  }

  enum class ImplConventionContext { Callee, Parameter, Result };

  // impl-convention ::= 'a'                     // direct, autoreleased
  // impl-convention ::= 'd'                     // direct, no ownership transfer
  // impl-convention ::= 'g'                     // direct, guaranteed
  // impl-convention ::= 'i'                     // indirect, ownership transfer
  // impl-convention ::= 'l'                     // indirect, inout
  // impl-convention ::= 'o'                     // direct, ownership transfer
  Optional<StringRef> demangleImplConvention(ImplConventionContext ctxt) {
#define CASE(CHAR, FOR_CALLEE, FOR_PARAMETER, FOR_RESULT)            \
    if (Mangled.nextIf(CHAR)) {                                      \
      switch (ctxt) {                                                \
      case ImplConventionContext::Callee: return (FOR_CALLEE);       \
      case ImplConventionContext::Parameter: return (FOR_PARAMETER); \
      case ImplConventionContext::Result: return (FOR_RESULT);       \
      }                                                              \
      llvm_unreachable("bad context");                               \
    }

    CASE('a',   Nothing,                Nothing,         "@autoreleased")
    CASE('d',   "@callee_unowned",      "@unowned",      "@unowned")
    CASE('g',   "@callee_guaranteed",   "@guaranteed",   Nothing)
    CASE('i',   Nothing,                "@in",           "@out")
    CASE('l',   Nothing,                "@inout",        Nothing)
    CASE('o',   "@callee_owned",        "@owned",        "@owned")
    return Nothing;

#undef RETURN
  }

  // impl-callee-convention ::= 't'
  // impl-callee-convention ::= impl-convention
  bool demangleImplCalleeConvention(NodePointer type) {
    StringRef attr;
    if (Mangled.nextIf('t')) {
      attr = "@thin";
    } else if (auto optConv =
                 demangleImplConvention(ImplConventionContext::Callee)) {
      attr = optConv.getValue();
    } else {
      return failure();
    }
    type->addChild(Node::create(Node::Kind::ImplConvention, attr));
    return true;
  }

  void addImplFunctionAttribute(NodePointer parent, StringRef attr,
                         Node::Kind kind = Node::Kind::ImplFunctionAttribute) {
    parent->addChild(Node::create(kind, attr));
  }

  // impl-parameter ::= impl-convention type
  bool demangleImplParameters(Node *parent) {
    while (!Mangled.nextIf('_')) {
      auto input = demangleImplParameterOrResult(Node::Kind::ImplParameter);
      if (!input) return false;
      parent->addChild(input);
    }
    return true;
  }

  // impl-result ::= impl-convention type
  bool demangleImplResults(Node *parent) {
    while (!Mangled.nextIf('_')) {
      auto res = demangleImplParameterOrResult(Node::Kind::ImplResult);
      if (!res) return false;
      parent->addChild(res);
    }
    return true;
  }

  NodePointer demangleImplParameterOrResult(Node::Kind kind) {
    auto getContext = [](Node::Kind kind) -> ImplConventionContext {
      if (kind == Node::Kind::ImplParameter)
        return ImplConventionContext::Parameter;
      else if (kind == Node::Kind::ImplResult)
        return ImplConventionContext::Result;
      else
        llvm_unreachable("unexpected node kind");
    };

    auto convention = demangleImplConvention(getContext(kind));
    if (!convention) return nullptr;
    auto type = demangleType();
    if (!type) return nullptr;

    NodePointer node = Node::create(kind);
    node->addChild(Node::create(Node::Kind::ImplConvention,
                                convention.getValue()));
    node->addChild(type);
    return node;
  }
};
} // end anonymous namespace

NodePointer Demangle::demangleSymbolAsNode(llvm::StringRef mangled,
                                           const DemangleOptions &options) {
  PrettyStackTraceStringAction prettyStackTrace("demangling string", mangled);
  Demangler demangler(mangled);
  demangler.demangle();
  return demangler.getDemangled();
}

namespace {
class NodePrinter {
private:
  DemanglerPrinter Printer;
  DemangleOptions Options;
  
public:
  NodePrinter(DemangleOptions options) : Options(options) {}
  
  std::string printRoot(Node *root) {
    print(root);
    return Printer.str();
  }

private:  
  void printChildren(Node::iterator begin,
                     Node::iterator end,
                     const char *sep = nullptr) {
    for (; begin != end;) {
      print(begin->getPtr());
      ++begin;
      if (sep && begin != end)
        Printer << sep;
    }
  }
  
  void printChildren(Node *pointer, const char *sep = nullptr) {
    if (!pointer)
      return;
    Node::iterator begin = pointer->begin(), end = pointer->end();
    printChildren(begin, end, sep);
  }
  
  Node *getFirstChildOfKind(Node *pointer, Node::Kind kind) {
    if (!pointer)
      return nullptr;
    for (NodePointer &child : *pointer) {
      if (child && child->getKind() == kind)
        return child.getPtr();
    }
    return nullptr;
  }
  
  bool typeNeedsColonForDecl(Node *type) {
    if (!type)
      return false;
    if (!type->hasChildren())
      return false;
    Node *child = type->getChild(0);
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
  
  void printBoundGenericNoSugar(Node *pointer) {
    if (pointer->getNumChildren() < 2)
      return;
    Node *typelist = pointer->getChild(1);
    print(pointer->getChild(0));
    Printer << "<";
    printChildren(typelist, ", ");
    Printer << ">";
  }

  static bool isSwiftModule(Node *node) {
    return (node->getKind() == Node::Kind::Module &&
            node->getText() == "swift");
  }

  static bool isIdentifier(Node *node, StringRef desired) {
    return (node->getKind() == Node::Kind::Identifier &&
            node->getText() == desired);
  }
  
  enum class SugarType {
    None,
    Optional,
    Slice
  };
  
  SugarType findSugar(NodePointer pointer) {
    if (pointer->getNumChildren() == 1 && pointer->getKind() == Node::Kind::Type)
      return findSugar(pointer->getChild(0));
    
    if (pointer->getNumChildren() != 2)
      return SugarType::None;
    
    if (pointer->getKind() != Node::Kind::BoundGenericEnum &&
        pointer->getKind() != Node::Kind::BoundGenericStructure)
      return SugarType::None;

    auto unboundType = pointer->getChild(0)->getChild(0); // drill through Type
    auto typeArgs = pointer->getChild(1);
    
    if (pointer->getKind() == Node::Kind::BoundGenericEnum) {
      // swift.Optional
      if (isIdentifier(unboundType->getChild(1), "Optional") &&
          typeArgs->getNumChildren() == 1 &&
          isSwiftModule(unboundType->getChild(0))) {
        return SugarType::Optional;
      }
      
      return SugarType::None;
    }
    else /*if (pointer->getKind() == Node::Kind::BoundGenericStructure)*/
    {
      // swift.Array
      if (isIdentifier(unboundType->getChild(1), "Array") &&
          typeArgs->getNumChildren() == 1 &&
          isSwiftModule(unboundType->getChild(0))) {
        return SugarType::Slice;
      }

      return SugarType::None;
    }

    return SugarType::None;
    
  }
  
  void printBoundGeneric(Node *pointer) {
    if (pointer->getNumChildren() < 2)
      return;
    if (pointer->getNumChildren() != 2) {
      printBoundGenericNoSugar(pointer);
      return;
    }

    if (Options.SynthesizeSugarOnTypes == false ||
        pointer->getKind() == Node::Kind::BoundGenericClass)
    {
      // no sugar here
      printBoundGenericNoSugar(pointer);
      return;
    }

    SugarType sugarType = findSugar(pointer);
    
    switch (sugarType)
    {
      case SugarType::None:
        printBoundGenericNoSugar(pointer);
        break;
      case SugarType::Optional: {
        Node *type = pointer->getChild(1)->getChild(0);
        bool needs_parens = false;
        if (findSugar(type) != SugarType::None)
          needs_parens = true;
        if (needs_parens)
          Printer << "(";
        print(type);
        if (needs_parens)
          Printer << ")";
        Printer << "?";
      }
        break;
      case SugarType::Slice: {
        Node *type = pointer->getChild(1)->getChild(0);
        bool needs_parens = false;
        if (findSugar(type) != SugarType::None)
          needs_parens = true;
        if (needs_parens)
          Printer << "(";
        print(type);
        if (needs_parens)
          Printer << ")";
        Printer << "[]";
      }
        break;
    }
  }

  void printImplFunctionType(Node *fn) {
    enum State { Attrs, Inputs, Results } curState = Attrs;
    auto transitionTo = [&](State newState) {
      assert(newState >= curState);
      for (; curState != newState; curState = State(curState + 1)) {
        switch (curState) {
        case Attrs: Printer << '('; continue;
        case Inputs: Printer << ") -> ("; continue;
        case Results: llvm_unreachable("no state after Results");
        }
        llvm_unreachable("bad state");
      }
    };

    for (auto &child : *fn) {
      if (child->getKind() == Node::Kind::ImplParameter) {
        if (curState == Inputs) Printer << ", ";
        transitionTo(Inputs);
        print(child.getPtr());
      } else if (child->getKind() == Node::Kind::ImplResult) {
        if (curState == Results) Printer << ", ";
        transitionTo(Results);
        print(child.getPtr());
      } else {
        assert(curState == Attrs);
        print(child.getPtr());
        Printer << ' ';
      }
    }
    transitionTo(Results);
    Printer << ')';
  }

  void printContext(Node *context) {
    // TODO: parenthesize local contexts?
    print(context, /*asContext*/ true);
    Printer << '.';
  }

  void print(Node *pointer, bool asContext = false, bool suppressType = false);
};
} // end anonymous namespace

void NodePrinter::print(Node *pointer, bool asContext, bool suppressType) {
  // Common code for handling entities.
  auto printEntity = [&](bool hasName, bool hasType, StringRef extraName) {
    printContext(pointer->getChild(0));

    bool printType = (hasType && !suppressType);
    bool useParens = (printType && asContext);

    if (useParens) Printer << '(';

    if (hasName) print(pointer->getChild(1));
    Printer << extraName;

    if (printType &&
        // FIXME: Hack for rdar://15753317
        pointer->getNumChildren() > 1 + unsigned(hasName)) {
      Node *type = pointer->getChild(1 + unsigned(hasName));
      if (typeNeedsColonForDecl(type))
        Printer << " : ";
      else
        Printer << " ";
      print(type);
    }

    if (useParens) Printer << ')';      
  };

  Node::Kind kind = pointer->getKind();
  switch (kind) {
  case Node::Kind::Failure:
    return;
  case Node::Kind::Directness:
    Printer << pointer->getText() << " ";
    return;
  case Node::Kind::Variable:
  case Node::Kind::Function:
    printEntity(true, true, "");
    return;
  case Node::Kind::ExplicitClosure:
  case Node::Kind::ImplicitClosure: {
    auto index = pointer->getChild(1)->getIndex();
    DemanglerPrinter name;
    name << '(';
    if (pointer->getKind() == Node::Kind::ImplicitClosure)
      name << "implicit ";
    name << "closure #" << (index + 1) << ")";
    printEntity(false, false, name.str());
    return;
  }
  case Node::Kind::Global:
    printChildren(pointer);
    return;
  case Node::Kind::Suffix:
    Printer << " with unmangled suffix " << QuotedString(pointer->getText());
    return;
  case Node::Kind::Initializer:
    printEntity(false, true, "initializer");
    return;
  case Node::Kind::DefaultArgumentInitializer: {
    auto index = pointer->getChild(1);
    DemanglerPrinter strPrinter;
    strPrinter << "(default argument " << index->getIndex() << ")";
    printEntity(false, false, strPrinter.str());
    return;
  }
  case Node::Kind::DeclContext:
    print(pointer->getChild(0), asContext);
    return;
  case Node::Kind::Type:
    print(pointer->getChild(0), asContext);
    return;
  case Node::Kind::Class:
  case Node::Kind::Structure:
  case Node::Kind::Enum:
  case Node::Kind::Protocol:
  case Node::Kind::TypeAlias:
    printEntity(true, false, "");
    return;
  case Node::Kind::LocalDeclName:
    Printer << '(';
    print(pointer->getChild(1));
    Printer << " #" << (pointer->getChild(0)->getIndex() + 1) << ')';
    return;
  case Node::Kind::Module:
  case Node::Kind::Identifier:
    Printer << pointer->getText();
    return;
  case Node::Kind::FunctionType:
    printChildren(pointer);
    return;
  case Node::Kind::UncurriedFunctionType: {
    Node *metatype = pointer->getChild(0);
    Printer << "(";
    print(metatype);
    Printer << ")";
    Node *real_func = pointer->getChild(1);
    real_func = real_func->getChild(0);
    printChildren(real_func);
    return;
  }
  case Node::Kind::ArgumentTuple: {
    bool need_parens = false;
    if (pointer->getNumChildren() > 1)
      need_parens = true;
    else {
      if (!pointer->hasChildren())
        need_parens = true;
      else {
        Node::Kind child0_kind = pointer->getChild(0)->getChild(0)->getKind();
        if (child0_kind != Node::Kind::VariadicTuple &&
            child0_kind != Node::Kind::NonVariadicTuple)
          need_parens = true;
      }
    }
    if (need_parens)
      Printer << "(";
    printChildren(pointer);
    if (need_parens)
      Printer << ")";
    return;
  }
  case Node::Kind::NonVariadicTuple:
  case Node::Kind::VariadicTuple: {
    Printer << "(";
    printChildren(pointer, ", ");
    if (pointer->getKind() == Node::Kind::VariadicTuple)
      Printer << "...";
    Printer << ")";
    return;
  }
  case Node::Kind::TupleElement:
    if (pointer->getNumChildren() == 1) {
      Node *type = pointer->getChild(0);
      print(type);
    } else if (pointer->getNumChildren() == 2) {
      Node *id = pointer->getChild(0);
      Node *type = pointer->getChild(1);
      print(id);
      print(type);
    }
    return;
  case Node::Kind::TupleElementName:
    Printer << pointer->getText() << " : ";
    return;
  case Node::Kind::TupleElementType:
    Printer << pointer->getText();
    return;
  case Node::Kind::ReturnType:
    if (pointer->getNumChildren() == 0)
      Printer << " -> " << pointer->getText();
    else {
      Printer << " -> ";
      printChildren(pointer);
    }
    return;
  case Node::Kind::Weak:
    Printer << "@weak ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::Unowned:
    Printer << "@unowned ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::InOut:
    Printer << "@inout ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::ObjCAttribute:
    Printer << "@objc ";
    return;
  case Node::Kind::BuiltinTypeName:
    Printer << pointer->getText();
    return;
  case Node::Kind::Number:
    Printer << pointer->getIndex();
    return;
  case Node::Kind::ArrayType: {
    Node *type = pointer->getChild(0);
    Node *size = pointer->getChild(1);
    print(type);
    Printer << "[";
    print(size);
    Printer << "]";
    return;
  }
  case Node::Kind::InfixOperator:
    Printer << pointer->getText() << " @infix";
    return;
  case Node::Kind::PrefixOperator:
    Printer << pointer->getText() << " @prefix";
    return;
  case Node::Kind::PostfixOperator:
    Printer << pointer->getText() << " @postfix";
    return;
  case Node::Kind::DependentProtocolWitnessTableGenerator:
    Printer << "dependent protocol witness table generator for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::DependentProtocolWitnessTableTemplate:
    Printer << "dependent protocol witness table template for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::LazyProtocolWitnessTableAccessor:
    Printer << "lazy protocol witness table accessor for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::LazyProtocolWitnessTableTemplate:
    Printer << "lazy protocol witness table template for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::ProtocolWitnessTable:
    Printer << "protocol witness table for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::ProtocolWitness: {
    Printer << "protocol witness for ";
    print(pointer->getFirstChild());
    return;
  }
  case Node::Kind::FieldOffset: {
    Printer << "field offset for ";
    auto entity = pointer->getFirstChild();
    print(entity, /*asContext*/ false,
             /*suppressType*/ !Options.DisplayTypeOfIVarFieldOffset);
    return;
  }
  case Node::Kind::BridgeToBlockFunction:
    Printer << "bridge-to-block function for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::ReabstractionThunk:
  case Node::Kind::ReabstractionThunkHelper: {
    Printer << "reabstraction thunk ";
    if (pointer->getKind() == Node::Kind::ReabstractionThunkHelper)
      Printer << "helper ";
    auto generics = getFirstChildOfKind(pointer, Node::Kind::Generics);
    assert(pointer->getNumChildren() == 2 + unsigned(generics != nullptr));
    if (generics) {
      print(generics);
      Printer << " ";
    }
    Printer << "from ";
    print(pointer->getChild(pointer->getNumChildren() - 2));
    Printer << " to ";
    print(pointer->getChild(pointer->getNumChildren() - 1));
    return;
  }
  case Node::Kind::GenericTypeMetadataPattern:
    Printer << "generic type metadata pattern for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::Metaclass:
    Printer << "metaclass for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::TypeMetadata:
    Printer << "type metadata for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::NominalTypeDescriptor:
    Printer << "nominal type descriptor for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::ValueWitnessKind:
    Printer << pointer->getText() << " value witness for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::ValueWitnessTable:
    Printer << "value witness table for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::WitnessTableOffset:
    Printer << "witness table offset for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::BoundGenericClass:
  case Node::Kind::BoundGenericStructure:
  case Node::Kind::BoundGenericEnum:
    printBoundGeneric(pointer);
    return;
  case Node::Kind::ObjCBlock: {
    Printer << "@objc_block ";
    Node *tuple = pointer->getChild(0);
    Node *rettype = pointer->getChild(1);
    print(tuple);
    print(rettype);
    return;
  }
  case Node::Kind::Metatype: {
    Node *type = pointer->getChild(0);
    print(type);
    Printer << ".metatype";
    return;
  }
  case Node::Kind::ArchetypeRef:
    Printer << pointer->getText();
    return;
  case Node::Kind::AssociatedTypeRef:
    print(pointer->getChild(0));
    Printer << '.' << pointer->getChild(1)->getText();
    return;
  case Node::Kind::SelfTypeRef:
    print(pointer->getChild(0));
    Printer << ".Self";
    return;
  case Node::Kind::ProtocolList: {
    Node *type_list = pointer->getChild(0);
    if (!type_list)
      return;
    bool needs_proto_marker = (type_list->getNumChildren() != 1);
    if (needs_proto_marker)
      Printer << "protocol<";
    printChildren(type_list, ", ");
    if (needs_proto_marker)
      Printer << ">";
    return;
  }
  case Node::Kind::Generics: {
    if (pointer->getNumChildren() == 0)
      return;
    Printer << "<";
    printChildren(pointer, ", ");
    Printer << ">";
    return;
  }
  case Node::Kind::QualifiedArchetype: {
    if (pointer->getNumChildren() < 2)
      return;
    Node *number = pointer->getChild(0);
    Node *decl_ctx = pointer->getChild(1);
    Printer << "(archetype " << number->getIndex() << " of ";
    print(decl_ctx);
    Printer << ")";
    return;
  }
  case Node::Kind::GenericType: {
    Node *atype_list = pointer->getChild(0);
    Node *fct_type = pointer->getChild(1)->getChild(0);
    print(atype_list);
    print(fct_type);
    return;
  }
  case Node::Kind::Addressor:
    printEntity(true, true, ".addressor");
    return;
  case Node::Kind::Getter:
    printEntity(true, true, ".getter");
    return;
  case Node::Kind::Setter:
    printEntity(true, true, ".setter");
    return;
  case Node::Kind::Allocator:
    printEntity(false, true, "__allocating_init");
    return;
  case Node::Kind::Constructor:
    printEntity(false, true, "init");
    return;
  case Node::Kind::Destructor:
    printEntity(false, false, "destructor");
    return;
  case Node::Kind::Deallocator:
    printEntity(false, false, "__deallocating_destructor");
    return;
  case Node::Kind::ProtocolConformance: {
    Node *child0 = pointer->getChild(0);
    Node *child1 = pointer->getChild(1);
    Node *child2 = pointer->getChild(2);
    print(child0);
    Printer << " : ";
    print(child1);
    Printer << " in ";
    print(child2);
    return;
  }
  case Node::Kind::TypeList:
    printChildren(pointer);
    return;
  case Node::Kind::ArchetypeAndProtocol: {
    Node *child0 = pointer->getChild(0);
    Node *child1 = pointer->getChild(1);
    print(child0);
    Printer << " : ";
    print(child1);
    return;
  }
  case Node::Kind::ImplConvention:
    Printer << pointer->getText();
    return;
  case Node::Kind::ImplFunctionAttribute:
    Printer << pointer->getText();
    return;
  case Node::Kind::ImplParameter:
  case Node::Kind::ImplResult:
    printChildren(pointer, " ");
    return;
  case Node::Kind::ImplFunctionType:
    printImplFunctionType(pointer);
    return;
  case Node::Kind::Unknown:
    return;
  case Node::Kind::ErrorType:
    Printer << "<ERROR TYPE>";
    return;
  }
  llvm_unreachable("bad node kind!");
}

std::string Demangle::nodeToString(NodePointer root,
                                   const DemangleOptions &options) {
  if (!root)
    return "";
  return NodePrinter(options).printRoot(root.getPtr());
}

std::string Demangle::demangleSymbolAsString(llvm::StringRef mangled,
                                             const DemangleOptions &options) {
  auto root = demangleSymbolAsNode(mangled, options);
  if (!root) return mangled.str();

  PrettyStackTraceStringAction trace("printing the demangling of", mangled);
  std::string demangling = nodeToString(std::move(root), options);
  if (demangling.empty())
    return mangled.str();
  return demangling;
}
