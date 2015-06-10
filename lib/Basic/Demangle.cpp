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
#include "swift/Strings.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Punycode.h"
#include "swift/Basic/UUID.h"
#include "llvm/ADT/StringRef.h"
#include <functional>
#include <ostream>
#include <sstream>
#include <vector>
#include <cstdlib>

using namespace swift;
using namespace Demangle;

[[noreturn]]
static void unreachable(const char *Message) {
  fprintf(stderr, "fatal error: %s\n", Message);
  std::abort();
}

namespace {
struct QuotedString {
  std::string Value;

  QuotedString(std::string Value) : Value(Value) {}
};
} // end unnamed namespace

std::ostream &operator<<(std::ostream &OS, const QuotedString &QS) {
  OS << '"';
  for (auto C : QS.Value) {
    switch (C) {
    case '\\': OS << "\\\\"; break;
    case '\t': OS << "\\t"; break;
    case '\n': OS << "\\n"; break;
    case '\r': OS << "\\r"; break;
    case '"': OS << "\\\""; break;
    case '\'': OS << '\''; break; // no need to escape these
    case '\0': OS << "\\0"; break;
    default:
      auto c = static_cast<unsigned char>(C);
      // Other ASCII control characters should get escaped.
      if (c < 0x20 || c == 0x7F) {
        static const char Hexdigit[] = {
          '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
          'A', 'B', 'C', 'D', 'E', 'F'
        };
        OS << "\\x" << Hexdigit[c >> 4] << Hexdigit[c & 0xF];
      } else {
        OS << c;
      }
      break;
    }
  }
  OS << '"';
  return OS;
}

Node::~Node() {
  switch (NodePayloadKind) {
  case PayloadKind::None: return;
  case PayloadKind::Index: return;
  case PayloadKind::Text: TextPayload.~basic_string(); return;
  }
  unreachable("bad payload kind");
}

namespace {
  struct FindPtr {
    FindPtr(Node *v) : Target(v) {}
    bool operator()(NodePointer sp) const {
      return sp.get() == Target;
    }
  private:
    Node *Target;
  };

/// A class for printing to a std::string.
class DemanglerPrinter {
public:
  DemanglerPrinter() {}

  template <class T> DemanglerPrinter &operator<<(T &&Value) {
    Stream << std::forward<T>(Value);
    return *this;
  }

  DemanglerPrinter &operator<<(StringRef Value) {
    Stream.write(Value.data(), Value.size());
    return *this;
  }

  /// Destructively take the contents of this stream.
  std::string str() { return std::move(Stream.str()); }

private:
  std::stringstream Stream;
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
  case 's':
  case 'Z':
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

  bool nextIfNot(char c) {
    if (isEmpty() || peek() == c) return false;
    advanceOffset(1);
    return true;
  }

  /// Claim the next few characters if they exactly match the given string.
  bool nextIf(StringRef str) {
    if (!Text.startswith(str)) return false;
    advanceOffset(str.size());
    return true;
  }

  /// Return the next len characters without claiming them.  Asserts
  /// that there are at least so many characters.
  StringRef slice(size_t len) { return Text.substr(0, len); }

  /// Return the current string ref without claiming any characters.
  StringRef str() { return Text; }

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

  bool readUntil(char c, std::string &result) {
    llvm::Optional<char> c2;
    while (!isEmpty() && (c2 = peek()).getValue() != c) {
      result.push_back(c2.getValue());
      advanceOffset(1);
    }
    return c2.hasValue() && c2.getValue() == c;
  }
};

static StringRef toString(Directness d) {
  switch (d) {
  case Directness::Direct:
    return "direct";
  case Directness::Indirect:
    return "indirect";
  }
  unreachable("bad directness");
}

static StringRef toString(ValueWitnessKind k) {
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
  case ValueWitnessKind::InitializeBufferWithTakeOfBuffer:
    return "initializeBufferWithTakeOfBuffer";
  case ValueWitnessKind::DestroyArray:
    return "destroyArray";
  case ValueWitnessKind::InitializeArrayWithCopy:
    return "initializeArrayWithCopy";
  case ValueWitnessKind::InitializeArrayWithTakeFrontToBack:
    return "initializeArrayWithTakeFrontToBack";
  case ValueWitnessKind::InitializeArrayWithTakeBackToFront:
    return "initializeArrayWithTakeBackToFront";
  case ValueWitnessKind::StoreExtraInhabitant:
    return "storeExtraInhabitant";
  case ValueWitnessKind::GetExtraInhabitantIndex:
    return "getExtraInhabitantIndex";
  case ValueWitnessKind::GetEnumTag:
    return "getEnumTag";
  case ValueWitnessKind::InplaceProjectEnumData:
    return "inplaceProjectEnumData";
  }
  unreachable("bad value witness kind");
}

/// The main class for parsing a demangling tree out of a mangled string.
class Demangler {
  std::vector<NodePointer> Substitutions;
  std::vector<unsigned> ArchetypeCounts;
  unsigned ArchetypeCount = 0;
  NameSource Mangled;
public:  
  Demangler(llvm::StringRef mangled) : Mangled(mangled) {}

/// Try to demangle a child node of the given kind.  If that fails,
/// return; otherwise add it to the parent.
#define DEMANGLE_CHILD_OR_RETURN(PARENT, CHILD_KIND) do { \
    auto _node = demangle##CHILD_KIND();                  \
    if (!_node) return nullptr;                           \
    (PARENT)->addChild(std::move(_node));                 \
  } while (false)

/// Try to demangle a child node of the given kind.  If that fails,
/// return; otherwise add it to the parent.
#define DEMANGLE_CHILD_AS_NODE_OR_RETURN(PARENT, CHILD_KIND) do {  \
    auto _kind = demangle##CHILD_KIND();                           \
    if (!_kind.hasValue()) return nullptr;                         \
    (PARENT)->addChild(NodeFactory::create(Node::Kind::CHILD_KIND, \
                                           unsigned(*_kind)));     \
  } while (false)

  void resetGenericContext() {
    ArchetypeCounts.clear();
    ArchetypeCount = 0;
  }

  /// Attempt to demangle the source string.  The root node will
  /// always be a Global.  Extra characters at the end will be
  /// tolerated (and included as a Suffix node as a child of the
  /// Global).
  ///
  /// \return true if the mangling succeeded
  NodePointer demangleTopLevel() {
    if (!Mangled.nextIf("_T"))
      return nullptr;

    NodePointer topLevel = NodeFactory::create(Node::Kind::Global);

    // First demangle any specialization prefixes.
    if (Mangled.nextIf("TS")) {
      do {
        DEMANGLE_CHILD_OR_RETURN(topLevel, SpecializedAttribute);

        // The Substitution header does not share state with the rest
        // of the mangling.
        Substitutions.clear();
        resetGenericContext();
      } while (Mangled.nextIf("_TTS"));

      // Then check that we have a global.
      if (!Mangled.nextIf("_T"))
        return nullptr;

    } else if (Mangled.nextIf("To")) {
      topLevel->addChild(NodeFactory::create(Node::Kind::ObjCAttribute));
    } else if (Mangled.nextIf("TO")) {
      topLevel->addChild(NodeFactory::create(Node::Kind::NonObjCAttribute));
    } else if (Mangled.nextIf("TD")) {
      topLevel->addChild(NodeFactory::create(Node::Kind::DynamicAttribute));
    } else if (Mangled.nextIf("Td")) {
      topLevel->addChild(NodeFactory::create(
                                   Node::Kind::DirectMethodReferenceAttribute));
    } else if (Mangled.nextIf("TV")) {
      topLevel->addChild(NodeFactory::create(Node::Kind::VTableAttribute));
    }

    DEMANGLE_CHILD_OR_RETURN(topLevel, Global);

    // Add a suffix node if there's anything left unmangled.
    if (!Mangled.isEmpty()) {
      topLevel->addChild(NodeFactory::create(Node::Kind::Suffix,
                                             Mangled.getString()));
    }

    return topLevel;
  }

  NodePointer demangleTypeName() {
    return demangleType();
  }
  
private:
  enum class IsProtocol {
    yes = true, no = false
  };

  enum class IsVariadic {
    yes = true, no = false
  };

  Optional<Directness> demangleDirectness() {
    if (Mangled.nextIf('d'))
      return Directness::Direct;
    if (Mangled.nextIf('i'))
      return Directness::Indirect;
    return None;
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

  Optional<ValueWitnessKind> demangleValueWitnessKind() {
    if (!Mangled)
      return None;
    char c1 = Mangled.next();
    if (!Mangled)
      return None;
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
    if (c1 == 'C' && c2 == 'c')
      return ValueWitnessKind::InitializeArrayWithCopy;
    if (c1 == 'T' && c2 == 'K')
      return ValueWitnessKind::InitializeBufferWithTakeOfBuffer;
    if (c1 == 'T' && c2 == 'k')
      return ValueWitnessKind::InitializeBufferWithTake;
    if (c1 == 't' && c2 == 'k')
      return ValueWitnessKind::InitializeWithTake;
    if (c1 == 'T' && c2 == 't')
      return ValueWitnessKind::InitializeArrayWithTakeFrontToBack;
    if (c1 == 't' && c2 == 'T')
      return ValueWitnessKind::InitializeArrayWithTakeBackToFront;
    if (c1 == 'p' && c2 == 'r')
      return ValueWitnessKind::ProjectBuffer;
    if (c1 == 'X' && c2 == 'x')
      return ValueWitnessKind::DestroyArray;
    if (c1 == 'x' && c2 == 's')
      return ValueWitnessKind::StoreExtraInhabitant;
    if (c1 == 'x' && c2 == 'g')
      return ValueWitnessKind::GetExtraInhabitantIndex;
    if (c1 == 'u' && c2 == 'g')
      return ValueWitnessKind::GetEnumTag;
    if (c1 == 'u' && c2 == 'p')
      return ValueWitnessKind::InplaceProjectEnumData;
    return None;
  }

  NodePointer demangleGlobal() {
    if (!Mangled)
      return nullptr;

    // Type metadata.
    if (Mangled.nextIf('M')) {
      if (Mangled.nextIf('P')) {
        auto pattern =
            NodeFactory::create(Node::Kind::GenericTypeMetadataPattern);
        DEMANGLE_CHILD_AS_NODE_OR_RETURN(pattern, Directness);
        DEMANGLE_CHILD_OR_RETURN(pattern, Type);
        return pattern;
      }
      if (Mangled.nextIf('a')) {
        auto accessor =
          NodeFactory::create(Node::Kind::TypeMetadataAccessFunction);
        DEMANGLE_CHILD_OR_RETURN(accessor, Type);
        return accessor;
      }
      if (Mangled.nextIf('L')) {
        auto cache = NodeFactory::create(Node::Kind::TypeMetadataLazyCache);
        DEMANGLE_CHILD_OR_RETURN(cache, Type);
        return cache;
      }
      if (Mangled.nextIf('m')) {
        auto metaclass = NodeFactory::create(Node::Kind::Metaclass);
        DEMANGLE_CHILD_OR_RETURN(metaclass, Type);
        return metaclass;
      }
      if (Mangled.nextIf('n')) {
        auto nominalType =
            NodeFactory::create(Node::Kind::NominalTypeDescriptor);
        DEMANGLE_CHILD_OR_RETURN(nominalType, Type);
        return nominalType;
      }
      auto metadata = NodeFactory::create(Node::Kind::TypeMetadata);
      DEMANGLE_CHILD_AS_NODE_OR_RETURN(metadata, Directness);
      DEMANGLE_CHILD_OR_RETURN(metadata, Type);
      return metadata;
    }

    // Partial application thunks.
    if (Mangled.nextIf('P')) {
      if (!Mangled.nextIf('A')) return nullptr;
      Node::Kind kind = Node::Kind::PartialApplyForwarder;
      if (Mangled.nextIf('o'))
        kind = Node::Kind::PartialApplyObjCForwarder;
      auto forwarder = NodeFactory::create(kind);
      if (Mangled.nextIf("__T"))
        DEMANGLE_CHILD_OR_RETURN(forwarder, Global);
      return forwarder;
    }

    // Top-level types, for various consumers.
    if (Mangled.nextIf('t')) {
      auto type = NodeFactory::create(Node::Kind::TypeMangling);
      DEMANGLE_CHILD_OR_RETURN(type, Type);
      return type;
    }

    // Value witnesses.
    if (Mangled.nextIf('w')) {
      Optional<ValueWitnessKind> w = demangleValueWitnessKind();
      if (!w.hasValue())
        return nullptr;
      auto witness =
        NodeFactory::create(Node::Kind::ValueWitness, unsigned(w.getValue()));
      DEMANGLE_CHILD_OR_RETURN(witness, Type);
      return witness;
    }

    // Offsets, value witness tables, and protocol witnesses.
    if (Mangled.nextIf('W')) {
      if (Mangled.nextIf('V')) {
        auto witnessTable = NodeFactory::create(Node::Kind::ValueWitnessTable);
        DEMANGLE_CHILD_OR_RETURN(witnessTable, Type);
        return witnessTable;
      }
      if (Mangled.nextIf('o')) {
        auto witnessTableOffset =
            NodeFactory::create(Node::Kind::WitnessTableOffset);
        DEMANGLE_CHILD_OR_RETURN(witnessTableOffset, Entity);
        return witnessTableOffset;
      }
      if (Mangled.nextIf('v')) {
        auto fieldOffset = NodeFactory::create(Node::Kind::FieldOffset);
        DEMANGLE_CHILD_AS_NODE_OR_RETURN(fieldOffset, Directness);
        DEMANGLE_CHILD_OR_RETURN(fieldOffset, Entity);
        return fieldOffset;
      }
      if (Mangled.nextIf('P')) {
        auto witnessTable =
            NodeFactory::create(Node::Kind::ProtocolWitnessTable);
        DEMANGLE_CHILD_OR_RETURN(witnessTable, ProtocolConformance);
        return witnessTable;
      }
      if (Mangled.nextIf('Z')) {
        auto accessor =
          NodeFactory::create(Node::Kind::LazyProtocolWitnessTableAccessor);
        DEMANGLE_CHILD_OR_RETURN(accessor, ProtocolConformance);
        return accessor;
      }
      if (Mangled.nextIf('z')) {
        auto tableTemplate =
          NodeFactory::create(Node::Kind::LazyProtocolWitnessTableTemplate);
        DEMANGLE_CHILD_OR_RETURN(tableTemplate, ProtocolConformance);
        return tableTemplate;
      }
      if (Mangled.nextIf('D')) {
        auto tableGenerator = NodeFactory::create(
            Node::Kind::DependentProtocolWitnessTableGenerator);
        DEMANGLE_CHILD_OR_RETURN(tableGenerator, ProtocolConformance);
        return tableGenerator;
      }
      if (Mangled.nextIf('d')) {
        auto tableTemplate = NodeFactory::create(
            Node::Kind::DependentProtocolWitnessTableTemplate);
        DEMANGLE_CHILD_OR_RETURN(tableTemplate, ProtocolConformance);
        return tableTemplate;
      }
      return nullptr;
    }

    // Other thunks.
    if (Mangled.nextIf('T')) {
      if (Mangled.nextIf('R')) {
        auto thunk = NodeFactory::create(Node::Kind::ReabstractionThunkHelper);
        if (!demangleReabstractSignature(thunk))
          return nullptr;
        return thunk;
      }
      if (Mangled.nextIf('r')) {
        auto thunk = NodeFactory::create(Node::Kind::ReabstractionThunk);
        if (!demangleReabstractSignature(thunk))
          return nullptr;
        return thunk;
      }
      if (Mangled.nextIf('W')) {
        NodePointer thunk = NodeFactory::create(Node::Kind::ProtocolWitness);
        DEMANGLE_CHILD_OR_RETURN(thunk, ProtocolConformance);
        // The entity is mangled in its own generic context.
        resetGenericContext();
        DEMANGLE_CHILD_OR_RETURN(thunk, Entity);
        return thunk;
      }
      return nullptr;
    }

    // Everything else is just an entity.
    return demangleEntity();
  }

  NodePointer demangleGenericSpecialization(NodePointer specialization) {
    while (!Mangled.nextIf('_')) {
      // Otherwise, we have another parameter. Demangle the type.
      NodePointer param = NodeFactory::create(Node::Kind::GenericSpecializationParam);
      DEMANGLE_CHILD_OR_RETURN(param, Type);

      // Then parse any conformances until we find an underscore. Pop off the
      // underscore since it serves as the end of our mangling list.
      while (!Mangled.nextIf('_')) {
        DEMANGLE_CHILD_OR_RETURN(param, ProtocolConformance);
      }

      // Add the parameter to our specialization list.
      specialization->addChild(param);
    }

    return specialization;
  }

/// TODO: This is an atrocity. Come up with a shorter name.
#define FUNCSIGSPEC_CREATE_PARAM_KIND(kind)                                    \
  NodeFactory::create(Node::Kind::FunctionSignatureSpecializationParamKind,    \
                      unsigned(FunctionSigSpecializationParamKind::kind))
#define FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(payload)                              \
  NodeFactory::create(Node::Kind::FunctionSignatureSpecializationParamPayload, \
                      payload)

  bool demangleFuncSigSpecializationConstantProp(NodePointer parent) {
    // Then figure out what was actually constant propagated. First check if
    // we have a function.
    if (Mangled.nextIf("fr")) {
      // Demangle the identifier
      NodePointer name = demangleIdentifier();
      if (!name || !Mangled.nextIf('_'))
        return false;
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_KIND(ConstantPropFunction));
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(name->getText()));
      return true;
    }

    if (Mangled.nextIf('g')) {
      NodePointer name = demangleIdentifier();
      if (!name || !Mangled.nextIf('_'))
        return false;
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_KIND(ConstantPropGlobal));
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(name->getText()));
      return true;
    }

    if (Mangled.nextIf('i')) {
      std::string Str;
      if (!Mangled.readUntil('_', Str) || !Mangled.nextIf('_'))
        return false;
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_KIND(ConstantPropInteger));
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(Str));
      return true;
    }

    if (Mangled.nextIf("fl")) {
      std::string Str;
      if (!Mangled.readUntil('_', Str) || !Mangled.nextIf('_'))
        return false;
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_KIND(ConstantPropFloat));
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(Str));
      return true;
    }

    if (Mangled.nextIf("s")) {
      // Skip: 'e' encoding 'v' str. encoding is a 0 or 1 and str is a string of
      // length less than or equal to 32. We do not specialize strings with a
      // length greater than 32.
      if (!Mangled.nextIf('e'))
        return false;
      char encoding = Mangled.peek();
      if (encoding != '0' && encoding != '1')
        return false;
      std::string encodingStr;
      if (encoding == '0')
        encodingStr += "u8";
      else
        encodingStr += "u16";
      Mangled.advanceOffset(1);

      if (!Mangled.nextIf('v'))
        return false;
      NodePointer str = demangleIdentifier();
      if (!str || !Mangled.nextIf('_'))
        return false;

      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_KIND(ConstantPropString));
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(encodingStr));
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(str->getText()));
      return true;
    }

    unreachable("Unknown constant prop specialization");
  }

  bool demangleFuncSigSpecializationClosureProp(NodePointer parent) {
    // We don't actually demangle the function or types for now. But we do want
    // to signal that we specialized a closure.

    NodePointer name = demangleIdentifier();
    if (!name) {
      return false;
    }

    parent->addChild(FUNCSIGSPEC_CREATE_PARAM_KIND(ClosureProp));
    parent->addChild(FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(name->getText()));

    // Then demangle types until we fail.
    NodePointer type;
    while (Mangled.peek() != '_' && (type = demangleType())) {
      parent->addChild(type);
    }

    // Eat last '_'
    if (!Mangled.nextIf('_'))
      return false;

    return true;
  }

  NodePointer
  demangleFunctionSignatureSpecialization(NodePointer specialization) {
    unsigned paramCount = 0;

    // Until we hit the last '_' in our specialization info...
    while (!Mangled.nextIf('_')) {
      // Create the parameter.
      NodePointer param =
        NodeFactory::create(Node::Kind::FunctionSignatureSpecializationParam,
                            paramCount);

      // First handle options.
      if (Mangled.nextIf("n_")) {
        // Leave the parameter empty.
      } else if (Mangled.nextIf("d_")) {
        auto result = FUNCSIGSPEC_CREATE_PARAM_KIND(Dead);
        if (!result)
          return nullptr;
        param->addChild(result);
      } else if (Mangled.nextIf("cp")) {
        if (!demangleFuncSigSpecializationConstantProp(param))
          return nullptr;
      } else if (Mangled.nextIf("cl")) {
        if (!demangleFuncSigSpecializationClosureProp(param))
          return nullptr;
      } else if (Mangled.nextIf("i_")) {
        auto result = FUNCSIGSPEC_CREATE_PARAM_KIND(InOutToValue);
        if (!result)
          return nullptr;
        param->addChild(result);
      } else {
        // Otherwise handle option sets.
        unsigned Value = 0;
        if (Mangled.nextIf('g')) {
          Value |=
              unsigned(FunctionSigSpecializationParamKind::OwnedToGuaranteed);
        }

        if (Mangled.nextIf('s')) {
          Value |= unsigned(FunctionSigSpecializationParamKind::SROA);
        }

        if (!Mangled.nextIf("_"))
          return nullptr;

        if (!Value)
          return nullptr;

        auto result = NodeFactory::create(
            Node::Kind::FunctionSignatureSpecializationParamKind, Value);
        if (!result)
          return nullptr;
        param->addChild(result);
      }

      specialization->addChild(param);
      paramCount++;
    }

    return specialization;
  }

#undef FUNCSIGSPEC_CREATE_PARAM_KIND
#undef FUNCSIGSPEC_CREATE_PARAM_PAYLOAD

  NodePointer demangleSpecializedAttribute() {
    if (Mangled.nextIf("g")) {
      auto spec = NodeFactory::create(Node::Kind::GenericSpecialization);
      // Create a node for the pass id.
      spec->addChild(NodeFactory::create(Node::Kind::SpecializationPassID,
                                         unsigned(Mangled.next() - 48)));
      // And then mangle the generic specialization.
      return demangleGenericSpecialization(spec);
    }
    if (Mangled.nextIf("f")) {
      auto spec =
          NodeFactory::create(Node::Kind::FunctionSignatureSpecialization);

      // Add the pass id.
      spec->addChild(NodeFactory::create(Node::Kind::SpecializationPassID,
                                         unsigned(Mangled.next() - 48)));

      // Then perform the function signature specialization.
      return demangleFunctionSignatureSpecialization(spec);
    }

    // We don't know how to handle this specialization.
    return nullptr;
  }
  
  NodePointer demangleDeclName() {
    // decl-name ::= local-decl-name
    // local-decl-name ::= 'L' index identifier
    if (Mangled.nextIf('L')) {
      NodePointer discriminator = demangleIndexAsNode();
      if (!discriminator) return nullptr;

      NodePointer name = demangleIdentifier();
      if (!name) return nullptr;

      NodePointer localName = NodeFactory::create(Node::Kind::LocalDeclName);
      localName->addChild(std::move(discriminator));
      localName->addChild(std::move(name));
      return localName;

    } else if (Mangled.nextIf('P')) {
      NodePointer discriminator = demangleIdentifier();
      if (!discriminator) return nullptr;

      NodePointer name = demangleIdentifier();
      if (!name) return nullptr;

      auto privateName = NodeFactory::create(Node::Kind::PrivateDeclName);
      privateName->addChildren(std::move(discriminator), std::move(name));
      return privateName;
    }

    // decl-name ::= identifier
    return demangleIdentifier();
  }

  NodePointer demangleIdentifier(Optional<Node::Kind> kind = None) {
    if (!Mangled)
      return nullptr;
    
    bool isPunycoded = Mangled.nextIf('X');
    std::string decodeBuffer;

    auto decode = [&](StringRef s) -> StringRef {
      if (!isPunycoded)
        return s;
      if (!Punycode::decodePunycodeUTF8(s, decodeBuffer))
        return {};
      return decodeBuffer;
    };
    
    bool isOperator = false;
    if (Mangled.nextIf('o')) {
      isOperator = true;
      // Operator identifiers aren't valid in the contexts that are
      // building more specific identifiers.
      if (kind.hasValue()) return nullptr;

      char op_mode = Mangled.next();
      switch (op_mode) {
      case 'p':
        kind = Node::Kind::PrefixOperator;
        break;
      case 'P':
        kind = Node::Kind::PostfixOperator;
        break;
      case 'i':
        kind = Node::Kind::InfixOperator;
        break;
      default:
        return nullptr;
      }
    }

    if (!kind.hasValue()) kind = Node::Kind::Identifier;

    Node::IndexType length;
    if (!demangleNatural(length))
      return nullptr;
    if (!Mangled.hasAtLeast(length))
      return nullptr;
    
    StringRef identifier = Mangled.slice(length);
    Mangled.advanceOffset(length);
    
    // Decode Unicode identifiers.
    identifier = decode(identifier);
    if (identifier.empty())
      return nullptr;

    // Decode operator names.
    std::string opDecodeBuffer;
    if (isOperator) {
                                        // abcdefghijklmnopqrstuvwxyz
      static const char op_char_table[] = "& @/= >    <*!|+?%-~   ^ .";

      opDecodeBuffer.reserve(identifier.size());
      for (signed char c : identifier) {
        if (c < 0) {
          // Pass through Unicode characters.
          opDecodeBuffer.push_back(c);
          continue;
        }
        if (c < 'a' || c > 'z')
          return nullptr;
        char o = op_char_table[c - 'a'];
        if (o == ' ')
          return nullptr;
        opDecodeBuffer.push_back(o);
      }
      identifier = opDecodeBuffer;
    }
    
    return NodeFactory::create(*kind, identifier);
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
    return NodeFactory::create(kind, index);
  }

  NodePointer createSwiftType(Node::Kind typeKind, StringRef name) {
    NodePointer type = NodeFactory::create(typeKind);
    type->addChild(NodeFactory::create(Node::Kind::Module, STDLIB_NAME));
    type->addChild(NodeFactory::create(Node::Kind::Identifier, name));
    return type;
  }

  /// Demangle a <substitution>, given that we've already consumed the 'S'.
  NodePointer demangleSubstitutionIndex() {
    if (!Mangled)
      return nullptr;
    if (Mangled.nextIf('o'))
      return NodeFactory::create(Node::Kind::Module, "ObjectiveC");
    if (Mangled.nextIf('C'))
      return NodeFactory::create(Node::Kind::Module, "C");
    if (Mangled.nextIf('s'))
      return NodeFactory::create(Node::Kind::Module, STDLIB_NAME);
    if (Mangled.nextIf('a'))
      return createSwiftType(Node::Kind::Structure, "Array");
    if (Mangled.nextIf('b'))
      return createSwiftType(Node::Kind::Structure, "Bool");
    if (Mangled.nextIf('c'))
      return createSwiftType(Node::Kind::Structure, "UnicodeScalar");
    if (Mangled.nextIf('d'))
      return createSwiftType(Node::Kind::Structure, "Double");
    if (Mangled.nextIf('f'))
      return createSwiftType(Node::Kind::Structure, "Float");
    if (Mangled.nextIf('i'))
      return createSwiftType(Node::Kind::Structure, "Int");
    if (Mangled.nextIf('q'))
      return createSwiftType(Node::Kind::Enum, "Optional");
    if (Mangled.nextIf('Q'))
      return createSwiftType(Node::Kind::Enum, "ImplicitlyUnwrappedOptional");
    if (Mangled.nextIf('S'))
      return createSwiftType(Node::Kind::Structure, "String");
    if (Mangled.nextIf('u'))
      return createSwiftType(Node::Kind::Structure, "UInt");
    Node::IndexType index_sub;
    if (!demangleIndex(index_sub))
      return nullptr;
    if (index_sub >= Substitutions.size())
      return nullptr;
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

    auto decl = NodeFactory::create(kind);
    decl->addChild(context);
    decl->addChild(name);
    Substitutions.push_back(decl);
    return decl;
  }

  NodePointer demangleProtocolName() {
    NodePointer proto = demangleProtocolNameImpl();
    if (!proto) return nullptr;

    NodePointer type = NodeFactory::create(Node::Kind::Type);
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

      auto proto = NodeFactory::create(Node::Kind::Protocol);
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
    // context ::= 'E' module context (extension defined in a different module)
    // context ::= 'e' generic-signature module context (generic extension)
    if (!Mangled) return nullptr;
    if (Mangled.nextIf('E')) {
      NodePointer ext = NodeFactory::create(Node::Kind::Extension);
      NodePointer def_module = demangleModule();
      if (!def_module) return nullptr;
      NodePointer type = demangleContext();
      ext->addChild(def_module);
      ext->addChild(type);
      return ext;
    }
    if (Mangled.nextIf('e')) {
      NodePointer ext = NodeFactory::create(Node::Kind::Extension);
      NodePointer sig = demangleGenericSignature();
      // The generic context is currently re-specified by the type mangling.
      // If we ever remove 'self' from manglings, we should stop resetting the
      // context here.
      resetGenericContext();
      if (!sig) return nullptr;
      NodePointer def_module = demangleModule();
      if (!def_module) return nullptr;
      NodePointer type = demangleContext();
      
      ext->addChild(def_module);
      ext->addChild(type);
      ext->addChild(sig);
      return ext;
    }
    if (Mangled.nextIf('S'))
      return demangleSubstitutionIndex();
    if (isStartOfEntity(Mangled.peek()))
      return demangleEntity();
    return demangleModule();
  }
  
  NodePointer demangleProtocolList() {
    NodePointer proto_list = NodeFactory::create(Node::Kind::ProtocolList);
    NodePointer type_list = NodeFactory::create(Node::Kind::TypeList);
    proto_list->addChild(type_list);
    while (!Mangled.nextIf('_')) {
      NodePointer proto = demangleProtocolName();
      if (!proto)
        return nullptr;
      type_list->addChild(std::move(proto));
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
        NodeFactory::create(Node::Kind::ProtocolConformance);
    proto_conformance->addChild(type);
    proto_conformance->addChild(protocol);
    proto_conformance->addChild(context);
    return proto_conformance;
  }

  // entity ::= entity-kind context entity-name
  // entity ::= nominal-type
  NodePointer demangleEntity() {
    // static?
    bool isStatic = Mangled.nextIf('Z');
  
    // entity-kind
    Node::Kind entityBasicKind;
    if (Mangled.nextIf('F')) {
      entityBasicKind = Node::Kind::Function;
    } else if (Mangled.nextIf('v')) {
      entityBasicKind = Node::Kind::Variable;
    } else if (Mangled.nextIf('I')) {
      entityBasicKind = Node::Kind::Initializer;
    } else if (Mangled.nextIf('s')) {
      entityBasicKind = Node::Kind::Subscript;
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
      entityKind = Node::Kind::Deallocator;
      hasType = false;
    } else if (Mangled.nextIf('d')) {
      entityKind = Node::Kind::Destructor;
      hasType = false;
    } else if (Mangled.nextIf('e')) {
      entityKind = Node::Kind::IVarInitializer;
      hasType = false;
    } else if (Mangled.nextIf('E')) {
      entityKind = Node::Kind::IVarDestroyer;
      hasType = false;
    } else if (Mangled.nextIf('C')) {
      entityKind = Node::Kind::Allocator;
    } else if (Mangled.nextIf('c')) {
      entityKind = Node::Kind::Constructor;
    } else if (Mangled.nextIf('a')) {
      if (Mangled.nextIf('O')) {
        entityKind = Node::Kind::OwningMutableAddressor;
      } else if (Mangled.nextIf('o')) {
        entityKind = Node::Kind::NativeOwningMutableAddressor;
      } else if (Mangled.nextIf('p')) {
        entityKind = Node::Kind::NativePinningMutableAddressor;
      } else if (Mangled.nextIf('u')) {
        entityKind = Node::Kind::UnsafeMutableAddressor;
      } else {
        return nullptr;
      }
      name = demangleDeclName();
      if (!name) return nullptr;
    } else if (Mangled.nextIf('l')) {
      if (Mangled.nextIf('O')) {
        entityKind = Node::Kind::OwningAddressor;
      } else if (Mangled.nextIf('o')) {
        entityKind = Node::Kind::NativeOwningAddressor;
      } else if (Mangled.nextIf('p')) {
        entityKind = Node::Kind::NativePinningAddressor;
      } else if (Mangled.nextIf('u')) {
        entityKind = Node::Kind::UnsafeAddressor;
      } else {
        return nullptr;
      }
      name = demangleDeclName();
      if (!name) return nullptr;
    } else if (Mangled.nextIf('g')) {
      entityKind = Node::Kind::Getter;
      name = demangleDeclName();
      if (!name) return nullptr;
    } else if (Mangled.nextIf('G')) {
      entityKind = Node::Kind::GlobalGetter;
      name = demangleDeclName();
      if (!name) return nullptr;
    } else if (Mangled.nextIf('s')) {
      entityKind = Node::Kind::Setter;
      name = demangleDeclName();
      if (!name) return nullptr;
    } else if (Mangled.nextIf('m')) {
      entityKind = Node::Kind::MaterializeForSet;
      name = demangleDeclName();
      if (!name) return nullptr;
    } else if (Mangled.nextIf('w')) {
      entityKind = Node::Kind::WillSet;
      name = demangleDeclName();
      if (!name) return nullptr;
    } else if (Mangled.nextIf('W')) {
      entityKind = Node::Kind::DidSet;
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

    NodePointer entity = NodeFactory::create(entityKind);
    entity->addChild(context);

    if (name) entity->addChild(name);

    if (hasType) {
      auto type = demangleType();
      if (!type) return nullptr;
      entity->addChild(type);
    }
    
    if (isStatic) {
      auto staticNode = NodeFactory::create(Node::Kind::Static);
      staticNode->addChild(entity);
      return staticNode;
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
      D.ArchetypeCount = D.ArchetypeCounts.back();
    }
  };
  
  /// Demangle a generic clause.
  ///
  /// \param C - not really required; just a token to prove that the caller
  ///   has thought to enter a generic context
  NodePointer demangleGenerics(GenericContext &C) {
    NodePointer archetypes = NodeFactory::create(Node::Kind::Generics);
    Node::Kind nodeKind = Node::Kind::Archetype;
    while (true) {
      if (nodeKind == Node::Kind::Archetype && Mangled.nextIf('U')) {
        nodeKind = Node::Kind::AssociatedType;
        continue;
      }

      NodePointer protocolList;
      if (Mangled.nextIf('_')) {
        if (!Mangled)
          return nullptr;
        char c = Mangled.peek();
        if (c != '_' && c != 'S'
            && (nodeKind == Node::Kind::AssociatedType || c != 'U')
            && !isStartOfIdentifier(c))
          break;
      } else {
        protocolList = demangleProtocolList();
        if (!protocolList)
          return nullptr;
      }

      NodePointer archetype;
      if (nodeKind == Node::Kind::Archetype) {
        archetype =
          NodeFactory::create(nodeKind, archetypeName(ArchetypeCount++));
      } else {
        archetype = NodeFactory::create(nodeKind);
      }

      if (protocolList) {
        archetype->addChild(std::move(protocolList));
      }

      archetypes->addChild(std::move(archetype));
    }
    return archetypes;
  }

  NodePointer demangleArchetypeRef(Node::IndexType depth, Node::IndexType i) {
    auto makeArchetypeRef = [&](Node::IndexType nameIndex) -> NodePointer {
      auto ref = NodeFactory::create(Node::Kind::ArchetypeRef,
                                     archetypeName(nameIndex));
      ref->addChild(NodeFactory::create(Node::Kind::Index, depth));
      ref->addChild(NodeFactory::create(Node::Kind::Index, i));
      return ref;
    };
  
    if (depth == 0 && ArchetypeCount == 0) {
      return makeArchetypeRef(i);
    }
    size_t length = ArchetypeCounts.size();
    if (depth >= length)
      return nullptr;
    size_t index = ArchetypeCounts[length - 1 - depth] + i;
    size_t max =
        (depth == 0) ? ArchetypeCount : ArchetypeCounts[length - depth];
    if (index >= max)
      return nullptr;
    
    return makeArchetypeRef(index);
  }
  
  NodePointer demangleDependentType() {
    // A dependent member type begins with a non-index, non-'d' character.
    auto c = Mangled.peek();
    if (c != 'd' && c != '_' && !isdigit(c)) {
      NodePointer baseType = demangleType();
      if (!baseType) return nullptr;
      NodePointer protocol = demangleProtocolName();
      if (!protocol) return nullptr;
      NodePointer depTy = demangleIdentifier(Node::Kind::DependentMemberType);
      if (!depTy) return nullptr;
      
      depTy->addChild(baseType);
      depTy->addChild(protocol);
      return depTy;
    }
    
    // Otherwise, we have a generic parameter.
    Node::IndexType depth, index;
    if (Mangled.nextIf('d')) {
      if (!demangleIndex(depth))
        return nullptr;
      depth += 1;
      if (!demangleIndex(index))
        return nullptr;
    } else {
      depth = 0;
      if (!demangleIndex(index))
        return nullptr;
    }
    
    DemanglerPrinter Name;
    if (depth == 0) {
      Name << archetypeName(index);
    } else {
      if (depth >= ArchetypeCounts.size())
        return nullptr;
      Name << archetypeName(ArchetypeCounts[depth] + index);
    }

    auto paramTy = NodeFactory::create(Node::Kind::DependentGenericParamType,
                                       std::move(Name.str()));
    paramTy->addChild(NodeFactory::create(Node::Kind::Index, depth));
    paramTy->addChild(NodeFactory::create(Node::Kind::Index, index));
    
    return paramTy;
  }
  
  NodePointer demangleGenericSignature() {
    assert(ArchetypeCounts.empty() && "already some generic context?!");

    auto sig = NodeFactory::create(Node::Kind::DependentGenericSignature);
    // First read in the parameter counts at each depth.
    Node::IndexType count = ~(Node::IndexType)0;
    
    auto addCount = [&]{
      auto countNode =
        NodeFactory::create(Node::Kind::DependentGenericParamCount, count);
      sig->addChild(countNode);
      ArchetypeCounts.push_back(ArchetypeCount);
      ArchetypeCount += count;
    };
    
    while (Mangled.peek() != 'R' && Mangled.peek() != 'r') {
      if (Mangled.nextIf('z')) {
        count = 0;
      } else if (demangleIndex(count)) {
        count += 1;
      } else {
        return nullptr;
      }
      addCount();
    }
    
    // No mangled parameters means we have exactly one.
    if (count == ~(Node::IndexType)0) {
      count = 1;
      addCount();
    }
    
    // Next read in the generic requirements, if any.
    if (Mangled.nextIf('r'))
      return sig;
    
    if (!Mangled.nextIf('R'))
      return nullptr;
    
    while (!Mangled.nextIf('_')) {
      NodePointer reqt = demangleGenericRequirement();
      if (!reqt) return nullptr;
      sig->addChild(reqt);
    }
    
    return sig;
  }

  NodePointer demangleMetatypeRepresentation() {
    if (Mangled.nextIf('t'))
      return NodeFactory::create(Node::Kind::MetatypeRepresentation, "@thin");

    if (Mangled.nextIf('T'))
      return NodeFactory::create(Node::Kind::MetatypeRepresentation, "@thick");

    if (Mangled.nextIf('o'))
      return NodeFactory::create(Node::Kind::MetatypeRepresentation,
                                 "@objc_metatype");

    unreachable("Unhandled metatype representation");
  }
  
  NodePointer demangleGenericRequirement() {
    if (Mangled.nextIf('d')) {
      NodePointer type = demangleType();
      if (!type) return nullptr;
      NodePointer requirement = demangleType();
      if (!requirement) return nullptr;
      auto reqt = NodeFactory::create(
          Node::Kind::DependentGenericConformanceRequirement);
      reqt->addChild(type);
      reqt->addChild(requirement);
      return reqt;
    }
    if (Mangled.nextIf('z')) {
      NodePointer first = demangleType();
      if (!first) return nullptr;
      NodePointer second = demangleType();
      if (!second) return nullptr;
      auto reqt = NodeFactory::create(
          Node::Kind::DependentGenericSameTypeRequirement);
      reqt->addChild(first);
      reqt->addChild(second);
      return reqt;
    }
    // Any other introducer indicates a protocol constraint.
    NodePointer first = demangleType();
    if (!first) return nullptr;
    NodePointer protocol = demangleProtocolName();
    if (!protocol) return nullptr;
    
    auto reqt = NodeFactory::create(
                            Node::Kind::DependentGenericConformanceRequirement);
    reqt->addChild(first);
    reqt->addChild(protocol);
    return reqt;
  }
  
  NodePointer demangleArchetypeType() {
    auto makeSelfType = [&](NodePointer proto) -> NodePointer {
      auto selfType = NodeFactory::create(Node::Kind::SelfTypeRef);
      selfType->addChild(proto);
      Substitutions.push_back(selfType);
      return selfType;
    };
    
    auto makeAssociatedType = [&](NodePointer root) -> NodePointer {
      NodePointer name = demangleIdentifier();
      if (!name) return nullptr;
      auto assocType = NodeFactory::create(Node::Kind::AssociatedTypeRef);
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
      NodePointer decl_ctx = NodeFactory::create(Node::Kind::DeclContext);
      NodePointer ctx = demangleContext();
      if (!ctx)
        return nullptr;
      decl_ctx->addChild(ctx);
      auto qual_atype = NodeFactory::create(Node::Kind::QualifiedArchetype);
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
    NodePointer tuple = NodeFactory::create(
        isV == IsVariadic::yes ? Node::Kind::VariadicTuple
                               : Node::Kind::NonVariadicTuple);
    while (!Mangled.nextIf('_')) {
      if (!Mangled)
        return nullptr;
      NodePointer elt = NodeFactory::create(Node::Kind::TupleElement);

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
    NodePointer out_node = NodeFactory::create(Node::Kind::ReturnType);
    out_node->addChild(out_args);
    return out_node;
  }

  NodePointer demangleType() {
    NodePointer type = demangleTypeImpl();
    if (!type)
      return nullptr;
    NodePointer nodeType = NodeFactory::create(Node::Kind::Type);
    nodeType->addChild(type);
    return nodeType;
  }
  
  NodePointer demangleFunctionType(Node::Kind kind) {
    bool throws = false;
    if (Mangled &&
        Mangled.nextIf('z')) {
      throws = true;
    }
    NodePointer in_args = demangleType();
    if (!in_args)
      return nullptr;
    NodePointer out_args = demangleType();
    if (!out_args)
      return nullptr;
    NodePointer block = NodeFactory::create(kind);
    
    if (throws) {
      block->addChild(NodeFactory::create(Node::Kind::ThrowsAnnotation));
    }
    
    NodePointer in_node = NodeFactory::create(Node::Kind::ArgumentTuple);
    block->addChild(in_node);
    in_node->addChild(in_args);
    block->addChild(postProcessReturnTypeNode(out_args));
    return block;
  }
  
  NodePointer demangleTypeImpl() {
    if (!Mangled)
      return nullptr;
    char c = Mangled.next();
    if (c == 'B') {
      if (!Mangled)
        return nullptr;
      c = Mangled.next();
      if (c == 'b')
        return NodeFactory::create(Node::Kind::BuiltinTypeName,
                                     "Builtin.BridgeObject");
      if (c == 'B')
        return NodeFactory::create(Node::Kind::BuiltinTypeName,
                                     "Builtin.UnsafeValueBuffer");
      if (c == 'f') {
        Node::IndexType size;
        if (demangleBuiltinSize(size)) {
          return NodeFactory::create(
              Node::Kind::BuiltinTypeName,
              (DemanglerPrinter() << "Builtin.Float" << size).str());
        }
      }
      if (c == 'i') {
        Node::IndexType size;
        if (demangleBuiltinSize(size)) {
          return NodeFactory::create(
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
            return NodeFactory::create(
                Node::Kind::BuiltinTypeName,
                (DemanglerPrinter() << "Builtin.Vec" << elts << "xInt" << size)
                    .str());
          }
          if (Mangled.nextIf('f')) {
            Node::IndexType size;
            if (!demangleBuiltinSize(size))
              return nullptr;
            return NodeFactory::create(
                Node::Kind::BuiltinTypeName,
                (DemanglerPrinter() << "Builtin.Vec" << elts << "xFloat"
                                    << size).str());
          }
          if (Mangled.nextIf('p'))
            return NodeFactory::create(
                Node::Kind::BuiltinTypeName,
                (DemanglerPrinter() << "Builtin.Vec" << elts << "xRawPointer")
                    .str());
        }
      }
      if (c == 'O')
        return NodeFactory::create(Node::Kind::BuiltinTypeName,
                                     "Builtin.UnknownObject");
      if (c == 'o')
        return NodeFactory::create(Node::Kind::BuiltinTypeName,
                                     "Builtin.NativeObject");
      if (c == 'p')
        return NodeFactory::create(Node::Kind::BuiltinTypeName,
                                     "Builtin.RawPointer");
      if (c == 'w')
        return NodeFactory::create(Node::Kind::BuiltinTypeName,
                                     "Builtin.Word");
      return nullptr;
    }
    if (c == 'a')
      return demangleDeclarationName(Node::Kind::TypeAlias);

    if (c == 'b') {
      return demangleFunctionType(Node::Kind::ObjCBlock);
    }
    if (c == 'c') {
      return demangleFunctionType(Node::Kind::CFunctionPointer);
    }
    if (c == 'D') {
      NodePointer type = demangleType();
      if (!type)
        return nullptr;

      NodePointer dynamicSelf = NodeFactory::create(Node::Kind::DynamicSelf);
      dynamicSelf->addChild(type);
      return dynamicSelf;
    }
    if (c == 'E') {
      if (!Mangled.nextIf('R'))
        return nullptr;
      if (!Mangled.nextIf('R'))
        return nullptr;
      return NodeFactory::create(Node::Kind::ErrorType, std::string());
    }
    if (c == 'F') {
      return demangleFunctionType(Node::Kind::FunctionType);
    }
    if (c == 'f') {
      return demangleFunctionType(Node::Kind::UncurriedFunctionType);
    }
    if (c == 'G') {
      NodePointer unboundType = demangleType();
      if (!unboundType)
        return nullptr;
      NodePointer type_list = NodeFactory::create(Node::Kind::TypeList);
      while (!Mangled.nextIf('_')) {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        type_list->addChild(type);
        if (Mangled.isEmpty())
          return nullptr;
      }
      Node::Kind bound_type_kind;
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
          return nullptr;
      }
      NodePointer type_application =
          NodeFactory::create(bound_type_kind);
      type_application->addChild(unboundType);
      type_application->addChild(type_list);
      return type_application;
    }
    if (c == 'K') {
      return demangleFunctionType(Node::Kind::AutoClosureType);
    }
    if (c == 'M') {
      NodePointer type = demangleType();
      if (!type)
        return nullptr;
      NodePointer metatype = NodeFactory::create(Node::Kind::Metatype);
      metatype->addChild(type);
      return metatype;
    }
    if (c == 'X') {
      if (Mangled.nextIf('M')) {
        NodePointer metatypeRepr = demangleMetatypeRepresentation();
        if (!metatypeRepr) return nullptr;

        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer metatype = NodeFactory::create(Node::Kind::Metatype);
        metatype->addChild(metatypeRepr);
        metatype->addChild(type);
        return metatype;
      }
    }
    if (c == 'P') {
      if (Mangled.nextIf('M')) {
        NodePointer type = demangleType();
        if (!type) return nullptr;
        auto metatype = NodeFactory::create(Node::Kind::ExistentialMetatype);
        metatype->addChild(type);
        return metatype;
      }

      return demangleProtocolList();
    }

    if (c == 'X') {
      if (Mangled.nextIf('P')) {
        if (Mangled.nextIf('M')) {
          NodePointer metatypeRepr = demangleMetatypeRepresentation();
          if (!metatypeRepr) return nullptr;

          NodePointer type = demangleType();
          if (!type) return nullptr;

          auto metatype = NodeFactory::create(Node::Kind::ExistentialMetatype);
          metatype->addChild(metatypeRepr);
          metatype->addChild(type);
          return metatype;
        }

        return demangleProtocolList();
      }
    }
    if (c == 'Q') {
      return demangleArchetypeType();
    }
    if (c == 'q') {
      return demangleDependentType();
    }
    if (c == 'R') {
      NodePointer inout = NodeFactory::create(Node::Kind::InOut);
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
    if (c == 'u') {
      NodePointer sig = demangleGenericSignature();
      if (!sig) return nullptr;
      NodePointer sub = demangleType();
      if (!sub) return nullptr;
      NodePointer dependentGenericType
        = NodeFactory::create(Node::Kind::DependentGenericType);
      dependentGenericType->addChild(sig);
      dependentGenericType->addChild(sub);
      return dependentGenericType;
    }
    if (c == 'U') {
      GenericContext genericContext(*this);
      NodePointer generics = demangleGenerics(genericContext);
      if (!generics)
        return nullptr;
      NodePointer base = demangleType();
      if (!base)
        return nullptr;
      NodePointer genericType = NodeFactory::create(Node::Kind::GenericType);
      genericType->addChild(generics);
      genericType->addChild(base);
      return genericType;
    }
    if (c == 'X') {
      if (Mangled.nextIf('f')) {
        return demangleFunctionType(Node::Kind::ThinFunctionType);
      }
      if (Mangled.nextIf('o')) {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer unowned = NodeFactory::create(Node::Kind::Unowned);
        unowned->addChild(type);
        return unowned;
      }
      if (Mangled.nextIf('u')) {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer unowned = NodeFactory::create(Node::Kind::Unmanaged);
        unowned->addChild(type);
        return unowned;
      }
      if (Mangled.nextIf('w')) {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer weak = NodeFactory::create(Node::Kind::Weak);
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
    if (Mangled.nextIf('G')) {
      NodePointer generics = demangleGenericSignature();
      if (!generics) return false;
      signature->addChild(std::move(generics));
    }

    NodePointer srcType = demangleType();
    if (!srcType) return false;
    signature->addChild(std::move(srcType));

    NodePointer destType = demangleType();
    if (!destType) return false;
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
    NodePointer type = NodeFactory::create(Node::Kind::ImplFunctionType);

    if (!demangleImplCalleeConvention(type))
      return nullptr;

    if (Mangled.nextIf('C')) {
      if (Mangled.nextIf('b'))
        addImplFunctionAttribute(type, "@convention(block)");
      else if (Mangled.nextIf('c'))
        addImplFunctionAttribute(type, "@convention(c)");
      else if (Mangled.nextIf('m'))
        addImplFunctionAttribute(type, "@convention(method)");
      else if (Mangled.nextIf('O'))
        addImplFunctionAttribute(type, "@convention(objc_method)");
      else if (Mangled.nextIf('w'))
        addImplFunctionAttribute(type, "@convention(witness_method)");
      else
        return nullptr;
    }

    if (Mangled.nextIf('N'))
      addImplFunctionAttribute(type, "@noreturn");

    // Enter a new generic context if this type is generic.
    // FIXME: replace with std::optional, when we have it.
    std::vector<GenericContext> genericContext;
    if (Mangled.nextIf('G')) {
      genericContext.emplace_back(*this);
      NodePointer generics = demangleGenerics(genericContext.front());
      if (!generics)
        return nullptr;
      type->addChild(generics);
    }

    // Expect the attribute terminator.
    if (!Mangled.nextIf('_'))
      return nullptr;

    // Demangle the parameters.
    if (!demangleImplParameters(type))
      return nullptr;

    // Demangle the result type.
    if (!demangleImplResults(type))
      return nullptr;

    return type;
  }

  enum class ImplConventionContext { Callee, Parameter, Result };

  /// impl-convention ::= 'a'                     // direct, autoreleased
  /// impl-convention ::= 'd'                     // direct, no ownership transfer
  /// impl-convention ::= 'D'                     // direct, no ownership transfer,
  ///                                             // dependent on self
  /// impl-convention ::= 'g'                     // direct, guaranteed
  /// impl-convention ::= 'e'                     // direct, deallocating
  /// impl-convention ::= 'i'                     // indirect, ownership transfer
  /// impl-convention ::= 'l'                     // indirect, inout
  /// impl-convention ::= 'o'                     // direct, ownership transfer
  ///
  /// Returns an empty string otherwise.
  StringRef demangleImplConvention(ImplConventionContext ctxt) {
#define CASE(CHAR, FOR_CALLEE, FOR_PARAMETER, FOR_RESULT)            \
    if (Mangled.nextIf(CHAR)) {                                      \
      switch (ctxt) {                                                \
      case ImplConventionContext::Callee: return (FOR_CALLEE);       \
      case ImplConventionContext::Parameter: return (FOR_PARAMETER); \
      case ImplConventionContext::Result: return (FOR_RESULT);       \
      }                                                              \
      unreachable("bad context");                               \
    }
    auto Nothing = StringRef();
    CASE('a',   Nothing,                Nothing,         "@autoreleased")
    CASE('d',   "@callee_unowned",      "@unowned",      "@unowned")
    CASE('d',   Nothing,                Nothing,         "@unowned_inner_pointer")
    CASE('g',   "@callee_guaranteed",   "@guaranteed",   Nothing)
    CASE('e',   Nothing,                "@deallocating", Nothing)
    CASE('i',   Nothing,                "@in",           "@out")
    CASE('l',   Nothing,                "@inout",        Nothing)
    CASE('o',   "@callee_owned",        "@owned",        "@owned")
    return Nothing;
#undef CASE
  }

  // impl-callee-convention ::= 't'
  // impl-callee-convention ::= impl-convention
  bool demangleImplCalleeConvention(NodePointer type) {
    StringRef attr;
    if (Mangled.nextIf('t')) {
      attr = "@convention(thin)";
    } else {
      attr = demangleImplConvention(ImplConventionContext::Callee);
    }
    if (attr.empty()) {
      return false;
    }
    type->addChild(NodeFactory::create(Node::Kind::ImplConvention, attr));
    return true;
  }

  void addImplFunctionAttribute(NodePointer parent, StringRef attr,
                         Node::Kind kind = Node::Kind::ImplFunctionAttribute) {
    parent->addChild(NodeFactory::create(kind, attr));
  }

  // impl-parameter ::= impl-convention type
  bool demangleImplParameters(NodePointer parent) {
    while (!Mangled.nextIf('_')) {
      auto input = demangleImplParameterOrResult(Node::Kind::ImplParameter);
      if (!input) return false;
      parent->addChild(input);
    }
    return true;
  }

  // impl-result ::= impl-convention type
  bool demangleImplResults(NodePointer parent) {
    while (!Mangled.nextIf('_')) {
      auto res = demangleImplParameterOrResult(Node::Kind::ImplResult);
      if (!res) return false;
      parent->addChild(res);
    }
    return true;
  }

  NodePointer demangleImplParameterOrResult(Node::Kind kind) {
    if (Mangled.nextIf('z')) {
      // Only valid for a result.
      if (kind != Node::Kind::ImplResult)
        return nullptr;
      kind = Node::Kind::ImplErrorResult;
    }
  
    auto getContext = [](Node::Kind kind) -> ImplConventionContext {
      if (kind == Node::Kind::ImplParameter)
        return ImplConventionContext::Parameter;
      else if (kind == Node::Kind::ImplResult
               || kind == Node::Kind::ImplErrorResult)
        return ImplConventionContext::Result;
      else
        unreachable("unexpected node kind");
    };

    auto convention = demangleImplConvention(getContext(kind));
    if (convention.empty()) return nullptr;
    auto type = demangleType();
    if (!type) return nullptr;

    NodePointer node = NodeFactory::create(kind);
    node->addChild(NodeFactory::create(Node::Kind::ImplConvention,
                                       convention));
    node->addChild(type);
    
    return node;
  }
};
} // end anonymous namespace

NodePointer
swift::Demangle::demangleSymbolAsNode(const char *MangledName,
                                      size_t MangledNameLength,
                                      const DemangleOptions &Options) {
  Demangler demangler(StringRef(MangledName, MangledNameLength));
  return demangler.demangleTopLevel();
}

NodePointer
swift::Demangle::demangleTypeAsNode(const char *MangledName,
                                    size_t MangledNameLength,
                                    const DemangleOptions &Options) {
  Demangler demangler(StringRef(MangledName, MangledNameLength));
  return demangler.demangleTypeName();
}

namespace {
class NodePrinter {
private:
  DemanglerPrinter Printer;
  DemangleOptions Options;
  
public:
  NodePrinter(DemangleOptions options) : Options(options) {}
  
  std::string printRoot(NodePointer root) {
    print(root);
    return Printer.str();
  }

private:  
  void printChildren(Node::iterator begin,
                     Node::iterator end,
                     const char *sep = nullptr) {
    for (; begin != end;) {
      print(*begin);
      ++begin;
      if (sep && begin != end)
        Printer << sep;
    }
  }
  
  void printChildren(NodePointer pointer, const char *sep = nullptr) {
    if (!pointer)
      return;
    Node::iterator begin = pointer->begin(), end = pointer->end();
    printChildren(begin, end, sep);
  }
  
  NodePointer getFirstChildOfKind(NodePointer pointer, Node::Kind kind) {
    if (!pointer)
      return nullptr;
    for (NodePointer &child : *pointer) {
      if (child && child->getKind() == kind)
        return child;
    }
    return nullptr;
  }

  void printBoundGenericNoSugar(NodePointer pointer) {
    if (pointer->getNumChildren() < 2)
      return;
    NodePointer typelist = pointer->getChild(1);
    print(pointer->getChild(0));
    Printer << "<";
    printChildren(typelist, ", ");
    Printer << ">";
  }

  static bool isSwiftModule(NodePointer node) {
    return (node->getKind() == Node::Kind::Module &&
            node->getText() == STDLIB_NAME);
  }
  
  static bool isDebuggerGeneratedModule(NodePointer node) {
      return (node->getKind() == Node::Kind::Module &&
              0 == node->getText().find(LLDB_EXPRESSIONS_MODULE_NAME_PREFIX));
    }

  static bool isIdentifier(NodePointer node, StringRef desired) {
    return (node->getKind() == Node::Kind::Identifier &&
            node->getText() == desired);
  }
  
  enum class SugarType {
    None,
    Optional,
    ImplicitlyUnwrappedOptional,
    Array,
    Dictionary
  };
  
  /// Determine whether this is a "simple" type, from the type-simple
  /// production.
  bool isSimpleType(NodePointer pointer) {
    switch (pointer->getKind()) {
    case Node::Kind::Archetype:
    case Node::Kind::ArchetypeRef:
    case Node::Kind::AssociatedType:
    case Node::Kind::AssociatedTypeRef:
    case Node::Kind::BoundGenericClass:
    case Node::Kind::BoundGenericEnum:
    case Node::Kind::BoundGenericStructure:
    case Node::Kind::BuiltinTypeName:
    case Node::Kind::Class:
    case Node::Kind::DependentGenericType:
    case Node::Kind::DependentMemberType:
    case Node::Kind::DependentGenericParamType:
    case Node::Kind::DynamicSelf:
    case Node::Kind::Enum:
    case Node::Kind::ErrorType:
    case Node::Kind::ExistentialMetatype:
    case Node::Kind::Metatype:
    case Node::Kind::MetatypeRepresentation:
    case Node::Kind::Module:
    case Node::Kind::NonVariadicTuple:
    case Node::Kind::Protocol:
    case Node::Kind::QualifiedArchetype:
    case Node::Kind::ReturnType:
    case Node::Kind::SelfTypeRef:
    case Node::Kind::Structure:
    case Node::Kind::TupleElementName:
    case Node::Kind::Type:
    case Node::Kind::TypeAlias:
    case Node::Kind::TypeList:
    case Node::Kind::VariadicTuple:
      return true;

    case Node::Kind::Allocator:
    case Node::Kind::ArgumentTuple:
    case Node::Kind::AutoClosureType:
    case Node::Kind::CFunctionPointer:
    case Node::Kind::Constructor:
    case Node::Kind::Deallocator:
    case Node::Kind::DeclContext:
    case Node::Kind::DefaultArgumentInitializer:
    case Node::Kind::DependentGenericSignature:
    case Node::Kind::DependentGenericParamCount:
    case Node::Kind::DependentGenericConformanceRequirement:
    case Node::Kind::DependentGenericSameTypeRequirement:
    case Node::Kind::DependentProtocolWitnessTableGenerator:
    case Node::Kind::DependentProtocolWitnessTableTemplate:
    case Node::Kind::Destructor:
    case Node::Kind::DidSet:
    case Node::Kind::DirectMethodReferenceAttribute:
    case Node::Kind::Directness:
    case Node::Kind::DynamicAttribute:
    case Node::Kind::ExplicitClosure:
    case Node::Kind::Extension:
    case Node::Kind::FieldOffset:
    case Node::Kind::Function:
    case Node::Kind::FunctionSignatureSpecialization:
    case Node::Kind::FunctionSignatureSpecializationParam:
    case Node::Kind::FunctionSignatureSpecializationParamKind:
    case Node::Kind::FunctionSignatureSpecializationParamPayload:
    case Node::Kind::FunctionType:
    case Node::Kind::Generics:
    case Node::Kind::GenericSpecialization:
    case Node::Kind::GenericSpecializationParam:
    case Node::Kind::GenericType:
    case Node::Kind::GenericTypeMetadataPattern:
    case Node::Kind::Getter:
    case Node::Kind::Global:
    case Node::Kind::GlobalGetter:
    case Node::Kind::Identifier:
    case Node::Kind::Index:
    case Node::Kind::IVarInitializer:
    case Node::Kind::IVarDestroyer:
    case Node::Kind::ImplConvention:
    case Node::Kind::ImplFunctionAttribute:
    case Node::Kind::ImplFunctionType:
    case Node::Kind::ImplicitClosure:
    case Node::Kind::ImplParameter:
    case Node::Kind::ImplResult:
    case Node::Kind::ImplErrorResult:
    case Node::Kind::InOut:
    case Node::Kind::InfixOperator:
    case Node::Kind::Initializer:
    case Node::Kind::LazyProtocolWitnessTableAccessor:
    case Node::Kind::LazyProtocolWitnessTableTemplate:
    case Node::Kind::LocalDeclName:
    case Node::Kind::PrivateDeclName:
    case Node::Kind::MaterializeForSet:
    case Node::Kind::Metaclass:
    case Node::Kind::NativeOwningAddressor:
    case Node::Kind::NativeOwningMutableAddressor:
    case Node::Kind::NativePinningAddressor:
    case Node::Kind::NativePinningMutableAddressor:
    case Node::Kind::NominalTypeDescriptor:
    case Node::Kind::NonObjCAttribute:
    case Node::Kind::Number:
    case Node::Kind::ObjCAttribute:
    case Node::Kind::ObjCBlock:
    case Node::Kind::OwningAddressor:
    case Node::Kind::OwningMutableAddressor:
    case Node::Kind::PartialApplyForwarder:
    case Node::Kind::PartialApplyObjCForwarder:
    case Node::Kind::PostfixOperator:
    case Node::Kind::PrefixOperator:
    case Node::Kind::ProtocolConformance:
    case Node::Kind::ProtocolList:
    case Node::Kind::ProtocolWitness:
    case Node::Kind::ProtocolWitnessTable:
    case Node::Kind::ReabstractionThunk:
    case Node::Kind::ReabstractionThunkHelper:
    case Node::Kind::Setter:
    case Node::Kind::SpecializationPassID:
    case Node::Kind::Static:
    case Node::Kind::Subscript:
    case Node::Kind::Suffix:
    case Node::Kind::ThinFunctionType:
    case Node::Kind::TupleElement:
    case Node::Kind::TypeMangling:
    case Node::Kind::TypeMetadata:
    case Node::Kind::TypeMetadataAccessFunction:
    case Node::Kind::TypeMetadataLazyCache:
    case Node::Kind::UncurriedFunctionType:
    case Node::Kind::Unmanaged:
    case Node::Kind::Unowned:
    case Node::Kind::UnsafeAddressor:
    case Node::Kind::UnsafeMutableAddressor:
    case Node::Kind::ValueWitness:
    case Node::Kind::ValueWitnessTable:
    case Node::Kind::Variable:
    case Node::Kind::VTableAttribute:
    case Node::Kind::Weak:
    case Node::Kind::WillSet:
    case Node::Kind::WitnessTableOffset:
    case Node::Kind::ThrowsAnnotation:
      return false;
    }
    unreachable("bad node kind");
  }

  SugarType findSugar(NodePointer pointer) {
    if (pointer->getNumChildren() == 1 && 
        pointer->getKind() == Node::Kind::Type)
      return findSugar(pointer->getChild(0));
    
    if (pointer->getNumChildren() != 2)
      return SugarType::None;
    
    if (pointer->getKind() != Node::Kind::BoundGenericEnum &&
        pointer->getKind() != Node::Kind::BoundGenericStructure)
      return SugarType::None;

    auto unboundType = pointer->getChild(0)->getChild(0); // drill through Type
    auto typeArgs = pointer->getChild(1);
    
    if (pointer->getKind() == Node::Kind::BoundGenericEnum) {
      // Swift.Optional
      if (isIdentifier(unboundType->getChild(1), "Optional") &&
          typeArgs->getNumChildren() == 1 &&
          isSwiftModule(unboundType->getChild(0))) {
        return SugarType::Optional;
      }

      // Swift.ImplicitlyUnwrappedOptional
      if (isIdentifier(unboundType->getChild(1), 
                       "ImplicitlyUnwrappedOptional") &&
          typeArgs->getNumChildren() == 1 &&
          isSwiftModule(unboundType->getChild(0))) {
        return SugarType::ImplicitlyUnwrappedOptional;
      }

      return SugarType::None;
    }

    assert(pointer->getKind() == Node::Kind::BoundGenericStructure);

    // Array
    if (isIdentifier(unboundType->getChild(1), "Array") &&
        typeArgs->getNumChildren() == 1 &&
        isSwiftModule(unboundType->getChild(0))) {
      return SugarType::Array;
    }

    // Dictionary
    if (isIdentifier(unboundType->getChild(1), "Dictionary") &&
        typeArgs->getNumChildren() == 2 &&
        isSwiftModule(unboundType->getChild(0))) {
      return SugarType::Dictionary;
    }

    return SugarType::None;
  }
  
  void printBoundGeneric(NodePointer pointer) {
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
    
    switch (sugarType) {
      case SugarType::None:
        printBoundGenericNoSugar(pointer);
        break;
      case SugarType::Optional:
      case SugarType::ImplicitlyUnwrappedOptional: {
        NodePointer type = pointer->getChild(1)->getChild(0);
        bool needs_parens = !isSimpleType(type);
        if (needs_parens)
          Printer << "(";
        print(type);
        if (needs_parens)
          Printer << ")";
        Printer << (sugarType == SugarType::Optional ? "?" : "!");
        break;
      }
      case SugarType::Array: {
        NodePointer type = pointer->getChild(1)->getChild(0);
        Printer << "[";
        print(type);
        Printer << "]";
        break;
      }
      case SugarType::Dictionary: {
        NodePointer keyType = pointer->getChild(1)->getChild(0);
        NodePointer valueType = pointer->getChild(1)->getChild(1);
        Printer << "[";
        print(keyType);
        Printer << " : ";
        print(valueType);
        Printer << "]";
        break;
      }
    }
  }

  void printSimplifiedEntityType(NodePointer context, NodePointer entityType);

  void printFunctionType(NodePointer node) {
    assert(node->getNumChildren() == 2 || node->getNumChildren() == 3);
    unsigned startIndex = 0;
    bool throws = false;
    if (node->getNumChildren() == 3) {
      assert(node->getChild(0)->getKind() == Node::Kind::ThrowsAnnotation);
      startIndex++;
      throws = true;
    }
    print(node->getChild(startIndex));
    if (throws) Printer << " throws";
    print(node->getChild(startIndex+1));
  }

  void printImplFunctionType(NodePointer fn) {
    enum State { Attrs, Inputs, Results } curState = Attrs;
    auto transitionTo = [&](State newState) {
      assert(newState >= curState);
      for (; curState != newState; curState = State(curState + 1)) {
        switch (curState) {
        case Attrs: Printer << '('; continue;
        case Inputs: Printer << ") -> ("; continue;
        case Results: unreachable("no state after Results");
        }
        unreachable("bad state");
      }
    };

    for (auto &child : *fn) {
      if (child->getKind() == Node::Kind::ImplParameter) {
        if (curState == Inputs) Printer << ", ";
        transitionTo(Inputs);
        print(child);
      } else if (child->getKind() == Node::Kind::ImplResult
                 || child->getKind() == Node::Kind::ImplErrorResult) {
        if (curState == Results) Printer << ", ";
        transitionTo(Results);
        print(child);
      } else {
        assert(curState == Attrs);
        print(child);
        Printer << ' ';
      }
    }
    transitionTo(Results);
    Printer << ')';
  }

  void printContext(NodePointer context) {
    // TODO: parenthesize local contexts?
    if (Options.DisplayDebuggerGeneratedModule ||
       !isDebuggerGeneratedModule(context))
    {
      print(context, /*asContext*/ true);
      if (context->getKind() == Node::Kind::Module && Options.Simplified)
          return;
      Printer << '.';
    }
  }

  void print(NodePointer pointer, bool asContext = false, bool suppressType = false);

  unsigned printFunctionSigSpecializationParam(NodePointer pointer,
                                               unsigned Idx);
};
} // end anonymous namespace

static bool isExistentialType(NodePointer node) {
  assert(node->getKind() == Node::Kind::Type);
  node = node->getChild(0);
  return (node->getKind() == Node::Kind::ExistentialMetatype ||
          node->getKind() == Node::Kind::ProtocolList);
}

/// Print the relevant parameters and return the new index.
unsigned NodePrinter::printFunctionSigSpecializationParam(NodePointer pointer,
                                                          unsigned Idx) {
  NodePointer firstChild = pointer->getChild(Idx);
  unsigned V = firstChild->getIndex();
  auto K = FunctionSigSpecializationParamKind(V);
  switch (K) {
  case FunctionSigSpecializationParamKind::Dead:
  case FunctionSigSpecializationParamKind::InOutToValue:
    print(pointer->getChild(Idx++));
    return Idx;
  case FunctionSigSpecializationParamKind::ConstantPropFunction:
  case FunctionSigSpecializationParamKind::ConstantPropGlobal: {
    Printer << "[";
    print(pointer->getChild(Idx++));
    Printer << " : ";
    const auto &text = pointer->getChild(Idx++)->getText();
    std::string demangledName = demangleSymbolAsString(text);
    if (demangledName.empty()) {
      Printer << text;
    } else {
      Printer << demangledName;
    }
    Printer << "]";
    return Idx;
  }
  case FunctionSigSpecializationParamKind::ConstantPropInteger:
  case FunctionSigSpecializationParamKind::ConstantPropFloat:
    Printer << "[";
    print(pointer->getChild(Idx++));
    Printer << " : ";
    print(pointer->getChild(Idx++));
    Printer << "]";
    return Idx;
  case FunctionSigSpecializationParamKind::ConstantPropString:
    Printer << "[";
    print(pointer->getChild(Idx++));
    Printer << " : ";
    print(pointer->getChild(Idx++));
    Printer << "'";
    print(pointer->getChild(Idx++));
    Printer << "'";
    Printer << "]";
    return Idx;
  case FunctionSigSpecializationParamKind::ClosureProp:
    Printer << "[";
    print(pointer->getChild(Idx++));
    Printer << " : ";
    print(pointer->getChild(Idx++));
    Printer << ", Argument Types : [";
    for (unsigned e = pointer->getNumChildren(); Idx < e;) {
      NodePointer child = pointer->getChild(Idx);
      // Until we no longer have a type node, keep demangling.
      if (child->getKind() != Node::Kind::Type)
        break;
      print(child);
      ++Idx;

      // If we are not done, print the ", ".
      if (Idx < e && pointer->getChild(Idx)->hasText())
        Printer << ", ";
    }
    Printer << "]";
    return Idx;
  default:
    break;
  }

  assert(
      ((V & unsigned(FunctionSigSpecializationParamKind::OwnedToGuaranteed)) ||
       (V & unsigned(FunctionSigSpecializationParamKind::SROA))) &&
      "Invalid OptionSet");
  print(pointer->getChild(Idx++));
  return Idx;
}

static bool isClassType(NodePointer pointer) {
  return pointer->getKind() == Node::Kind::Class;
}

static bool useColonForEntityType(NodePointer entity, NodePointer type) {
  switch (entity->getKind()) {
  case Node::Kind::Variable:
  case Node::Kind::Initializer:
  case Node::Kind::DefaultArgumentInitializer:
  case Node::Kind::IVarInitializer:
  case Node::Kind::Class:
  case Node::Kind::Structure:
  case Node::Kind::Enum:
  case Node::Kind::Protocol:
  case Node::Kind::TypeAlias:
  case Node::Kind::OwningAddressor:
  case Node::Kind::OwningMutableAddressor:
  case Node::Kind::NativeOwningAddressor:
  case Node::Kind::NativeOwningMutableAddressor:
  case Node::Kind::NativePinningAddressor:
  case Node::Kind::NativePinningMutableAddressor:
  case Node::Kind::UnsafeAddressor:
  case Node::Kind::UnsafeMutableAddressor:
  case Node::Kind::GlobalGetter:
  case Node::Kind::Getter:
  case Node::Kind::Setter:
  case Node::Kind::MaterializeForSet:
  case Node::Kind::WillSet:
  case Node::Kind::DidSet:
    return true;

  case Node::Kind::Subscript:
  case Node::Kind::Function:
  case Node::Kind::ExplicitClosure:
  case Node::Kind::ImplicitClosure:
  case Node::Kind::Allocator:
  case Node::Kind::Constructor:
  case Node::Kind::Destructor:
  case Node::Kind::Deallocator:
  case Node::Kind::IVarDestroyer: {
    // We expect to see a function type here, but if we don't, use the colon.
    type = type->getChild(0);
    while (type->getKind() == Node::Kind::GenericType ||
           type->getKind() == Node::Kind::DependentGenericType)
      type = type->getChild(1)->getChild(0);
    return (type->getKind() != Node::Kind::FunctionType &&
            type->getKind() != Node::Kind::UncurriedFunctionType &&
            type->getKind() != Node::Kind::CFunctionPointer &&
            type->getKind() != Node::Kind::ThinFunctionType);
  }

  default:
    unreachable("not an entity");
  }
}

static bool isMethodContext(const NodePointer &context) {
  switch (context->getKind()) {
  case Node::Kind::Structure:
  case Node::Kind::Enum:
  case Node::Kind::Class:
  case Node::Kind::Protocol:
  case Node::Kind::Extension:
    return true;
  default:
    return false;
  }
}

/// Perform any desired type simplifications for an entity in Simplified mode.
void NodePrinter::printSimplifiedEntityType(NodePointer context,
                                            NodePointer entityType) {
  // Only do anything special to methods.
  if (!isMethodContext(context)) return print(entityType);

  // Strip off a single level of uncurried function type.
  NodePointer type = entityType;
  assert(type->getKind() == Node::Kind::Type);
  type = type->getChild(0);

  NodePointer generics;
  if (type->getKind() == Node::Kind::GenericType ||
      type->getKind() == Node::Kind::DependentGenericType) {
    generics = type->getChild(0);
    type = type->getChild(1)->getChild(0);
  }

  if (type->getKind() == Node::Kind::UncurriedFunctionType) {
    if (generics) print(generics);
    print(type->getChild(type->getNumChildren() - 1)->getChild(0));
  } else {
    print(entityType);
  }
}

void NodePrinter::print(NodePointer pointer, bool asContext, bool suppressType) {
  // Common code for handling entities.
  auto printEntity = [&](bool hasName, bool hasType, StringRef extraName) {
    printContext(pointer->getChild(0));

    bool printType = (hasType && !suppressType);
    bool useParens = (printType && asContext);

    if (useParens) Printer << '(';

    if (hasName) print(pointer->getChild(1));
    Printer << extraName;

    if (printType) {
      NodePointer type = pointer->getChild(1 + unsigned(hasName));
      if (useColonForEntityType(pointer, type)) {
        Printer << " : ";
        print(type);
      } else if (Options.Simplified) {
        printSimplifiedEntityType(pointer->getChild(0), type);
      } else {
        Printer << " ";
        print(type);
      }
    }

    if (useParens) Printer << ')';      
  };

  Node::Kind kind = pointer->getKind();
  switch (kind) {
  case Node::Kind::Static:
    Printer << "static ";
    print(pointer->getChild(0), asContext, suppressType);
    return;
  case Node::Kind::Directness:
    Printer << toString(Directness(pointer->getIndex())) << " ";
    return;
  case Node::Kind::Extension:
    assert((pointer->getNumChildren() == 2 || pointer->getNumChildren() == 3)
           && "Extension expects 2 or 3 children.");
    Printer << "ext.";
    // Print the module where extension is defined.
    print(pointer->getChild(0), true);
    Printer << ".";
    print(pointer->getChild(1), asContext);
    if (pointer->getNumChildren() == 3)
      print(pointer->getChild(2), true);
    return;
  case Node::Kind::Variable:
  case Node::Kind::Function:
  case Node::Kind::Subscript:
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
    printEntity(false, false, "(variable initialization expression)");
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
  case Node::Kind::TypeMangling:
    print(pointer->getChild(0));
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
  case Node::Kind::PrivateDeclName:
    Printer << '(';
    print(pointer->getChild(1));
    Printer << " in " << pointer->getChild(0)->getText() << ')';
    return;
  case Node::Kind::Module:
    if (!Options.Simplified)
      Printer << pointer->getText();
    return;
  case Node::Kind::Identifier:
    Printer << pointer->getText();
    return;
  case Node::Kind::Index:
    Printer << pointer->getIndex();
    return;
  case Node::Kind::AutoClosureType:
    Printer << "@autoclosure ";
    printFunctionType(pointer);
    return;
  case Node::Kind::ThinFunctionType:
    Printer << "@convention(thin) ";
    printFunctionType(pointer);
    return;
  case Node::Kind::FunctionType:
    printFunctionType(pointer);
    return;
  case Node::Kind::UncurriedFunctionType:
    print(pointer->getChild(0));
    print(pointer->getChild(1)->getChild(0));
    return;
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
      NodePointer type = pointer->getChild(0);
      print(type);
    } else if (pointer->getNumChildren() == 2) {
      NodePointer id = pointer->getChild(0);
      NodePointer type = pointer->getChild(1);
      print(id);
      print(type);
    }
    return;
  case Node::Kind::TupleElementName:
    Printer << pointer->getText() << " : ";
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
    Printer << "weak ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::Unowned:
    Printer << "unowned ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::Unmanaged:
    Printer << "unowned(unsafe) ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::InOut:
    Printer << "inout ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::NonObjCAttribute:
    Printer << "@!objc ";
    return;
  case Node::Kind::ObjCAttribute:
    Printer << "@objc ";
    return;
  case Node::Kind::DirectMethodReferenceAttribute:
    Printer << "super ";
    return;
  case Node::Kind::DynamicAttribute:
    Printer << "dynamic ";
    return;
  case Node::Kind::VTableAttribute:
    Printer << "override ";
    return;
  case Node::Kind::FunctionSignatureSpecialization:
  case Node::Kind::GenericSpecialization: {
    if (pointer->getKind() == Node::Kind::FunctionSignatureSpecialization) {
      Printer << "function signature specialization <";
    } else {
      Printer << "generic specialization <";
    }
    bool hasPrevious = false;
    // We skip the 0 index since the SpecializationPassID does not contain any
    // information that is useful to our users.
    for (unsigned i = 1, e = pointer->getNumChildren(); i < e; ++i) {
      // Ignore empty specializations.
      if (!pointer->getChild(i)->hasChildren())
        continue;
      if (hasPrevious)
        Printer << ", ";
      print(pointer->getChild(i));
      hasPrevious = true;
    }
    Printer << "> of ";
    return;
  }
  case Node::Kind::GenericSpecializationParam:
    print(pointer->getChild(0));
    for (unsigned i = 1, e = pointer->getNumChildren(); i < e; ++i) {
      if (i == 1)
        Printer << " with ";
      else
        Printer << " and ";
      print(pointer->getChild(i));
    }
    return;
  case Node::Kind::FunctionSignatureSpecializationParam: {
    uint64_t argNum = pointer->getIndex();

    Printer << "Arg[" << argNum << "] = ";

    unsigned Idx = printFunctionSigSpecializationParam(pointer, 0);

    for (unsigned e = pointer->getNumChildren(); Idx < e;) {
      Printer << " and ";
      Idx = printFunctionSigSpecializationParam(pointer, Idx);
    }

    return;
  }
  case Node::Kind::FunctionSignatureSpecializationParamPayload: {
    std::string demangledName = demangleSymbolAsString(pointer->getText());
    if (demangledName.empty()) {
      Printer << pointer->getText();
    } else {
      Printer << demangledName;
    }
    return;
  }
  case Node::Kind::FunctionSignatureSpecializationParamKind: {
    uint64_t raw = pointer->getIndex();

    bool convertedToGuaranteed =
        raw & uint64_t(FunctionSigSpecializationParamKind::OwnedToGuaranteed);
    if (convertedToGuaranteed) {
      Printer << "Owned To Guaranteed";
    }

    if (raw & uint64_t(FunctionSigSpecializationParamKind::SROA)) {
      if (convertedToGuaranteed)
        Printer << " and ";
      Printer << "Exploded";
      return;
    }

    if (convertedToGuaranteed)
      return;

    switch (FunctionSigSpecializationParamKind(raw)) {
    case FunctionSigSpecializationParamKind::Dead:
      Printer << "Dead";
      break;
    case FunctionSigSpecializationParamKind::InOutToValue:
      Printer << "Value Promoted from InOut";
      break;
    case FunctionSigSpecializationParamKind::ConstantPropFunction:
      Printer << "Constant Propagated Function";
      break;
    case FunctionSigSpecializationParamKind::ConstantPropGlobal:
      Printer << "Constant Propagated Global";
      break;
    case FunctionSigSpecializationParamKind::ConstantPropInteger:
      Printer << "Constant Propagated Integer";
      break;
    case FunctionSigSpecializationParamKind::ConstantPropFloat:
      Printer << "Constant Propagated Float";
      break;
    case FunctionSigSpecializationParamKind::ConstantPropString:
      Printer << "Constant Propagated String";
      break;
    case FunctionSigSpecializationParamKind::ClosureProp:
      Printer << "Closure Propagated";
      break;
    case FunctionSigSpecializationParamKind::OwnedToGuaranteed:
    case FunctionSigSpecializationParamKind::SROA:
      unreachable("option sets should have been handled earlier");
    }
    return;
  }
  case Node::Kind::SpecializationPassID:
    Printer << pointer->getIndex();
    return;
  case Node::Kind::BuiltinTypeName:
    Printer << pointer->getText();
    return;
  case Node::Kind::Number:
    Printer << pointer->getIndex();
    return;
  case Node::Kind::InfixOperator:
    Printer << pointer->getText() << " infix";
    return;
  case Node::Kind::PrefixOperator:
    Printer << pointer->getText() << " prefix";
    return;
  case Node::Kind::PostfixOperator:
    Printer << pointer->getText() << " postfix";
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
    print(pointer->getChild(1));
    Printer << " in conformance ";
    print(pointer->getChild(0));
    return;
  }
  case Node::Kind::PartialApplyForwarder:
    Printer << "partial apply forwarder";
    if (pointer->hasChildren()) {
      Printer << " for ";
      print(pointer->getFirstChild());
    }
    return;
  case Node::Kind::PartialApplyObjCForwarder:
    Printer << "partial apply ObjC forwarder";
    if (pointer->hasChildren()) {
      Printer << " for ";
      print(pointer->getFirstChild());
    }
    return;
  case Node::Kind::FieldOffset: {
    print(pointer->getChild(0)); // directness
    Printer << "field offset for ";
    auto entity = pointer->getChild(1);
    print(entity, /*asContext*/ false,
             /*suppressType*/ !Options.DisplayTypeOfIVarFieldOffset);
    return;
  }
  case Node::Kind::ReabstractionThunk:
  case Node::Kind::ReabstractionThunkHelper: {
    Printer << "reabstraction thunk ";
    if (pointer->getKind() == Node::Kind::ReabstractionThunkHelper)
      Printer << "helper ";
    auto generics = getFirstChildOfKind(pointer, Node::Kind::DependentGenericSignature);
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
    print(pointer->getChild(0)); // directness
    Printer << "generic type metadata pattern for ";
    print(pointer->getChild(1));
    return;
  case Node::Kind::Metaclass:
    Printer << "metaclass for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::TypeMetadata:
    print(pointer->getChild(0)); // directness
    Printer << "type metadata for ";
    print(pointer->getChild(1));
    return;
  case Node::Kind::TypeMetadataAccessFunction:
    Printer << "type metadata accessor for ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::TypeMetadataLazyCache:
    Printer << "lazy cache variable for type metadata for ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::NominalTypeDescriptor:
    Printer << "nominal type descriptor for ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::ValueWitness:
    Printer << toString(ValueWitnessKind(pointer->getIndex()))
            << " value witness for ";
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
  case Node::Kind::DynamicSelf:
    Printer << "Self";
    return;
  case Node::Kind::CFunctionPointer: {
    Printer << "@convention(c) ";
    printFunctionType(pointer);
    return;
  }
  case Node::Kind::ObjCBlock: {
    Printer << "@convention(block) ";
    printFunctionType(pointer);
    return;
  }
  case Node::Kind::Metatype: {
    unsigned Idx = 0;
    if (pointer->getNumChildren() == 2) {
      NodePointer repr = pointer->getChild(Idx);
      print(repr);
      Printer << " ";
      Idx++;
    }
    NodePointer type = pointer->getChild(Idx);
    print(type);
    if (isExistentialType(type)) {
      Printer << ".Protocol";
    } else {
      Printer << ".Type";
    }
    return;
  }
  case Node::Kind::ExistentialMetatype: {
    unsigned Idx = 0;
    if (pointer->getNumChildren() == 2) {
      NodePointer repr = pointer->getChild(Idx);
      print(repr);
      Printer << " ";
      Idx++;
    }

    NodePointer type = pointer->getChild(Idx);
    print(type);
    Printer << ".Type";
    return;
  }
  case Node::Kind::MetatypeRepresentation: {
    Printer << pointer->getText();
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
    NodePointer type_list = pointer->getChild(0);
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
    print(pointer->getChild(0));
    for (unsigned i = 1, e = pointer->getNumChildren(); i != e; ++i) {
      auto child = pointer->getChild(i);
      if (child->getKind() != Node::Kind::Archetype) break;
      Printer << ", ";
      print(child);
    }
    Printer << ">";
    return;
  }
  case Node::Kind::Archetype: {
    Printer << pointer->getText();
    if (pointer->hasChildren()) {
      Printer << " : ";
      print(pointer->getChild(0));
    }
    return;
  }
  case Node::Kind::AssociatedType:
    // Don't print for now.
    return;
  case Node::Kind::QualifiedArchetype: {
    if (pointer->getNumChildren() < 2)
      return;
    NodePointer number = pointer->getChild(0);
    NodePointer decl_ctx = pointer->getChild(1);
    Printer << "(archetype " << number->getIndex() << " of ";
    print(decl_ctx);
    Printer << ")";
    return;
  }
  case Node::Kind::GenericType: {
    NodePointer atype_list = pointer->getChild(0);
    NodePointer fct_type = pointer->getChild(1)->getChild(0);
    print(atype_list);
    print(fct_type);
    return;
  }
  case Node::Kind::OwningAddressor:
    printEntity(true, true, ".owningAddressor");
    return;
  case Node::Kind::OwningMutableAddressor:
    printEntity(true, true, ".owningMutableAddressor");
    return;
  case Node::Kind::NativeOwningAddressor:
    printEntity(true, true, ".nativeOwningAddressor");
    return;
  case Node::Kind::NativeOwningMutableAddressor:
    printEntity(true, true, ".nativeOwningMutableAddressor");
    return;
  case Node::Kind::NativePinningAddressor:
    printEntity(true, true, ".nativePinningAddressor");
    return;
  case Node::Kind::NativePinningMutableAddressor:
    printEntity(true, true, ".nativePinningMutableAddressor");
    return;
  case Node::Kind::UnsafeAddressor:
    printEntity(true, true, ".unsafeAddressor");
    return;
  case Node::Kind::UnsafeMutableAddressor:
    printEntity(true, true, ".unsafeMutableAddressor");
    return;
  case Node::Kind::GlobalGetter:
    printEntity(true, true, ".getter");
    return;
  case Node::Kind::Getter:
    printEntity(true, true, ".getter");
    return;
  case Node::Kind::Setter:
    printEntity(true, true, ".setter");
    return;
  case Node::Kind::MaterializeForSet:
    printEntity(true, true, ".materializeForSet");
    return;
  case Node::Kind::WillSet:
    printEntity(true, true, ".willset");
    return;
  case Node::Kind::DidSet:
    printEntity(true, true, ".didset");
    return;
  case Node::Kind::Allocator:
    printEntity(false, true,
                isClassType(pointer->getChild(0))
                  ? "__allocating_init" : "init");
    return;
  case Node::Kind::Constructor:
    printEntity(false, true, "init");
    return;
  case Node::Kind::Destructor:
    printEntity(false, false, "deinit");
    return;
  case Node::Kind::Deallocator:
    printEntity(false, false,
                isClassType(pointer->getChild(0))
                  ? "__deallocating_deinit" : "deinit");
    return;
  case Node::Kind::IVarInitializer:
    printEntity(false, false, "__ivar_initializer");
    return;
  case Node::Kind::IVarDestroyer:
    printEntity(false, false, "__ivar_destroyer");
    return;
  case Node::Kind::ProtocolConformance: {
    NodePointer child0 = pointer->getChild(0);
    NodePointer child1 = pointer->getChild(1);
    NodePointer child2 = pointer->getChild(2);
    print(child0);
    if (!Options.Simplified) {
      Printer << " : ";
      print(child1);
      Printer << " in ";
      print(child2);
    }
    return;
  }
  case Node::Kind::TypeList:
    printChildren(pointer);
    return;
  case Node::Kind::ImplConvention:
    Printer << pointer->getText();
    return;
  case Node::Kind::ImplFunctionAttribute:
    Printer << pointer->getText();
    return;
  case Node::Kind::ImplErrorResult:
    Printer << "@error ";
    SWIFT_FALLTHROUGH;
  case Node::Kind::ImplParameter:
  case Node::Kind::ImplResult:
    printChildren(pointer, " ");
    return;
  case Node::Kind::ImplFunctionType:
    printImplFunctionType(pointer);
    return;
  case Node::Kind::ErrorType:
    Printer << "<ERROR TYPE>";
    return;
      
  case Node::Kind::DependentGenericSignature: {
    Printer << '<';
    
    unsigned paramNumber = 0;
    unsigned depth = 0;
    unsigned numChildren = pointer->getNumChildren();
    for (;
         depth < numChildren
           && pointer->getChild(depth)->getKind()
               == Node::Kind::DependentGenericParamCount;
         ++depth) {
      if (depth != 0)
        Printer << "><";
      
      unsigned count = pointer->getChild(depth)->getIndex();
      for (unsigned index = 0; index < count; ++index) {
        if (index != 0)
          Printer << ", ";
        Printer << archetypeName(paramNumber++);
      }
    }
    
    if (depth != numChildren) {
      Printer << " where ";
      for (unsigned i = depth; i < numChildren; ++i) {
        if (i > depth)
          Printer << ", ";
        print(pointer->getChild(i));
      }
    }
    Printer << '>';
    return;
  }
  case Node::Kind::DependentGenericParamCount:
    unreachable("should be printed as a child of a "
                "DependentGenericSignature");
  case Node::Kind::DependentGenericConformanceRequirement: {
    NodePointer type = pointer->getChild(0);
    NodePointer reqt = pointer->getChild(1);
    print(type);
    Printer << ": ";
    print(reqt);
    return;
  }
  case Node::Kind::DependentGenericSameTypeRequirement: {
    NodePointer fst = pointer->getChild(0);
    NodePointer snd = pointer->getChild(1);
    
    print(fst);
    Printer << " == ";
    print(snd);
    return;
  }
  case Node::Kind::DependentGenericParamType: {
    Printer << pointer->getText();
    return;
  }
  case Node::Kind::DependentGenericType: {
    NodePointer sig = pointer->getChild(0);
    NodePointer depTy = pointer->getChild(1);
    print(sig);
    Printer << ' ';
    print(depTy);
    return;
  }
  case Node::Kind::DependentMemberType: {
    NodePointer base = pointer->getChild(0);
    print(base);
    Printer << '.' << pointer->getText();
    return;
  }
  case Node::Kind::ThrowsAnnotation: {
    Printer<< " throws ";
    return;
  }
  }
  unreachable("bad node kind!");
}

std::string Demangle::nodeToString(NodePointer root,
                                   const DemangleOptions &options) {
  if (!root)
    return "";

  return NodePrinter(options).printRoot(root);
}

std::string Demangle::demangleSymbolAsString(const char *MangledName,
                                             size_t MangledNameLength,
                                             const DemangleOptions &Options) {
  auto mangled = StringRef(MangledName, MangledNameLength);
  auto root = demangleSymbolAsNode(MangledName, MangledNameLength, Options);
  if (!root) return mangled.str();

  std::string demangling = nodeToString(std::move(root), Options);
  if (demangling.empty())
    return mangled.str();
  return demangling;
}

std::string Demangle::demangleTypeAsString(const char *MangledName,
                                           size_t MangledNameLength,
                                           const DemangleOptions &Options) {
  auto mangled = StringRef(MangledName, MangledNameLength);
  auto root = demangleTypeAsNode(MangledName, MangledNameLength, Options);
  if (!root) return mangled.str();
  
  std::string demangling = nodeToString(std::move(root), Options);
  if (demangling.empty())
    return mangled.str();
  return demangling;
}

