//===--- Demangle.cpp - Swift Name Demangling -----------------------------===//
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
//  This file implements declaration name demangling in Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Demangle.h"
#include "swift/Basic/Demangler.h"
#include "swift/Basic/ManglingMacros.h"
#include "swift/Strings.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Punycode.h"
#include "swift/Basic/UUID.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Compiler.h"
#include <functional>
#include <vector>
#include <cstdio>
#include <cstdlib>

using namespace swift;
using namespace Demangle;

[[noreturn]]
static void unreachable(const char *Message) {
  fprintf(stderr, "fatal error: %s\n", Message);
  std::abort();
}

DemanglerPrinter &DemanglerPrinter::operator<<(unsigned long long n) & {
  char buffer[32];
  snprintf(buffer, sizeof(buffer), "%llu", n);
  Stream.append(buffer);
  return *this;
}
DemanglerPrinter &DemanglerPrinter::operator<<(long long n) & {
  char buffer[32];
  snprintf(buffer, sizeof(buffer), "%lld",n);
  Stream.append(buffer);
  return *this;
}

namespace {
struct QuotedString {
  std::string Value;

  explicit QuotedString(std::string Value) : Value(Value) {}
};
} // end anonymous namespace

static DemanglerPrinter &operator<<(DemanglerPrinter &printer,
                                    const QuotedString &QS) {
  printer << '"';
  for (auto C : QS.Value) {
    switch (C) {
    case '\\': printer << "\\\\"; break;
    case '\t': printer << "\\t"; break;
    case '\n': printer << "\\n"; break;
    case '\r': printer << "\\r"; break;
    case '"': printer << "\\\""; break;
    case '\'': printer << '\''; break; // no need to escape these
    case '\0': printer << "\\0"; break;
    default:
      auto c = static_cast<char>(C);
      // Other ASCII control characters should get escaped.
      if (c < 0x20 || c == 0x7F) {
        static const char Hexdigit[] = {
          '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
          'A', 'B', 'C', 'D', 'E', 'F'
        };
        printer << "\\x" << Hexdigit[c >> 4] << Hexdigit[c & 0xF];
      } else {
        printer << c;
      }
      break;
    }
  }
  printer << '"';
  return printer;
}

namespace {
  struct FindPtr {
    FindPtr(Node *v) : Target(v) {}
    bool operator()(NodePointer sp) const {
      return sp == Target;
    }
  private:
    Node *Target;
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

static std::string archetypeName(Node::IndexType index,
                                 Node::IndexType depth) {
  DemanglerPrinter name;
  do {
    name << (char)('A' + (index % 26));
    index /= 26;
  } while (index);
  if (depth != 0)
    name << depth;
  return std::move(name).str();
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
  case ValueWitnessKind::DestructiveProjectEnumData:
    return "destructiveProjectEnumData";
  case ValueWitnessKind::DestructiveInjectEnumTag:
    return "destructiveInjectEnumTag";
  }
  unreachable("bad value witness kind");
}

/// The main class for parsing a demangling tree out of a mangled string.
class OldDemangler {
  std::vector<NodePointer> Substitutions;
  NameSource Mangled;
  NodeFactory &Factory;
public:  
  OldDemangler(llvm::StringRef mangled, NodeFactory &Factory)
    : Mangled(mangled), Factory(Factory) {}

/// Try to demangle a child node of the given kind.  If that fails,
/// return; otherwise add it to the parent.
#define DEMANGLE_CHILD_OR_RETURN(PARENT, CHILD_KIND) do { \
    auto _node = demangle##CHILD_KIND();                  \
    if (!_node) return nullptr;                           \
    addChild(PARENT, _node);                              \
  } while (false)

/// Try to demangle a child node of the given kind.  If that fails,
/// return; otherwise add it to the parent.
#define DEMANGLE_CHILD_AS_NODE_OR_RETURN(PARENT, CHILD_KIND) do {  \
    auto _kind = demangle##CHILD_KIND();                           \
    if (!_kind.hasValue()) return nullptr;                         \
    addChild(PARENT, Factory.createNode(Node::Kind::CHILD_KIND,        \
                                           unsigned(*_kind)));     \
  } while (false)

  /// Attempt to demangle the source string.  The root node will
  /// always be a Global.  Extra characters at the end will be
  /// tolerated (and included as a Suffix node as a child of the
  /// Global).
  ///
  /// \return true if the mangling succeeded
  NodePointer demangleTopLevel() {
    if (!Mangled.nextIf("_T"))
      return nullptr;

    NodePointer topLevel = Factory.createNode(Node::Kind::Global);

    // First demangle any specialization prefixes.
    if (Mangled.nextIf("TS")) {
      do {
        DEMANGLE_CHILD_OR_RETURN(topLevel, SpecializedAttribute);

        // The Substitution header does not share state with the rest
        // of the mangling.
        Substitutions.clear();
      } while (Mangled.nextIf("_TTS"));

      // Then check that we have a global.
      if (!Mangled.nextIf("_T"))
        return nullptr;

    } else if (Mangled.nextIf("To")) {
      addChild(topLevel, Factory.createNode(Node::Kind::ObjCAttribute));
    } else if (Mangled.nextIf("TO")) {
      addChild(topLevel, Factory.createNode(Node::Kind::NonObjCAttribute));
    } else if (Mangled.nextIf("TD")) {
      addChild(topLevel, Factory.createNode(Node::Kind::DynamicAttribute));
    } else if (Mangled.nextIf("Td")) {
      addChild(topLevel, Factory.createNode(
                          Node::Kind::DirectMethodReferenceAttribute));
    } else if (Mangled.nextIf("TV")) {
      addChild(topLevel, Factory.createNode(Node::Kind::VTableAttribute));
    }

    DEMANGLE_CHILD_OR_RETURN(topLevel, Global);

    // Add a suffix node if there's anything left unmangled.
    if (!Mangled.isEmpty()) {
      addChild(topLevel, Factory.createNode(Node::Kind::Suffix,
                                        Mangled.getString()));
    }

    return topLevel;
  }

  NodePointer demangleTypeName() {
    return demangleType();
  }
  
private:
  enum class IsVariadic {
    yes = true, no = false
  };

  void addChild(NodePointer Parent, NodePointer Child) {
    Parent->addChild(Child, Factory);
  }
  
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
    char Code[2];
    if (!Mangled)
      return None;
    Code[0] = Mangled.next();
    if (!Mangled)
      return None;
    Code[1] = Mangled.next();

    StringRef CodeStr(Code, 2);
#define VALUE_WITNESS(MANGLING, NAME) \
  if (CodeStr == #MANGLING) return ValueWitnessKind::NAME;
#include "swift/Basic/ValueWitnessMangling.def"

    return None;
  }

  NodePointer demangleGlobal() {
    if (!Mangled)
      return nullptr;

    // Type metadata.
    if (Mangled.nextIf('M')) {
      if (Mangled.nextIf('P')) {
        auto pattern =
            Factory.createNode(Node::Kind::GenericTypeMetadataPattern);
        DEMANGLE_CHILD_OR_RETURN(pattern, Type);
        return pattern;
      }
      if (Mangled.nextIf('a')) {
        auto accessor =
          Factory.createNode(Node::Kind::TypeMetadataAccessFunction);
        DEMANGLE_CHILD_OR_RETURN(accessor, Type);
        return accessor;
      }
      if (Mangled.nextIf('L')) {
        auto cache = Factory.createNode(Node::Kind::TypeMetadataLazyCache);
        DEMANGLE_CHILD_OR_RETURN(cache, Type);
        return cache;
      }
      if (Mangled.nextIf('m')) {
        auto metaclass = Factory.createNode(Node::Kind::Metaclass);
        DEMANGLE_CHILD_OR_RETURN(metaclass, Type);
        return metaclass;
      }
      if (Mangled.nextIf('n')) {
        auto nominalType =
            Factory.createNode(Node::Kind::NominalTypeDescriptor);
        DEMANGLE_CHILD_OR_RETURN(nominalType, Type);
        return nominalType;
      }
      if (Mangled.nextIf('f')) {
        auto metadata = Factory.createNode(Node::Kind::FullTypeMetadata);
        DEMANGLE_CHILD_OR_RETURN(metadata, Type);
        return metadata;
      }
      if (Mangled.nextIf('p')) {
        auto metadata = Factory.createNode(Node::Kind::ProtocolDescriptor);
        DEMANGLE_CHILD_OR_RETURN(metadata, ProtocolName);
        return metadata;
      }
      auto metadata = Factory.createNode(Node::Kind::TypeMetadata);
      DEMANGLE_CHILD_OR_RETURN(metadata, Type);
      return metadata;
    }

    // Partial application thunks.
    if (Mangled.nextIf("PA")) {
      Node::Kind kind = Node::Kind::PartialApplyForwarder;
      if (Mangled.nextIf('o'))
        kind = Node::Kind::PartialApplyObjCForwarder;
      auto forwarder = Factory.createNode(kind);
      if (Mangled.nextIf("__T"))
        DEMANGLE_CHILD_OR_RETURN(forwarder, Global);
      return forwarder;
    }

    // Top-level types, for various consumers.
    if (Mangled.nextIf('t')) {
      auto type = Factory.createNode(Node::Kind::TypeMangling);
      DEMANGLE_CHILD_OR_RETURN(type, Type);
      return type;
    }

    // Value witnesses.
    if (Mangled.nextIf('w')) {
      Optional<ValueWitnessKind> w = demangleValueWitnessKind();
      if (!w.hasValue())
        return nullptr;
      auto witness =
        Factory.createNode(Node::Kind::ValueWitness, unsigned(w.getValue()));
      DEMANGLE_CHILD_OR_RETURN(witness, Type);
      return witness;
    }

    // Offsets, value witness tables, and protocol witnesses.
    if (Mangled.nextIf('W')) {
      if (Mangled.nextIf('V')) {
        auto witnessTable = Factory.createNode(Node::Kind::ValueWitnessTable);
        DEMANGLE_CHILD_OR_RETURN(witnessTable, Type);
        return witnessTable;
      }
      if (Mangled.nextIf('o')) {
        auto witnessTableOffset =
            Factory.createNode(Node::Kind::WitnessTableOffset);
        DEMANGLE_CHILD_OR_RETURN(witnessTableOffset, Entity);
        return witnessTableOffset;
      }
      if (Mangled.nextIf('v')) {
        auto fieldOffset = Factory.createNode(Node::Kind::FieldOffset);
        DEMANGLE_CHILD_AS_NODE_OR_RETURN(fieldOffset, Directness);
        DEMANGLE_CHILD_OR_RETURN(fieldOffset, Entity);
        return fieldOffset;
      }
      if (Mangled.nextIf('P')) {
        auto witnessTable =
            Factory.createNode(Node::Kind::ProtocolWitnessTable);
        DEMANGLE_CHILD_OR_RETURN(witnessTable, ProtocolConformance);
        return witnessTable;
      }
      if (Mangled.nextIf('G')) {
        auto witnessTable =
            Factory.createNode(Node::Kind::GenericProtocolWitnessTable);
        DEMANGLE_CHILD_OR_RETURN(witnessTable, ProtocolConformance);
        return witnessTable;
      }
      if (Mangled.nextIf('I')) {
        auto witnessTable = Factory.createNode(
            Node::Kind::GenericProtocolWitnessTableInstantiationFunction);
        DEMANGLE_CHILD_OR_RETURN(witnessTable, ProtocolConformance);
        return witnessTable;
      }
      if (Mangled.nextIf('l')) {
        auto accessor =
          Factory.createNode(Node::Kind::LazyProtocolWitnessTableAccessor);
        DEMANGLE_CHILD_OR_RETURN(accessor, Type);
        DEMANGLE_CHILD_OR_RETURN(accessor, ProtocolConformance);
        return accessor;
      }
      if (Mangled.nextIf('L')) {
        auto accessor =
          Factory.createNode(Node::Kind::LazyProtocolWitnessTableCacheVariable);
        DEMANGLE_CHILD_OR_RETURN(accessor, Type);
        DEMANGLE_CHILD_OR_RETURN(accessor, ProtocolConformance);
        return accessor;
      }
      if (Mangled.nextIf('a')) {
        auto tableTemplate =
          Factory.createNode(Node::Kind::ProtocolWitnessTableAccessor);
        DEMANGLE_CHILD_OR_RETURN(tableTemplate, ProtocolConformance);
        return tableTemplate;
      }
      if (Mangled.nextIf('t')) {
        auto accessor = Factory.createNode(
            Node::Kind::AssociatedTypeMetadataAccessor);
        DEMANGLE_CHILD_OR_RETURN(accessor, ProtocolConformance);
        DEMANGLE_CHILD_OR_RETURN(accessor, DeclName);
        return accessor;
      }
      if (Mangled.nextIf('T')) {
        auto accessor = Factory.createNode(
            Node::Kind::AssociatedTypeWitnessTableAccessor);
        DEMANGLE_CHILD_OR_RETURN(accessor, ProtocolConformance);
        DEMANGLE_CHILD_OR_RETURN(accessor, DeclName);
        DEMANGLE_CHILD_OR_RETURN(accessor, ProtocolName);
        return accessor;
      }
      return nullptr;
    }

    // Other thunks.
    if (Mangled.nextIf('T')) {
      if (Mangled.nextIf('R')) {
        auto thunk = Factory.createNode(Node::Kind::ReabstractionThunkHelper);
        if (!demangleReabstractSignature(thunk))
          return nullptr;
        return thunk;
      }
      if (Mangled.nextIf('r')) {
        auto thunk = Factory.createNode(Node::Kind::ReabstractionThunk);
        if (!demangleReabstractSignature(thunk))
          return nullptr;
        return thunk;
      }
      if (Mangled.nextIf('W')) {
        NodePointer thunk = Factory.createNode(Node::Kind::ProtocolWitness);
        DEMANGLE_CHILD_OR_RETURN(thunk, ProtocolConformance);
        // The entity is mangled in its own generic context.
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
      NodePointer param = Factory.createNode(Node::Kind::GenericSpecializationParam);
      DEMANGLE_CHILD_OR_RETURN(param, Type);

      // Then parse any conformances until we find an underscore. Pop off the
      // underscore since it serves as the end of our mangling list.
      while (!Mangled.nextIf('_')) {
        DEMANGLE_CHILD_OR_RETURN(param, ProtocolConformance);
      }

      // Add the parameter to our specialization list.
      specialization->addChild(param, Factory);
    }

    return specialization;
  }

/// TODO: This is an atrocity. Come up with a shorter name.
#define FUNCSIGSPEC_CREATE_PARAM_KIND(kind)                                    \
  Factory.createNode(                                                          \
      Node::Kind::FunctionSignatureSpecializationParamKind,                    \
      Node::IndexType(FunctionSigSpecializationParamKind::kind))

#define FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(payload)                              \
  Factory.createNode(Node::Kind::FunctionSignatureSpecializationParamPayload,  \
                     payload)

  bool demangleFuncSigSpecializationConstantProp(NodePointer parent) {
    // Then figure out what was actually constant propagated. First check if
    // we have a function.
    if (Mangled.nextIf("fr")) {
      // Demangle the identifier
      NodePointer name = demangleIdentifier();
      if (!name || !Mangled.nextIf('_'))
        return false;
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_KIND(ConstantPropFunction), Factory);
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(name->getText()), Factory);
      return true;
    }

    if (Mangled.nextIf('g')) {
      NodePointer name = demangleIdentifier();
      if (!name || !Mangled.nextIf('_'))
        return false;
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_KIND(ConstantPropGlobal), Factory);
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(name->getText()), Factory);
      return true;
    }

    if (Mangled.nextIf('i')) {
      std::string Str;
      if (!Mangled.readUntil('_', Str) || !Mangled.nextIf('_'))
        return false;
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_KIND(ConstantPropInteger), Factory);
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(Str), Factory);
      return true;
    }

    if (Mangled.nextIf("fl")) {
      std::string Str;
      if (!Mangled.readUntil('_', Str) || !Mangled.nextIf('_'))
        return false;
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_KIND(ConstantPropFloat), Factory);
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(Str), Factory);
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

      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_KIND(ConstantPropString), Factory);
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(encodingStr), Factory);
      parent->addChild(FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(str->getText()), Factory);
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

    parent->addChild(FUNCSIGSPEC_CREATE_PARAM_KIND(ClosureProp), Factory);
    parent->addChild(FUNCSIGSPEC_CREATE_PARAM_PAYLOAD(name->getText()), Factory);

    // Then demangle types until we fail.
    NodePointer type = nullptr;
    while (Mangled.peek() != '_' && (type = demangleType())) {
      parent->addChild(type, Factory);
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
        Factory.createNode(Node::Kind::FunctionSignatureSpecializationParam,
                            paramCount);

      // First handle options.
      if (Mangled.nextIf("n_")) {
        // Leave the parameter empty.
      } else if (Mangled.nextIf("cp")) {
        if (!demangleFuncSigSpecializationConstantProp(param))
          return nullptr;
      } else if (Mangled.nextIf("cl")) {
        if (!demangleFuncSigSpecializationClosureProp(param))
          return nullptr;
      } else if (Mangled.nextIf("i_")) {
        auto result = FUNCSIGSPEC_CREATE_PARAM_KIND(BoxToValue);
        if (!result)
          return nullptr;
        param->addChild(result, Factory);
      } else if (Mangled.nextIf("k_")) {
        auto result = FUNCSIGSPEC_CREATE_PARAM_KIND(BoxToStack);
        if (!result)
          return nullptr;
        param->addChild(result, Factory);
      } else {
        // Otherwise handle option sets.
        unsigned Value = 0;
        if (Mangled.nextIf('d')) {
          Value |=
            unsigned(FunctionSigSpecializationParamKind::Dead);
        }

        if (Mangled.nextIf('g')) {
          Value |=
              unsigned(FunctionSigSpecializationParamKind::OwnedToGuaranteed);
        }

        if (Mangled.nextIf('s')) {
          Value |= unsigned(FunctionSigSpecializationParamKind::SROA);
        }

        if (!Mangled.nextIf('_'))
          return nullptr;

        if (!Value)
          return nullptr;

        auto result = Factory.createNode(
            Node::Kind::FunctionSignatureSpecializationParamKind, Value);
        if (!result)
          return nullptr;
        param->addChild(result, Factory);
      }

      specialization->addChild(param, Factory);
      paramCount++;
    }

    return specialization;
  }

#undef FUNCSIGSPEC_CREATE_PARAM_KIND
#undef FUNCSIGSPEC_CREATE_PARAM_PAYLOAD

  NodePointer demangleSpecializedAttribute() {
    bool isNotReAbstracted = false;
    if (Mangled.nextIf("g") || (isNotReAbstracted = Mangled.nextIf("r"))) {
      auto spec = Factory.createNode(isNotReAbstracted ?
                              Node::Kind::GenericSpecializationNotReAbstracted :
                              Node::Kind::GenericSpecialization);

      // Create a node if the specialization is externally inlineable.
      if (Mangled.nextIf("q")) {
        auto kind = Node::Kind::SpecializationIsFragile;
        spec->addChild(Factory.createNode(kind), Factory);
      }

      // Create a node for the pass id.
      spec->addChild(Factory.createNode(Node::Kind::SpecializationPassID,
                                      unsigned(Mangled.next() - 48)), Factory);

      // And then mangle the generic specialization.
      return demangleGenericSpecialization(spec);
    }
    if (Mangled.nextIf("f")) {
      auto spec =
          Factory.createNode(Node::Kind::FunctionSignatureSpecialization);

      // Create a node if the specialization is externally inlineable.
      if (Mangled.nextIf("q")) {
        auto kind = Node::Kind::SpecializationIsFragile;
        spec->addChild(Factory.createNode(kind), Factory);
      }

      // Add the pass id.
      spec->addChild(Factory.createNode(Node::Kind::SpecializationPassID,
                                      unsigned(Mangled.next() - 48)), Factory);

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

      NodePointer localName = Factory.createNode(Node::Kind::LocalDeclName);
      localName->addChild(discriminator, Factory);
      localName->addChild(name, Factory);
      return localName;

    } else if (Mangled.nextIf('P')) {
      NodePointer discriminator = demangleIdentifier();
      if (!discriminator) return nullptr;

      NodePointer name = demangleIdentifier();
      if (!name) return nullptr;

      auto privateName = Factory.createNode(Node::Kind::PrivateDeclName);
      privateName->addChild(discriminator, Factory);
      privateName->addChild(name, Factory);
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
    
    return Factory.createNode(*kind, identifier);
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
    return Factory.createNode(kind, index);
  }

  NodePointer createSwiftType(Node::Kind typeKind, StringRef name) {
    NodePointer type = Factory.createNode(typeKind);
    type->addChild(Factory.createNode(Node::Kind::Module, STDLIB_NAME), Factory);
    type->addChild(Factory.createNode(Node::Kind::Identifier, name), Factory);
    return type;
  }

  /// Demangle a <substitution>, given that we've already consumed the 'S'.
  NodePointer demangleSubstitutionIndex() {
    if (!Mangled)
      return nullptr;
    if (Mangled.nextIf('o'))
      return Factory.createNode(Node::Kind::Module, MANGLING_MODULE_OBJC);
    if (Mangled.nextIf('C'))
      return Factory.createNode(Node::Kind::Module, MANGLING_MODULE_C);
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
    if (Mangled.nextIf('V'))
      return createSwiftType(Node::Kind::Structure, "UnsafeRawPointer");
    if (Mangled.nextIf('v'))
      return createSwiftType(Node::Kind::Structure, "UnsafeMutableRawPointer");
    if (Mangled.nextIf('P'))
      return createSwiftType(Node::Kind::Structure, "UnsafePointer");
    if (Mangled.nextIf('p'))
      return createSwiftType(Node::Kind::Structure, "UnsafeMutablePointer");
    if (Mangled.nextIf('q'))
      return createSwiftType(Node::Kind::Enum, "Optional");
    if (Mangled.nextIf('Q'))
      return createSwiftType(Node::Kind::Enum, "ImplicitlyUnwrappedOptional");
    if (Mangled.nextIf('R'))
      return createSwiftType(Node::Kind::Structure, "UnsafeBufferPointer");
    if (Mangled.nextIf('r'))
      return createSwiftType(Node::Kind::Structure, "UnsafeMutableBufferPointer");
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
    if (Mangled.nextIf('s')) {
      return Factory.createNode(Node::Kind::Module, STDLIB_NAME);
    }
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

    auto decl = Factory.createNode(kind);
    decl->addChild(context, Factory);
    decl->addChild(name, Factory);
    Substitutions.push_back(decl);
    return decl;
  }

  NodePointer demangleProtocolName() {
    NodePointer proto = demangleProtocolNameImpl();
    if (!proto) return nullptr;

    NodePointer type = Factory.createNode(Node::Kind::Type);
    type->addChild(proto, Factory);
    return type;
  }

  NodePointer demangleProtocolNameGivenContext(NodePointer context) {
    NodePointer name = demangleDeclName();
    if (!name) return nullptr;

    auto proto = Factory.createNode(Node::Kind::Protocol);
    proto->addChild(context, Factory);
    proto->addChild(name, Factory);
    Substitutions.push_back(proto);
    return proto;
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

      return demangleProtocolNameGivenContext(sub);
    }

    if (Mangled.nextIf('s')) {
      NodePointer stdlib = Factory.createNode(Node::Kind::Module, STDLIB_NAME);

      return demangleProtocolNameGivenContext(stdlib);
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

  NodePointer demangleBoundGenericArgs(NodePointer nominalType) {
    if (nominalType->getNumChildren() == 0)
      return nullptr;

    // Generic arguments for the outermost type come first.
    NodePointer parentOrModule = nominalType->getChild(0);

    if (parentOrModule->getKind() != Node::Kind::Module &&
        parentOrModule->getKind() != Node::Kind::Function &&
        parentOrModule->getKind() != Node::Kind::Extension) {
      parentOrModule = demangleBoundGenericArgs(parentOrModule);

      // Rebuild this type with the new parent type, which may have
      // had its generic arguments applied.
      NodePointer result = Factory.createNode(nominalType->getKind());
      result->addChild(parentOrModule, Factory);
      result->addChild(nominalType->getChild(1), Factory);

      nominalType = result;
    }

    NodePointer args = Factory.createNode(Node::Kind::TypeList);
    while (!Mangled.nextIf('_')) {
      NodePointer type = demangleType();
      if (!type)
        return nullptr;
      args->addChild(type, Factory);
      if (Mangled.isEmpty())
        return nullptr;
    }

    // If there were no arguments at this level there is nothing left
    // to do.
    if (args->getNumChildren() == 0)
      return nominalType;

    // Otherwise, build a bound generic type node from the unbound
    // type and arguments.
    NodePointer unboundType = Factory.createNode(Node::Kind::Type);
    unboundType->addChild(nominalType, Factory);

    Node::Kind kind;
    switch (nominalType->getKind()) { // look through Type node
      case Node::Kind::Class:
        kind = Node::Kind::BoundGenericClass;
        break;
      case Node::Kind::Structure:
        kind = Node::Kind::BoundGenericStructure;
        break;
      case Node::Kind::Enum:
        kind = Node::Kind::BoundGenericEnum;
        break;
      default:
        return nullptr;
    }
    NodePointer result = Factory.createNode(kind);
    result->addChild(unboundType, Factory);
    result->addChild(args, Factory);
    return result;
  }

  NodePointer demangleBoundGenericType() {
    // bound-generic-type ::= 'G' nominal-type (args+ '_')+
    //
    // Each level of nominal type nesting has its own list of arguments.

    NodePointer nominalType = demangleNominalType();
    if (!nominalType)
      return nullptr;

    return demangleBoundGenericArgs(nominalType);
  }

  NodePointer demangleContext() {
    // context ::= module
    // context ::= entity
    // context ::= 'E' module context (extension defined in a different module)
    // context ::= 'e' module context generic-signature (constrained extension)
    if (!Mangled) return nullptr;
    if (Mangled.nextIf('E')) {
      NodePointer ext = Factory.createNode(Node::Kind::Extension);
      NodePointer def_module = demangleModule();
      if (!def_module) return nullptr;
      NodePointer type = demangleContext();
      if (!type) return nullptr;
      ext->addChild(def_module, Factory);
      ext->addChild(type, Factory);
      return ext;
    }
    if (Mangled.nextIf('e')) {
      NodePointer ext = Factory.createNode(Node::Kind::Extension);
      NodePointer def_module = demangleModule();
      if (!def_module) return nullptr;
      NodePointer sig = demangleGenericSignature();
      // The generic context is currently re-specified by the type mangling.
      // If we ever remove 'self' from manglings, we should stop resetting the
      // context here.
      if (!sig) return nullptr;
      NodePointer type = demangleContext();
      if (!type) return nullptr;

      ext->addChild(def_module, Factory);
      ext->addChild(type, Factory);
      ext->addChild(sig, Factory);
      return ext;
    }
    if (Mangled.nextIf('S'))
      return demangleSubstitutionIndex();
    if (Mangled.nextIf('s'))
      return Factory.createNode(Node::Kind::Module, STDLIB_NAME);
    if (Mangled.nextIf('G'))
      return demangleBoundGenericType();
    if (isStartOfEntity(Mangled.peek()))
      return demangleEntity();
    return demangleModule();
  }
  
  NodePointer demangleProtocolList() {
    NodePointer proto_list = Factory.createNode(Node::Kind::ProtocolList);
    NodePointer type_list = Factory.createNode(Node::Kind::TypeList);
    proto_list->addChild(type_list, Factory);
    while (!Mangled.nextIf('_')) {
      NodePointer proto = demangleProtocolName();
      if (!proto)
        return nullptr;
      type_list->addChild(proto, Factory);
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
        Factory.createNode(Node::Kind::ProtocolConformance);
    proto_conformance->addChild(type, Factory);
    proto_conformance->addChild(protocol, Factory);
    proto_conformance->addChild(context, Factory);
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
    } else if (Mangled.nextIf('i')) {
      entityBasicKind = Node::Kind::Subscript;
    } else {
      return demangleNominalType();
    }

    NodePointer context = demangleContext();
    if (!context) return nullptr;

    // entity-name
    Node::Kind entityKind;
    bool hasType = true;
    NodePointer name = nullptr;
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

    NodePointer entity = Factory.createNode(entityKind);
    entity->addChild(context, Factory);

    if (name) entity->addChild(name, Factory);

    if (hasType) {
      auto type = demangleType();
      if (!type) return nullptr;
      entity->addChild(type, Factory);
    }
    
    if (isStatic) {
      auto staticNode = Factory.createNode(Node::Kind::Static);
      staticNode->addChild(entity, Factory);
      return staticNode;
    }

    return entity;
  }

  NodePointer getDependentGenericParamType(unsigned depth, unsigned index) {
    DemanglerPrinter PrintName;
    PrintName << archetypeName(index, depth);

    auto paramTy = Factory.createNode(Node::Kind::DependentGenericParamType,
                                       std::move(PrintName).str());
    paramTy->addChild(Factory.createNode(Node::Kind::Index, depth), Factory);
    paramTy->addChild(Factory.createNode(Node::Kind::Index, index), Factory);

    return paramTy;
  }

  NodePointer demangleGenericParamIndex() {
    Node::IndexType depth, index;

    if (Mangled.nextIf('d')) {
      if (!demangleIndex(depth))
        return nullptr;
      depth += 1;
      if (!demangleIndex(index))
        return nullptr;
    } else if (Mangled.nextIf('x')) {
      depth = 0;
      index = 0;
    } else {
      if (!demangleIndex(index))
        return nullptr;
      depth = 0;
      index += 1;
    }
    return getDependentGenericParamType(depth, index);
  }

  NodePointer demangleDependentMemberTypeName(NodePointer base) {
    assert(base->getKind() == Node::Kind::Type
           && "base should be a type");
    NodePointer assocTy = nullptr;

    if (Mangled.nextIf('S')) {
      assocTy = demangleSubstitutionIndex();
      if (!assocTy)
        return nullptr;
      if (assocTy->getKind() != Node::Kind::DependentAssociatedTypeRef)
        return nullptr;
    } else {
      NodePointer protocol = nullptr;
      if (Mangled.nextIf('P')) {
        protocol = demangleProtocolName();
        if (!protocol) return nullptr;
      }

      // TODO: If the protocol name was elided from the assoc type mangling,
      // we could try to fish it out of the generic signature constraints on the
      // base.
      assocTy = demangleIdentifier(Node::Kind::DependentAssociatedTypeRef);
      if (!assocTy) return nullptr;
      if (protocol)
        assocTy->addChild(protocol, Factory);

      Substitutions.push_back(assocTy);
    }

    NodePointer depTy = Factory.createNode(Node::Kind::DependentMemberType);
    depTy->addChild(base, Factory);
    depTy->addChild(assocTy, Factory);
    return depTy;
  }

  NodePointer demangleAssociatedTypeSimple() {
    // Demangle the base type.
    auto base = demangleGenericParamIndex();
    if (!base)
      return nullptr;

    NodePointer nodeType = Factory.createNode(Node::Kind::Type);
    nodeType->addChild(base, Factory);

    // Demangle the associated type name.
    return demangleDependentMemberTypeName(nodeType);
  }
  
  NodePointer demangleAssociatedTypeCompound() {
    // Demangle the base type.
    auto base = demangleGenericParamIndex();
    if (!base)
      return nullptr;

    // Demangle the associated type chain.
    while (!Mangled.nextIf('_')) {
      NodePointer nodeType = Factory.createNode(Node::Kind::Type);
      nodeType->addChild(base, Factory);
      
      base = demangleDependentMemberTypeName(nodeType);
      if (!base)
        return nullptr;
    }

    return base;
  }
  
  NodePointer demangleDependentType() {
    if (!Mangled)
      return nullptr;

    // A dependent member type begins with a non-index, non-'d' character.
    auto c = Mangled.peek();
    if (c != 'd' && c != '_' && !isDigit(c)) {
      NodePointer baseType = demangleType();
      if (!baseType) return nullptr;
      return demangleDependentMemberTypeName(baseType);
    }
    
    // Otherwise, we have a generic parameter.
    return demangleGenericParamIndex();
  }

  NodePointer demangleConstrainedTypeImpl() {
    // The constrained type can only be a generic parameter or an associated
    // type thereof. The 'q' introducer is thus left off of generic params.
    if (Mangled.nextIf('w')) {
      return demangleAssociatedTypeSimple();
    }
    if (Mangled.nextIf('W')) {
      return demangleAssociatedTypeCompound();
    }
    return demangleGenericParamIndex();
  }

  NodePointer demangleConstrainedType() {
    auto type = demangleConstrainedTypeImpl();
    if (!type)
      return nullptr;

    NodePointer nodeType = Factory.createNode(Node::Kind::Type);
    nodeType->addChild(type, Factory);
    return nodeType;
  }

  NodePointer demangleGenericSignature(bool isPseudogeneric = false) {
    auto sig =
      Factory.createNode(isPseudogeneric
                            ? Node::Kind::DependentPseudogenericSignature
                            : Node::Kind::DependentGenericSignature);
    // First read in the parameter counts at each depth.
    Node::IndexType count = ~(Node::IndexType)0;
    
    auto addCount = [&]{
      auto countNode =
        Factory.createNode(Node::Kind::DependentGenericParamCount, count);
      sig->addChild(countNode, Factory);
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
    
    while (!Mangled.nextIf('r')) {
      NodePointer reqt = demangleGenericRequirement();
      if (!reqt) return nullptr;
      sig->addChild(reqt, Factory);
    }
    
    return sig;
  }

  NodePointer demangleMetatypeRepresentation() {
    if (Mangled.nextIf('t'))
      return Factory.createNode(Node::Kind::MetatypeRepresentation, "@thin");

    if (Mangled.nextIf('T'))
      return Factory.createNode(Node::Kind::MetatypeRepresentation, "@thick");

    if (Mangled.nextIf('o'))
      return Factory.createNode(Node::Kind::MetatypeRepresentation,
                                 "@objc_metatype");

    unreachable("Unhandled metatype representation");
  }
  
  NodePointer demangleGenericRequirement() {
    NodePointer constrainedType = demangleConstrainedType();
    if (!constrainedType)
      return nullptr;
    if (Mangled.nextIf('z')) {
      NodePointer second = demangleType();
      if (!second) return nullptr;
      auto reqt = Factory.createNode(
          Node::Kind::DependentGenericSameTypeRequirement);
      reqt->addChild(constrainedType, Factory);
      reqt->addChild(second, Factory);
      return reqt;
    }

    if (Mangled.nextIf('l')) {
      StringRef name;
      Node::Kind kind;
      Node::IndexType size = SIZE_MAX;
      Node::IndexType alignment = SIZE_MAX;
      if (Mangled.nextIf('U')) {
        kind = Node::Kind::Identifier;
        name = "U";
      } else if (Mangled.nextIf('R')) {
        kind = Node::Kind::Identifier;
        name = "R";
      } else if (Mangled.nextIf('N')) {
        kind = Node::Kind::Identifier;
        name = "N";
      } else if (Mangled.nextIf('T')) {
        kind = Node::Kind::Identifier;
        name = "T";
      } else if (Mangled.nextIf('E')) {
        kind = Node::Kind::Identifier;
        if (!demangleNatural(size))
          return nullptr;
        if (!Mangled.nextIf('_'))
          return nullptr;
        if (!demangleNatural(alignment))
          return nullptr;
        name = "E";
      } else if (Mangled.nextIf('e')) {
        kind = Node::Kind::Identifier;
        if (!demangleNatural(size))
          return nullptr;
        name = "e";
      } else if (Mangled.nextIf('M')) {
        kind = Node::Kind::Identifier;
        if (!demangleNatural(size))
          return nullptr;
        if (!Mangled.nextIf('_'))
          return nullptr;
        if (!demangleNatural(alignment))
          return nullptr;
        name = "M";
      } else if (Mangled.nextIf('m')) {
        kind = Node::Kind::Identifier;
        if (!demangleNatural(size))
          return nullptr;
        name = "m";
      } else {
        unreachable("Unknown layout constraint");
      }

      NodePointer second = Factory.createNode(kind, name);
      if (!second) return nullptr;
      auto reqt = Factory.createNode(
        Node::Kind::DependentGenericLayoutRequirement);
      reqt->addChild(constrainedType, Factory);
      reqt->addChild(second, Factory);
      if (size != SIZE_MAX) {
        reqt->addChild(Factory.createNode(Node::Kind::Number, size), Factory);
        if (alignment != SIZE_MAX)
          reqt->addChild(Factory.createNode(Node::Kind::Number, alignment), Factory);
      }
      return reqt;
    }

    // Base class constraints are introduced by a class type mangling, which
    // will begin with either 'C' or 'S'.
    if (!Mangled)
      return nullptr;
    NodePointer constraint = nullptr;

    auto next = Mangled.peek();

    if (next == 'C') {
      constraint = demangleType();
      if (!constraint) return nullptr;
    } else if (next == 'S') {
      // A substitution may be either the module name of a protocol or a full
      // type name.
      NodePointer typeName = nullptr;
      Mangled.next();
      NodePointer sub = demangleSubstitutionIndex();
      if (!sub) return nullptr;
      if (sub->getKind() == Node::Kind::Protocol
          || sub->getKind() == Node::Kind::Class) {
        typeName = sub;
      } else if (sub->getKind() == Node::Kind::Module) {
        typeName = demangleProtocolNameGivenContext(sub);
        if (!typeName)
          return nullptr;
      } else {
        return nullptr;
      }
      constraint = Factory.createNode(Node::Kind::Type);
      constraint->addChild(typeName, Factory);
    } else {
      constraint = demangleProtocolName();
      if (!constraint)
        return nullptr;
    }
    auto reqt = Factory.createNode(
                          Node::Kind::DependentGenericConformanceRequirement);
    reqt->addChild(constrainedType, Factory);
    reqt->addChild(constraint, Factory);
    return reqt;
  }
  
  NodePointer demangleArchetypeType() {
    auto makeAssociatedType = [&](NodePointer root) -> NodePointer {
      NodePointer name = demangleIdentifier();
      if (!name) return nullptr;
      auto assocType = Factory.createNode(Node::Kind::AssociatedTypeRef);
      assocType->addChild(root, Factory);
      assocType->addChild(name, Factory);
      Substitutions.push_back(assocType);
      return assocType;
    };
    
    if (Mangled.nextIf('Q')) {
      NodePointer root = demangleArchetypeType();
      if (!root) return nullptr;
      return makeAssociatedType(root);
    }
    if (Mangled.nextIf('S')) {
      NodePointer sub = demangleSubstitutionIndex();
      if (!sub) return nullptr;
      return makeAssociatedType(sub);
    }
    if (Mangled.nextIf('s')) {
      NodePointer stdlib = Factory.createNode(Node::Kind::Module, STDLIB_NAME);
      return makeAssociatedType(stdlib);
    }
    if (Mangled.nextIf('q')) {
      NodePointer index = demangleIndexAsNode();
      if (!index)
        return nullptr;
      NodePointer decl_ctx = Factory.createNode(Node::Kind::DeclContext);
      NodePointer ctx = demangleContext();
      if (!ctx)
        return nullptr;
      decl_ctx->addChild(ctx, Factory);
      auto qual_atype = Factory.createNode(Node::Kind::QualifiedArchetype);
      qual_atype->addChild(index, Factory);
      qual_atype->addChild(decl_ctx, Factory);
      return qual_atype;
    }
    return nullptr;
  }

  NodePointer demangleTuple(IsVariadic isV) {
    NodePointer tuple = Factory.createNode(
        isV == IsVariadic::yes ? Node::Kind::VariadicTuple
                               : Node::Kind::NonVariadicTuple);
    while (!Mangled.nextIf('_')) {
      if (!Mangled)
        return nullptr;
      NodePointer elt = Factory.createNode(Node::Kind::TupleElement);

      if (isStartOfIdentifier(Mangled.peek())) {
        NodePointer label = demangleIdentifier(Node::Kind::TupleElementName);
        if (!label)
          return nullptr;
        elt->addChild(label, Factory);
      }

      NodePointer type = demangleType();
      if (!type)
        return nullptr;
      elt->addChild(type, Factory);

      tuple->addChild(elt, Factory);
    }
    return tuple;
  }
  
  NodePointer postProcessReturnTypeNode (NodePointer out_args) {
    NodePointer out_node = Factory.createNode(Node::Kind::ReturnType);
    out_node->addChild(out_args, Factory);
    return out_node;
  }

  NodePointer demangleType() {
    NodePointer type = demangleTypeImpl();
    if (!type)
      return nullptr;
    NodePointer nodeType = Factory.createNode(Node::Kind::Type);
    nodeType->addChild(type, Factory);
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
    NodePointer block = Factory.createNode(kind);
    
    if (throws) {
      block->addChild(Factory.createNode(Node::Kind::ThrowsAnnotation), Factory);
    }
    
    NodePointer in_node = Factory.createNode(Node::Kind::ArgumentTuple);
    block->addChild(in_node, Factory);
    in_node->addChild(in_args, Factory);
    block->addChild(postProcessReturnTypeNode(out_args), Factory);
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
        return Factory.createNode(Node::Kind::BuiltinTypeName,
                                     "Builtin.BridgeObject");
      if (c == 'B')
        return Factory.createNode(Node::Kind::BuiltinTypeName,
                                     "Builtin.UnsafeValueBuffer");
      if (c == 'f') {
        Node::IndexType size;
        if (demangleBuiltinSize(size)) {
          return Factory.createNode(
              Node::Kind::BuiltinTypeName,
              std::move(DemanglerPrinter() << "Builtin.Float" << size).str());
        }
      }
      if (c == 'i') {
        Node::IndexType size;
        if (demangleBuiltinSize(size)) {
          return Factory.createNode(
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
            return Factory.createNode(
                Node::Kind::BuiltinTypeName,
                (DemanglerPrinter() << "Builtin.Vec" << elts << "xInt" << size)
                    .str());
          }
          if (Mangled.nextIf('f')) {
            Node::IndexType size;
            if (!demangleBuiltinSize(size))
              return nullptr;
            return Factory.createNode(
                Node::Kind::BuiltinTypeName,
                (DemanglerPrinter() << "Builtin.Vec" << elts << "xFloat"
                                    << size).str());
          }
          if (Mangled.nextIf('p'))
            return Factory.createNode(
                Node::Kind::BuiltinTypeName,
                (DemanglerPrinter() << "Builtin.Vec" << elts << "xRawPointer")
                    .str());
        }
      }
      if (c == 'O')
        return Factory.createNode(Node::Kind::BuiltinTypeName,
                                     "Builtin.UnknownObject");
      if (c == 'o')
        return Factory.createNode(Node::Kind::BuiltinTypeName,
                                     "Builtin.NativeObject");
      if (c == 'p')
        return Factory.createNode(Node::Kind::BuiltinTypeName,
                                     "Builtin.RawPointer");
      if (c == 'w')
        return Factory.createNode(Node::Kind::BuiltinTypeName,
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

      NodePointer dynamicSelf = Factory.createNode(Node::Kind::DynamicSelf);
      dynamicSelf->addChild(type, Factory);
      return dynamicSelf;
    }
    if (c == 'E') {
      if (!Mangled.nextIf('R'))
        return nullptr;
      if (!Mangled.nextIf('R'))
        return nullptr;
      return Factory.createNode(Node::Kind::ErrorType, std::string());
    }
    if (c == 'F') {
      return demangleFunctionType(Node::Kind::FunctionType);
    }
    if (c == 'f') {
      return demangleFunctionType(Node::Kind::UncurriedFunctionType);
    }
    if (c == 'G') {
      return demangleBoundGenericType();
    }
    if (c == 'X') {
      if (Mangled.nextIf('b')) {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer boxType = Factory.createNode(Node::Kind::SILBoxType);
        boxType->addChild(type, Factory);
        return boxType;
      }
      if (Mangled.nextIf('B')) {
        NodePointer signature = nullptr;
        if (Mangled.nextIf('G')) {
          signature = demangleGenericSignature(/*pseudogeneric*/ false);
          if (!signature)
            return nullptr;
        }
        NodePointer layout = Factory.createNode(Node::Kind::SILBoxLayout);
        while (!Mangled.nextIf('_')) {
          Node::Kind kind;
          if (Mangled.nextIf('m'))
            kind = Node::Kind::SILBoxMutableField;
          else if (Mangled.nextIf('i'))
            kind = Node::Kind::SILBoxImmutableField;
          else
            return nullptr;
          
          auto type = demangleType();
          if (!type)
            return nullptr;
          auto field = Factory.createNode(kind);
          field->addChild(type, Factory);
          layout->addChild(field, Factory);
        }
        NodePointer genericArgs = nullptr;
        if (signature) {
          genericArgs = Factory.createNode(Node::Kind::TypeList);
          while (!Mangled.nextIf('_')) {
            auto type = demangleType();
            if (!type)
              return nullptr;
            genericArgs->addChild(type, Factory);
          }
        }
        NodePointer boxType =
          Factory.createNode(Node::Kind::SILBoxTypeWithLayout);
        boxType->addChild(layout, Factory);
        if (signature) {
          boxType->addChild(signature, Factory);
          assert(genericArgs);
          boxType->addChild(genericArgs, Factory);
        }
        return boxType;
      }
    }
    if (c == 'K') {
      return demangleFunctionType(Node::Kind::AutoClosureType);
    }
    if (c == 'M') {
      NodePointer type = demangleType();
      if (!type)
        return nullptr;
      NodePointer metatype = Factory.createNode(Node::Kind::Metatype);
      metatype->addChild(type, Factory);
      return metatype;
    }
    if (c == 'X') {
      if (Mangled.nextIf('M')) {
        NodePointer metatypeRepr = demangleMetatypeRepresentation();
        if (!metatypeRepr) return nullptr;

        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer metatype = Factory.createNode(Node::Kind::Metatype);
        metatype->addChild(metatypeRepr, Factory);
        metatype->addChild(type, Factory);
        return metatype;
      }
    }
    if (c == 'P') {
      if (Mangled.nextIf('M')) {
        NodePointer type = demangleType();
        if (!type) return nullptr;
        auto metatype = Factory.createNode(Node::Kind::ExistentialMetatype);
        metatype->addChild(type, Factory);
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

          auto metatype = Factory.createNode(Node::Kind::ExistentialMetatype);
          metatype->addChild(metatypeRepr, Factory);
          metatype->addChild(type, Factory);
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
    if (c == 'x') {
      // Special mangling for the first generic param.
      return getDependentGenericParamType(0, 0);
    }
    if (c == 'w') {
      return demangleAssociatedTypeSimple();
    }
    if (c == 'W') {
      return demangleAssociatedTypeCompound();
    }
    if (c == 'R') {
      NodePointer inout = Factory.createNode(Node::Kind::InOut);
      NodePointer type = demangleTypeImpl();
      if (!type)
        return nullptr;
      inout->addChild(type, Factory);
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
        = Factory.createNode(Node::Kind::DependentGenericType);
      dependentGenericType->addChild(sig, Factory);
      dependentGenericType->addChild(sub, Factory);
      return dependentGenericType;
    }
    if (c == 'X') {
      if (Mangled.nextIf('f')) {
        return demangleFunctionType(Node::Kind::ThinFunctionType);
      }
      if (Mangled.nextIf('o')) {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer unowned = Factory.createNode(Node::Kind::Unowned);
        unowned->addChild(type, Factory);
        return unowned;
      }
      if (Mangled.nextIf('u')) {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer unowned = Factory.createNode(Node::Kind::Unmanaged);
        unowned->addChild(type, Factory);
        return unowned;
      }
      if (Mangled.nextIf('w')) {
        NodePointer type = demangleType();
        if (!type)
          return nullptr;
        NodePointer weak = Factory.createNode(Node::Kind::Weak);
        weak->addChild(type, Factory);
        return weak;
      }

      // type ::= 'XF' impl-function-type
      if (Mangled.nextIf('F')) {
        return demangleImplFunctionType();
      }

      return nullptr;
    }
    if (isStartOfNominalType(c))
      return demangleDeclarationName(nominalTypeMarkerToNodeKind(c));
    return nullptr;
  }

  bool demangleReabstractSignature(NodePointer signature) {
    if (Mangled.nextIf('G')) {
      NodePointer generics = demangleGenericSignature();
      if (!generics) return false;
      signature->addChild(generics, Factory);
    }

    NodePointer srcType = demangleType();
    if (!srcType) return false;
    signature->addChild(srcType, Factory);

    NodePointer destType = demangleType();
    if (!destType) return false;
    signature->addChild(destType, Factory);

    return true;
  }

  // impl-function-type ::= impl-callee-convention impl-function-attribute*
  //                        generics? '_' impl-parameter* '_' impl-result* '_'
  // impl-function-attribute ::= 'Cb'            // compatible with C block invocation function
  // impl-function-attribute ::= 'Cc'            // compatible with C global function
  // impl-function-attribute ::= 'Cm'            // compatible with Swift method
  // impl-function-attribute ::= 'CO'            // compatible with ObjC method
  // impl-function-attribute ::= 'Cw'            // compatible with protocol witness
  // impl-function-attribute ::= 'G'             // generic
  NodePointer demangleImplFunctionType() {
    NodePointer type = Factory.createNode(Node::Kind::ImplFunctionType);

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

    // Enter a new generic context if this type is generic.
    // FIXME: replace with std::optional, when we have it.
    bool isPseudogeneric = false;
    if (Mangled.nextIf('G') ||
        (isPseudogeneric = Mangled.nextIf('g'))) {
      NodePointer generics = demangleGenericSignature(isPseudogeneric);
      if (!generics)
        return nullptr;
      type->addChild(generics, Factory);
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
    CASE('D',   Nothing,                Nothing,         "@unowned_inner_pointer")
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
    type->addChild(Factory.createNode(Node::Kind::ImplConvention, attr), Factory);
    return true;
  }

  void addImplFunctionAttribute(NodePointer parent, StringRef attr,
                         Node::Kind kind = Node::Kind::ImplFunctionAttribute) {
    parent->addChild(Factory.createNode(kind, attr), Factory);
  }

  // impl-parameter ::= impl-convention type
  bool demangleImplParameters(NodePointer parent) {
    while (!Mangled.nextIf('_')) {
      auto input = demangleImplParameterOrResult(Node::Kind::ImplParameter);
      if (!input) return false;
      parent->addChild(input, Factory);
    }
    return true;
  }

  // impl-result ::= impl-convention type
  bool demangleImplResults(NodePointer parent) {
    while (!Mangled.nextIf('_')) {
      auto res = demangleImplParameterOrResult(Node::Kind::ImplResult);
      if (!res) return false;
      parent->addChild(res, Factory);
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

    NodePointer node = Factory.createNode(kind);
    node->addChild(Factory.createNode(Node::Kind::ImplConvention, convention),
                   Factory);
    node->addChild(type, Factory);
    
    return node;
  }
};
} // end anonymous namespace


bool
swift::Demangle::isSwiftSymbol(const char *mangledName) {
  // The old mangling.
  if (mangledName[0] == '_' && mangledName[1] == 'T')
    return true;

  // The new mangling.
  for (unsigned i = 0; i < sizeof(MANGLING_PREFIX_STR) - 1; i++) {
    if (mangledName[i] != MANGLING_PREFIX_STR[i])
      return false;
  }
  return true;
}

NodePointer
swift::Demangle::demangleOldSymbolAsNode(StringRef MangledName,
                                         NodeFactory &Factory) {
  OldDemangler demangler(MangledName, Factory);
  return demangler.demangleTopLevel();
}

NodePointer swift::Demangle::demangleOldTypeAsNode(llvm::StringRef MangledName,
                                                   NodeFactory &Factory) {
  OldDemangler demangler(MangledName, Factory);
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
    return std::move(Printer).str();
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
              node->getText().startswith(LLDB_EXPRESSIONS_MODULE_NAME_PREFIX));
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
    case Node::Kind::SILBoxType:
    case Node::Kind::SILBoxTypeWithLayout:
    case Node::Kind::Structure:
    case Node::Kind::TupleElementName:
    case Node::Kind::Type:
    case Node::Kind::TypeAlias:
    case Node::Kind::TypeList:
    case Node::Kind::VariadicTuple:
      return true;

    case Node::Kind::ProtocolList:
      if (pointer->getChild(0)->getNumChildren() <= 1)
        return true;
      return false;

    case Node::Kind::Allocator:
    case Node::Kind::ArgumentTuple:
    case Node::Kind::AssociatedTypeMetadataAccessor:
    case Node::Kind::AssociatedTypeWitnessTableAccessor:
    case Node::Kind::AutoClosureType:
    case Node::Kind::CFunctionPointer:
    case Node::Kind::Constructor:
    case Node::Kind::CurryThunk:
    case Node::Kind::Deallocator:
    case Node::Kind::DeclContext:
    case Node::Kind::DefaultArgumentInitializer:
    case Node::Kind::DependentAssociatedTypeRef:
    case Node::Kind::DependentGenericSignature:
    case Node::Kind::DependentGenericParamCount:
    case Node::Kind::DependentGenericConformanceRequirement:
    case Node::Kind::DependentGenericLayoutRequirement:
    case Node::Kind::DependentGenericSameTypeRequirement:
    case Node::Kind::DependentPseudogenericSignature:
    case Node::Kind::Destructor:
    case Node::Kind::DidSet:
    case Node::Kind::DirectMethodReferenceAttribute:
    case Node::Kind::Directness:
    case Node::Kind::DynamicAttribute:
    case Node::Kind::ExplicitClosure:
    case Node::Kind::Extension:
    case Node::Kind::FieldOffset:
    case Node::Kind::FullTypeMetadata:
    case Node::Kind::Function:
    case Node::Kind::FunctionSignatureSpecialization:
    case Node::Kind::FunctionSignatureSpecializationParam:
    case Node::Kind::FunctionSignatureSpecializationParamKind:
    case Node::Kind::FunctionSignatureSpecializationParamPayload:
    case Node::Kind::FunctionType:
    case Node::Kind::GenericProtocolWitnessTable:
    case Node::Kind::GenericProtocolWitnessTableInstantiationFunction:
    case Node::Kind::GenericPartialSpecialization:
    case Node::Kind::GenericPartialSpecializationNotReAbstracted:
    case Node::Kind::GenericSpecialization:
    case Node::Kind::GenericSpecializationNotReAbstracted:
    case Node::Kind::GenericSpecializationParam:
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
    case Node::Kind::LazyProtocolWitnessTableCacheVariable:
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
    case Node::Kind::ProtocolDescriptor:
    case Node::Kind::ProtocolWitness:
    case Node::Kind::ProtocolWitnessTable:
    case Node::Kind::ProtocolWitnessTableAccessor:
    case Node::Kind::ReabstractionThunk:
    case Node::Kind::ReabstractionThunkHelper:
    case Node::Kind::Setter:
    case Node::Kind::SILBoxLayout:
    case Node::Kind::SILBoxMutableField:
    case Node::Kind::SILBoxImmutableField:
    case Node::Kind::SpecializationIsFragile:
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
    case Node::Kind::ReflectionMetadataBuiltinDescriptor:
    case Node::Kind::ReflectionMetadataFieldDescriptor:
    case Node::Kind::ReflectionMetadataAssocTypeDescriptor:
    case Node::Kind::ReflectionMetadataSuperclassDescriptor:
    case Node::Kind::GenericTypeParamDecl:
    case Node::Kind::ThrowsAnnotation:
    case Node::Kind::EmptyList:
    case Node::Kind::FirstElementMarker:
    case Node::Kind::VariadicMarker:
    case Node::Kind::OutlinedCopy:
    case Node::Kind::OutlinedConsume:
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

    if (!Options.SynthesizeSugarOnTypes ||
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
      if (context->getKind() == Node::Kind::Module && !Options.DisplayModuleNames)
          return;
      Printer << '.';
    }
  }

  void print(NodePointer pointer, bool asContext = false, bool suppressType = false);

  unsigned printFunctionSigSpecializationParam(NodePointer pointer,
                                               unsigned Idx);

  void printSpecializationPrefix(NodePointer node, StringRef Description,
                                 StringRef ParamPrefix = StringRef());
};
} // end anonymous namespace

static bool isExistentialType(NodePointer node) {
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
  case FunctionSigSpecializationParamKind::BoxToValue:
  case FunctionSigSpecializationParamKind::BoxToStack:
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
       (V & unsigned(FunctionSigSpecializationParamKind::SROA)) ||
       (V & unsigned(FunctionSigSpecializationParamKind::Dead))) &&
      "Invalid OptionSet");
  print(pointer->getChild(Idx++));
  return Idx;
}

void NodePrinter::printSpecializationPrefix(NodePointer node,
                                            StringRef Description,
                                            StringRef ParamPrefix) {
  if (!Options.DisplayGenericSpecializations) {
    Printer << "specialized ";
    return;
  }
  Printer << Description << " <";
  const char *Separator = "";
  for (unsigned i = 0, e = node->getNumChildren(); i < e; ++i) {
    switch (node->getChild(i)->getKind()) {
      case Node::Kind::SpecializationPassID:
        // We skip the SpecializationPassID since it does not contain any
        // information that is useful to our users.
        break;

      case Node::Kind::SpecializationIsFragile:
        Printer << Separator;
        Separator = ", ";
        print(node->getChild(i));
        break;

      default:
        // Ignore empty specializations.
        if (node->getChild(i)->hasChildren()) {
          Printer << Separator << ParamPrefix;
          Separator = ", ";
          print(node->getChild(i));
        }
        break;
    }
  }
  Printer << "> of ";
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
    while (type->getKind() == Node::Kind::DependentGenericType)
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

  if (type->getKind() == Node::Kind::DependentGenericType) {
    type = type->getChild(1)->getChild(0);
  }

  print(entityType);
}

void NodePrinter::print(NodePointer pointer, bool asContext, bool suppressType) {
  // Common code for handling entities.
  auto printEntity = [&](bool hasName, bool hasType, StringRef extraName) {
    if (Options.QualifyEntities)
      printContext(pointer->getChild(0));

    bool printType = (hasType && !suppressType);
    bool useParens = (printType && asContext);

    if (useParens) Printer << '(';

    if (hasName) print(pointer->getChild(1));
    Printer << extraName;

    if (printType) {
      NodePointer type = pointer->getChild(1 + unsigned(hasName));
      if (useColonForEntityType(pointer, type)) {
        if (Options.DisplayEntityTypes) {
          Printer << " : ";
          print(type);
        }
      } else if (!Options.DisplayEntityTypes) {
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
  case Node::Kind::CurryThunk:
    Printer << "curry thunk of ";
    print(pointer->getChild(0), asContext, suppressType);
    return;
  case Node::Kind::OutlinedCopy:
    Printer << "outlined copy of ";
    print(pointer->getChild(0), asContext, suppressType);
    return;
  case Node::Kind::OutlinedConsume:
    Printer << "outlined consume of ";
    print(pointer->getChild(0), asContext, suppressType);
    return;
  case Node::Kind::Directness:
    Printer << toString(Directness(pointer->getIndex())) << " ";
    return;
  case Node::Kind::Extension:
    assert((pointer->getNumChildren() == 2 || pointer->getNumChildren() == 3)
           && "Extension expects 2 or 3 children.");
    if (Options.QualifyEntities && Options.DisplayExtensionContexts) {
      Printer << "(extension in ";
      // Print the module where extension is defined.
      print(pointer->getChild(0), true);
      Printer << "):";
    }
    print(pointer->getChild(1), asContext);
    if (pointer->getNumChildren() == 3)
      print(pointer->getChild(2), true);
    return;
  case Node::Kind::Variable:
  case Node::Kind::Function:
  case Node::Kind::Subscript:
  case Node::Kind::GenericTypeParamDecl:
    printEntity(true, true, "");
    return;
  case Node::Kind::ExplicitClosure:
  case Node::Kind::ImplicitClosure: {
    auto index = pointer->getChild(1)->getIndex();
    DemanglerPrinter printName;
    printName << '(';
    if (pointer->getKind() == Node::Kind::ImplicitClosure)
      printName << "implicit ";
    printName << "closure #" << (index + 1) << ")";
    printEntity(false, false, std::move(printName).str());
    return;
  }
  case Node::Kind::Global:
    printChildren(pointer);
    return;
  case Node::Kind::Suffix:
    if (!Options.DisplayUnmangledSuffix) return;
    Printer << " with unmangled suffix " << QuotedString(pointer->getText());
    return;
  case Node::Kind::Initializer:
    printEntity(false, false, "(variable initialization expression)");
    return;
  case Node::Kind::DefaultArgumentInitializer: {
    auto index = pointer->getChild(1);
    DemanglerPrinter strPrinter;
    strPrinter << "(default argument " << index->getIndex() << ")";
    printEntity(false, false, std::move(strPrinter).str());
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
    if (Options.ShowPrivateDiscriminators)
      Printer << '(';

    print(pointer->getChild(1));

    if (Options.ShowPrivateDiscriminators)
      Printer << " in " << pointer->getChild(0)->getText() << ')';
    return;
  case Node::Kind::Module:
    if (Options.DisplayModuleNames)
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
  case Node::Kind::UncurriedFunctionType:
    printFunctionType(pointer);
    return;
  case Node::Kind::ArgumentTuple: {
    bool need_parens = false;
    if (pointer->getNumChildren() > 1)
      need_parens = true;
    else {
      if (!pointer->hasChildren())
        need_parens = true;
      else {
        Node::Kind child0_kind = pointer->getChild(0)->getKind();
        if (child0_kind == Node::Kind::Type)
          child0_kind = pointer->getChild(0)->getChild(0)->getKind();

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
    Printer << "@nonobjc ";
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
    return printSpecializationPrefix(pointer,
              "function signature specialization");
  case Node::Kind::GenericPartialSpecialization:
    return printSpecializationPrefix(pointer,
              "generic partial specialization", "Signature = ");
  case Node::Kind::GenericPartialSpecializationNotReAbstracted:
    return printSpecializationPrefix(pointer,
              "generic not-reabstracted partial specialization", "Signature = ");
  case Node::Kind::GenericSpecialization:
    return printSpecializationPrefix(pointer,
              "generic specialization");
  case Node::Kind::GenericSpecializationNotReAbstracted:
    return printSpecializationPrefix(pointer,
              "generic not re-abstracted specialization");
  case Node::Kind::SpecializationIsFragile:
    Printer << "preserving fragile attribute";
    return;
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

    bool printedOptionSet = false;
    if (raw & uint64_t(FunctionSigSpecializationParamKind::Dead)) {
      printedOptionSet = true;
      Printer << "Dead";
    }

    if (raw & uint64_t(FunctionSigSpecializationParamKind::OwnedToGuaranteed)) {
      if (printedOptionSet)
        Printer << " and ";
      printedOptionSet = true;
      Printer << "Owned To Guaranteed";
    }

    if (raw & uint64_t(FunctionSigSpecializationParamKind::SROA)) {
      if (printedOptionSet)
        Printer << " and ";
      Printer << "Exploded";
      return;
    }

    if (printedOptionSet)
      return;

    switch (FunctionSigSpecializationParamKind(raw)) {
    case FunctionSigSpecializationParamKind::BoxToValue:
      Printer << "Value Promoted from Box";
      break;
    case FunctionSigSpecializationParamKind::BoxToStack:
      Printer << "Stack Promoted from Box";
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
    case FunctionSigSpecializationParamKind::Dead:
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
  case Node::Kind::LazyProtocolWitnessTableAccessor:
    Printer << "lazy protocol witness table accessor for type ";
    print(pointer->getChild(0));
    Printer << " and conformance ";
    print(pointer->getChild(1));
    return;
  case Node::Kind::LazyProtocolWitnessTableCacheVariable:
    Printer << "lazy protocol witness table cache variable for type ";
    print(pointer->getChild(0));
    Printer << " and conformance ";
    print(pointer->getChild(1));
    return;
  case Node::Kind::ProtocolWitnessTableAccessor:
    Printer << "protocol witness table accessor for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::ProtocolWitnessTable:
    Printer << "protocol witness table for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::GenericProtocolWitnessTable:
    Printer << "generic protocol witness table for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::GenericProtocolWitnessTableInstantiationFunction:
    Printer << "instantiation function for generic protocol witness table for ";
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
    if (Options.ShortenPartialApply)
      Printer << "partial apply";
    else
      Printer << "partial apply forwarder";

    if (pointer->hasChildren()) {
      Printer << " for ";
      print(pointer->getFirstChild());
    }
    return;
  case Node::Kind::PartialApplyObjCForwarder:
    if (Options.ShortenPartialApply)
      Printer << "partial apply";
    else
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
    if (Options.ShortenThunk) {
      Printer << "thunk for ";
      print(pointer->getChild(pointer->getNumChildren() - 2));
      return;
    }
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
    Printer << "generic type metadata pattern for ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::Metaclass:
    Printer << "metaclass for ";
    print(pointer->getFirstChild());
    return;
  case Node::Kind::ProtocolDescriptor:
    Printer << "protocol descriptor for ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::FullTypeMetadata:
    Printer << "full type metadata for ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::TypeMetadata:
    Printer << "type metadata for ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::TypeMetadataAccessFunction:
    Printer << "type metadata accessor for ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::TypeMetadataLazyCache:
    Printer << "lazy cache variable for type metadata for ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::AssociatedTypeMetadataAccessor:
    Printer << "associated type metadata accessor for ";
    print(pointer->getChild(1));
    Printer << " in ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::AssociatedTypeWitnessTableAccessor:
    Printer << "associated type witness table accessor for ";
    print(pointer->getChild(1));
    Printer << " : ";
    print(pointer->getChild(2));
    Printer << " in ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::NominalTypeDescriptor:
    Printer << "nominal type descriptor for ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::ValueWitness:
    Printer << toString(ValueWitnessKind(pointer->getIndex()));
    if (Options.ShortenValueWitness) Printer << " for ";
    else Printer << " value witness for ";
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
  case Node::Kind::SILBoxType: {
    Printer << "@box ";
    NodePointer type = pointer->getChild(0);
    print(type);
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
    NodePointer type = pointer->getChild(Idx)->getChild(0);
    bool needs_parens = !isSimpleType(type);
    if (needs_parens)
      Printer << "(";
    print(type);
    if (needs_parens)
      Printer << ")";
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
  case Node::Kind::AssociatedTypeRef:
    print(pointer->getChild(0));
    Printer << '.' << pointer->getChild(1)->getText();
    return;
  case Node::Kind::ProtocolList: {
    NodePointer type_list = pointer->getChild(0);
    if (!type_list)
      return;
    if (type_list->getNumChildren() == 0)
      Printer << "Any";
    else
      printChildren(type_list, " & ");
    return;
  }
  case Node::Kind::AssociatedType:
    // Don't print for now.
    return;
  case Node::Kind::QualifiedArchetype: {
    if (Options.ShortenArchetype) {
      Printer << "(archetype)";
      return;
    }
    if (pointer->getNumChildren() < 2)
      return;
    NodePointer number = pointer->getChild(0);
    NodePointer decl_ctx = pointer->getChild(1);
    Printer << "(archetype " << number->getIndex() << " of ";
    print(decl_ctx);
    Printer << ")";
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
    if (pointer->getNumChildren() == 4) {
      // TODO: check if this is correct
      Printer << "property behavior storage of ";
      print(child2);
      Printer << " in ";
      print(child0);
      Printer << " : ";
      print(child1);
    } else {
      print(child0);
      if (Options.DisplayProtocolConformances) {
        Printer << " : ";
        print(child1);
        Printer << " in ";
        print(child2);
      }
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
    LLVM_FALLTHROUGH;
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
      
  case Node::Kind::DependentPseudogenericSignature:
  case Node::Kind::DependentGenericSignature: {
    Printer << '<';
    
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
        // FIXME: Depth won't match when a generic signature applies to a
        // method in generic type context.
        Printer << archetypeName(index, depth);
      }
    }
    
    if (depth != numChildren) {
      if (!Options.DisplayWhereClauses) {
        Printer << " where ...";
      } else {
        Printer << " where ";
        for (unsigned i = depth; i < numChildren; ++i) {
          if (i > depth)
            Printer << ", ";
          print(pointer->getChild(i));
        }
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
  case Node::Kind::DependentGenericLayoutRequirement: {
    NodePointer type = pointer->getChild(0);
    NodePointer layout = pointer->getChild(1);
    print(type);
    Printer << ": ";
    assert(layout->getKind() == Node::Kind::Identifier);
    assert(layout->getText().size() == 1);
    char c = layout->getText()[0];
    StringRef name;
    if (c == 'U') {
      name = "_UnknownLayout";
    } else if (c == 'R') {
      name = "_RefCountedObject";
    } else if (c == 'N') {
      name = "_NativeRefCountedObject";
    } else if (c == 'T') {
      name = "_Trivial";
    } else if (c == 'E' || c == 'e') {
      name = "_Trivial";
    } else if (c == 'M' || c == 'm') {
      name = "_TrivialAtMost";
    }
    Printer << name;
    if (pointer->getNumChildren() > 2) {
      Printer << "(";
      print(pointer->getChild(2));
      if (pointer->getNumChildren() > 3) {
        Printer << ", ";
        print(pointer->getChild(3));
      }
      Printer << ")";
    }
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
    Printer << '.';
    NodePointer assocTy = pointer->getChild(1);
    print(assocTy);
    return;
  }
  case Node::Kind::DependentAssociatedTypeRef: {
    Printer << pointer->getText();
    return;
  }
  case Node::Kind::ReflectionMetadataBuiltinDescriptor:
    Printer << "reflection metadata builtin descriptor ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::ReflectionMetadataFieldDescriptor:
    Printer << "reflection metadata field descriptor ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::ReflectionMetadataAssocTypeDescriptor:
    Printer << "reflection metadata associated type descriptor ";
    print(pointer->getChild(0));
    return;
  case Node::Kind::ReflectionMetadataSuperclassDescriptor:
    Printer << "reflection metadata superclass descriptor ";
    print(pointer->getChild(0));
    return;

  case Node::Kind::ThrowsAnnotation:
    Printer<< " throws ";
    return;
  case Node::Kind::EmptyList:
    Printer << " empty-list ";
    return;
  case Node::Kind::FirstElementMarker:
    Printer << " first-element-marker ";
    return;
  case Node::Kind::VariadicMarker:
    Printer << " variadic-marker ";
    return;
  case Node::Kind::SILBoxTypeWithLayout: {
    assert(pointer->getNumChildren() == 1 || pointer->getNumChildren() == 3);
    NodePointer layout = pointer->getChild(0);
    assert(layout->getKind() == Node::Kind::SILBoxLayout);
    NodePointer signature, genericArgs = nullptr;
    if (pointer->getNumChildren() == 3) {
      signature = pointer->getChild(1);
      assert(signature->getKind() == Node::Kind::DependentGenericSignature);
      genericArgs = pointer->getChild(2);
      assert(genericArgs->getKind() == Node::Kind::TypeList);
      
      print(signature);
      Printer << ' ';
    }
    print(layout);
    if (genericArgs) {
      Printer << " <";
      for (unsigned i = 0, e = genericArgs->getNumChildren(); i < e; ++i) {
        if (i > 0)
          Printer << ", ";
        print(genericArgs->getChild(i));
      }
      Printer << '>';
    }
    return;
  }
  case Node::Kind::SILBoxLayout:
    Printer << '{';
    for (unsigned i = 0; i < pointer->getNumChildren(); ++i) {
      if (i > 0)
        Printer << ',';
      Printer << ' ';
      print(pointer->getChild(i));
    }
    Printer << " }";
    return;
  case Node::Kind::SILBoxImmutableField:
  case Node::Kind::SILBoxMutableField:
    Printer << (pointer->getKind() == Node::Kind::SILBoxImmutableField
      ? "let "
      : "var ");
    assert(pointer->getNumChildren() == 1
           && pointer->getChild(0)->getKind() == Node::Kind::Type);
    print(pointer->getChild(0));
    return;
  }
  unreachable("bad node kind!");
}

std::string Demangle::nodeToString(NodePointer root,
                                   const DemangleOptions &options) {
  if (!root)
    return "";

  return NodePrinter(options).printRoot(root);
}
