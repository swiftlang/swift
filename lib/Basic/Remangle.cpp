//===--- Remangle.cpp - Swift re-mangling from a demangling tree ---------===//
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
//  This file implements the remangler, which turns a demangling parse
//  tree back into a mangled string.  This is useful for tools which
//  want to extract subtrees from mangled strings.
//
//===---------------------------------------------------------------------===//

#include "swift/Basic/Demangle.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Punycode.h"
#include "swift/Basic/UUID.h"
#include "swift/Strings.h"
#include "llvm/ADT/StringRef.h"
#include <ostream>
#include <sstream>
#include <vector>
#include <cstdlib>
#include <unordered_map>

using namespace swift;
using namespace Demangle;

[[noreturn]]
static void unreachable(const char *Message) {
  fprintf(stderr, "fatal error: %s\n", Message);
  std::abort();
}

/// Translate the given operator character into its mangled form.
///
/// Current operator characters:   @/=-+*%<>!&|^~ and the special operator '..'
static char mangleOperatorChar(char op) {
  switch (op) {
  case '&': return 'a'; // 'and'
  case '@': return 'c'; // 'commercial at sign'
  case '/': return 'd'; // 'divide'
  case '=': return 'e'; // 'equal'
  case '>': return 'g'; // 'greater'
  case '<': return 'l'; // 'less'
  case '*': return 'm'; // 'multiply'
  case '!': return 'n'; // 'negate'
  case '|': return 'o'; // 'or'
  case '+': return 'p'; // 'plus'
  case '?': return 'q'; // 'question'
  case '%': return 'r'; // 'remainder'
  case '-': return 's'; // 'subtract'
  case '~': return 't'; // 'tilde'
  case '^': return 'x'; // 'xor'
  case '.': return 'z'; // 'zperiod' (the z is silent)
  default:
    return op;
  }
}

static bool isNonAscii(StringRef str) {
  for (unsigned char c : str) {
    if (c >= 0x80)
      return true;
  }
  return false;
}

static std::ostream &operator<<(std::ostream &str, StringRef string) {
  str.write(string.data(), string.size());
  return str;
}

static void mangleIdentifier(StringRef ident, OperatorKind operatorKind,
                             bool usePunycode, std::ostream &out) {
  std::string punycodeBuf;
  if (usePunycode) {
    // If the identifier contains non-ASCII character, we mangle 
    // with an initial X and Punycode the identifier string.
    if (isNonAscii(ident)) {
      out << 'X';
      Punycode::encodePunycodeUTF8(ident, punycodeBuf);
      ident = punycodeBuf;
    }
  }

  // Mangle normal identifiers as
  //   count identifier-char+
  // where the count is the number of characters in the identifier,
  // and where individual identifier characters represent themselves.
  if (operatorKind == OperatorKind::NotOperator) {
    out << ident.size() << ident;
    return;
  }

  // Mangle operator identifiers as
  //   operator ::= 'o' operator-fixity count operator-char+
  //   operator-fixity ::= 'p' // prefix
  //   operator-fixity ::= 'P' // postfix
  //   operator-fixity ::= 'i' // infix
  // where the count is the number of characters in the operator,
  // and where the individual operator characters are translated.
  out << 'o' << [=] {
    switch (operatorKind) {
    case OperatorKind::NotOperator: unreachable("invalid");
    case OperatorKind::Infix: return 'i';
    case OperatorKind::Prefix: return 'p';
    case OperatorKind::Postfix: return 'P';
    }
    unreachable("invalid");
  }();

  // Mangle ASCII operators directly.
  out << ident.size();
  for (char ch : ident) {
    out << mangleOperatorChar(ch);
  }
}

void Demangle::mangleIdentifier(const char *data, size_t length,
                                OperatorKind operatorKind,
                                std::ostream &out, bool usePunycode) {
  return ::mangleIdentifier(StringRef(data, length), operatorKind,
                            usePunycode, out);
}

namespace {
  struct DeepHasher {
    size_t value = 0;

    void combine(size_t newValue) {
      value = 33 * value + newValue;
    }

    void hash(Node *node) {
      combine((size_t) node->getKind());
      if (node->hasIndex()) {
        combine(node->getIndex());
      } else if (node->hasText()) {
        StringRef text = node->getText();
        for (char c : text) {
          combine((unsigned char) c);
        }
      }
      for (const auto &child : *node) {
        hash(child.get());
      }
    }
  };
}

static size_t deepHash(Node *node) {
  DeepHasher hasher;
  hasher.hash(node);
  return hasher.value;  
}

static bool deepEquals(Node *lhs, Node *rhs) {
  if (lhs->getKind() != rhs->getKind())
    return false;
  if (lhs->hasIndex()) {
    if (!rhs->hasIndex())
      return false;
    if (lhs->getIndex() != rhs->getIndex())
      return false;
  } else if (lhs->hasText()) {
    if (!rhs->hasText())
      return false;
    if (lhs->getText() != rhs->getText())
      return false;
  } else if (rhs->hasIndex() || rhs->hasText()) {
    return false;
  }

  if (lhs->getNumChildren() != rhs->getNumChildren())
    return false;

  for (auto li = lhs->begin(), ri = lhs->begin(), le = lhs->end();
       li != le; ++li, ++ri) {
    if (!deepEquals(li->get(), ri->get()))
      return false;
  }

  return true;
}

namespace {
  struct SubstitutionEntry {
    Node *TheNode;
    size_t StoredHash;

    // Note that the constructor leaves this uninitialized.

    struct Hasher {
      size_t operator()(const SubstitutionEntry &entry) const {
        return entry.StoredHash;
      }
    };
    friend bool operator==(const SubstitutionEntry &lhs,
                           const SubstitutionEntry &rhs) {
      return (lhs.StoredHash == rhs.StoredHash &&
              deepEquals(lhs.TheNode, lhs.TheNode));
    }
  };

  class Remangler {
    struct ArchetypeInfo {
      Node::IndexType Index;
      Node::IndexType AbsoluteDepth;
    };

    std::ostream &Out;
    std::unordered_map<std::string, ArchetypeInfo> Archetypes;
    unsigned AbsoluteArchetypeDepth = 0;

    std::unordered_map<SubstitutionEntry, unsigned,
                       SubstitutionEntry::Hasher> Substitutions;
  public:
    Remangler(std::ostringstream &out) : Out(out) {}

    class EntityContext {
      Remangler &R;
      unsigned SavedAbsoluteDepth;
      bool AsContext = false;
    public:
      EntityContext(Remangler &R)
        : R(R), SavedAbsoluteDepth(R.AbsoluteArchetypeDepth) {
      }

      ~EntityContext() {
        assert(R.AbsoluteArchetypeDepth >= SavedAbsoluteDepth);
        R.AbsoluteArchetypeDepth = SavedAbsoluteDepth;
      }

      bool isAsContext() const {
        return AsContext;
      }

      class ManglingContextRAII {
        EntityContext &Ctx;
        bool SavedValue;
      public:
        ManglingContextRAII(EntityContext &ctx)
          : Ctx(ctx), SavedValue(ctx.AsContext) {
          ctx.AsContext = true;
        }

        ~ManglingContextRAII() {
          Ctx.AsContext = SavedValue;
        }
      };
    };

    void mangle(Node *node) {
      switch (node->getKind()) {
#define NODE(ID) case Node::Kind::ID: return mangle##ID(node);
#include "swift/Basic/DemangleNodes.def"
      }
      unreachable("bad demangling tree node");
    }

#define NODE(ID)                                                        \
    void mangle##ID(Node *node);
#define CONTEXT_NODE(ID)                                                \
    void mangle##ID(Node *node);                                        \
    void mangle##ID(Node *node, EntityContext &ctx);
#include "swift/Basic/DemangleNodes.def"

    void mangleIndex(Node::IndexType index);
    void mangleIdentifier(StringRef name, OperatorKind operatorKind);

    void mangleChildNodes(Node *node) { mangleNodes(node->begin(), node->end()); }
    void mangleNodes(Node::iterator i, Node::iterator e) {
      for (; i != e; ++i) {
        mangle(i->get());
      }
    }
    void mangleSingleChildNode(Node *node) {
      assert(node->getNumChildren() == 1);
      mangle(node->begin()->get());
    }
    void mangleChildNode(Node *node, unsigned index) {
      assert(index < node->getNumChildren());
      mangle(node->begin()[index].get());
    }

    void mangleSimpleEntity(Node *node, char basicKind, StringRef entityKind,
                            EntityContext &ctx);
    void mangleNamedEntity(Node *node, char basicKind, StringRef entityKind,
                           EntityContext &ctx);
    void mangleTypedEntity(Node *node, char basicKind, StringRef entityKind,
                           EntityContext &ctx);
    void mangleNamedAndTypedEntity(Node *node, char basicKind,
                                   StringRef entityKind,
                                   EntityContext &ctx);
    void mangleNominalType(Node *node, char basicKind, EntityContext &ctx);

    void mangleProtocolWithoutPrefix(Node *node);
    void mangleProtocolListWithoutPrefix(Node *node);

    void mangleEntityContext(Node *node, EntityContext &ctx);
    void mangleEntityType(Node *node, EntityContext &ctx);
    void mangleEntityGenericType(Node *node, EntityContext &ctx);
    void mangleGenerics(Node *node, EntityContext &ctx);

    bool trySubstitution(Node *node, SubstitutionEntry &entry);
    void addSubstitution(const SubstitutionEntry &entry);
    void resetSubstitutions();
  };
}

#define NODE(ID)
#define CONTEXT_NODE(ID)                        \
void Remangler::mangle##ID(Node *node) {        \
  EntityContext ctx(*this);                     \
  mangle##ID(node, ctx);                        \
}
#include "swift/Basic/DemangleNodes.def"

/// Reset the currently-active set of substitutions.  This is useful
/// when part of the mangling is done independently, e.g. when an
/// optimization pass modifies a pass.
void Remangler::resetSubstitutions() {
  Substitutions.clear();
}

bool Remangler::trySubstitution(Node *node, SubstitutionEntry &entry) {
  auto isInSwiftModule = [](Node *node) -> bool {
    auto context = node->begin()->get();
    return (context->getKind() == Node::Kind::Module &&
            context->getText() == STDLIB_NAME);
  };

  // Look for known substitutions.
  switch (node->getKind()) {
#define SUCCESS_IF_IS(VALUE, EXPECTED, SUBSTITUTION)            \
    do {                                                        \
      if ((VALUE) == (EXPECTED)) {                              \
        Out << SUBSTITUTION;                                    \
        return true;                                            \
      }                                                         \
    } while (0)
#define SUCCESS_IF_TEXT_IS(EXPECTED, SUBSTITUTION)              \
  SUCCESS_IF_IS(node->getText(), EXPECTED, SUBSTITUTION)
#define SUCCESS_IF_DECLNAME_IS(EXPECTED, SUBSTITUTION)          \
  SUCCESS_IF_IS(node->getChild(1)->getText(), EXPECTED, SUBSTITUTION)

  case Node::Kind::Module:
    SUCCESS_IF_TEXT_IS("ObjectiveC", "So");
    SUCCESS_IF_TEXT_IS("C", "SC");
    SUCCESS_IF_TEXT_IS(STDLIB_NAME, "Ss");
    break;
  case Node::Kind::Structure:
    if (isInSwiftModule(node)) {
      SUCCESS_IF_DECLNAME_IS("Array", "Sa");
      SUCCESS_IF_DECLNAME_IS("Bool", "Sb");
      SUCCESS_IF_DECLNAME_IS("UnicodeScalar", "Sc");
      SUCCESS_IF_DECLNAME_IS("Double", "Sd");
      SUCCESS_IF_DECLNAME_IS("Float", "Sf");
      SUCCESS_IF_DECLNAME_IS("Int", "Si");
      SUCCESS_IF_DECLNAME_IS("String", "SS");
      SUCCESS_IF_DECLNAME_IS("UInt", "Su");
    }
    break;
  case Node::Kind::Enum:
    if (isInSwiftModule(node)) {
      SUCCESS_IF_DECLNAME_IS("Optional", "Sq");
      SUCCESS_IF_DECLNAME_IS("ImplicitlyUnwrappedOptional", "SQ");
    }
    break;

  default:
    break;

#undef SUCCESS_IF_DECLNAME_IS
#undef SUCCESS_IF_TEXT_IS
#undef SUCCESS_IF_IS
  }

  // Go ahead and initialize the substitution entry.
  entry.TheNode = node;
  entry.StoredHash = deepHash(node);

  auto it = Substitutions.find(entry);
  if (it == Substitutions.end())
    return false;

  Out << 'S';
  mangleIndex(it->second);
  return true;
}

void Remangler::addSubstitution(const SubstitutionEntry &entry) {
  auto result = Substitutions.insert({entry, Substitutions.size()});
  assert(result.second);
  (void) result;
}

void Remangler::mangleIdentifier(Node *node) {
  mangleIdentifier(node->getText(), OperatorKind::NotOperator);
}
void Remangler::manglePrefixOperator(Node *node) {
  mangleIdentifier(node->getText(), OperatorKind::Prefix);
}
void Remangler::manglePostfixOperator(Node *node) {
  mangleIdentifier(node->getText(), OperatorKind::Postfix);
}
void Remangler::mangleInfixOperator(Node *node) {
  mangleIdentifier(node->getText(), OperatorKind::Infix);
}
void Remangler::mangleIdentifier(StringRef ident, OperatorKind operatorKind) {
  ::mangleIdentifier(ident, operatorKind, /*usePunycode*/ true, Out);
}

void Remangler::mangleNumber(Node *node) {
  mangleIndex(node->getIndex());
}
void Remangler::mangleIndex(Node::IndexType value) {
  if (value == 0) {
    Out << '_';
  } else {
    Out << (value - 1) << '_';
  }
}

void Remangler::mangleGlobal(Node *node) {
  Out << "_T";
  mangleChildNodes(node);
}

void Remangler::mangleSuffix(Node *node) {
  // Just add the suffix back on.
  Out << node->getText();
}

void Remangler::mangleGenericSpecialization(Node *node) {
  Out << "TSg";
  mangleChildNodes(node); // GenericSpecializationParams

  // Specializations are just prepended to already-mangled names.
  resetSubstitutions();

  // Start another mangled name.
  Out << "__T";
}
void Remangler::mangleGenericSpecializationParam(Node *node) {
  // Should be a type followed by a series of protocol conformances.
  mangleChildNodes(node);
  Out << '_';
}

void Remangler::mangleFunctionSignatureSpecialization(Node *node) {
  Out << "TSf";
  mangleChildNodes(node); // FunctionSignatureSpecializationParams

  // Specializations are just prepended to already-mangled names.
  resetSubstitutions();

  // Start another mangled name.
  Out << "__T";
}

void Remangler::mangleSpecializationPassID(Node *node) {
  Out << node->getIndex();
}

void Remangler::mangleFunctionSignatureSpecializationParam(Node *node) {
  if (!node->hasChildren()) {
    Out << "n_";
    return;
  }

  // The first child is always a kind that specifies the type of param that we
  // have.
  NodePointer firstChild = node->getChild(0);
  unsigned kindValue = firstChild->getIndex();
  auto kind = FunctionSigSpecializationParamKind(kindValue);

  switch (kind) {
  case FunctionSigSpecializationParamKind::Dead:
    Out << "d_";
    return;
  case FunctionSigSpecializationParamKind::ConstantPropFunction:
    Out << "cpfr";
    mangleIdentifier(node->getChild(1).get());
    Out << '_';
    return;
  case FunctionSigSpecializationParamKind::ConstantPropGlobal:
    Out << "cpg";
    mangleIdentifier(node->getChild(1).get());
    Out << '_';
    return;
  case FunctionSigSpecializationParamKind::ConstantPropInteger:
    Out << "cpi" << node->getChild(1)->getText() << '_';
    return;
  case FunctionSigSpecializationParamKind::ConstantPropFloat:
    Out << "cpfl" << node->getChild(1)->getText() << '_';
    return;
  case FunctionSigSpecializationParamKind::ConstantPropString: {
    Out << "cpse";
    StringRef encodingStr = node->getChild(1)->getText();
    if (encodingStr == "u8")
      Out << '0';
    else if (encodingStr == "u16")
      Out << '1';
    else
      unreachable("Unknown encoding");
    Out << 'v';
    mangleIdentifier(node->getChild(2).get());
    Out << '_';
    return;
  }
  case FunctionSigSpecializationParamKind::ClosureProp:
    Out << "cl";
    mangleIdentifier(node->getChild(1).get());
    for (unsigned i = 2, e = node->getNumChildren(); i != e; ++i) {
      mangleType(node->getChild(i).get());
    }
    Out << '_';
    return;
  case FunctionSigSpecializationParamKind::InOutToValue:
    Out << "i_";
    return;
  default:
    if (kindValue &
        unsigned(FunctionSigSpecializationParamKind::OwnedToGuaranteed))
      Out << 'g';
    if (kindValue & unsigned(FunctionSigSpecializationParamKind::SROA))
      Out << 's';
    Out << '_';
    return;
  }
}

void Remangler::mangleFunctionSignatureSpecializationParamPayload(Node *node) {
  // This should never be called since mangling parameter payloads require
  // knowing what the parameter kind is.
  unreachable("This should never be called");
}

void Remangler::mangleFunctionSignatureSpecializationParamKind(Node *node) {
  // This should never be called since mangling parameter kinds have influence
  // on the payloads.
  unreachable("This should never be called");
}

void Remangler::mangleProtocolConformance(Node *node) {
  // type, protocol name, context
  assert(node->getNumChildren() == 3);
  mangleChildNode(node, 0);
  mangleProtocolWithoutPrefix(node->begin()[1].get());
  mangleChildNode(node, 2);
}

void Remangler::mangleObjCAttribute(Node *node) {
  Out << "To";
}

void Remangler::mangleNonObjCAttribute(Node *node) {
  Out << "TO";
}

void Remangler::mangleDynamicAttribute(Node *node) {
  Out << "TD";
}

void Remangler::mangleVTableAttribute(Node *node) {
  Out << "TV";
}

void Remangler::mangleGenericTypeMetadataPattern(Node *node) {
  Out << "MP";
  mangleChildNodes(node); // directness, type
}

void Remangler::mangleTypeMetadataAccessFunction(Node *node) {
  Out << "Ma";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleTypeMetadataLazyCache(Node *node) {
  Out << "ML";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleMetaclass(Node *node) {
  Out << "Mm";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleNominalTypeDescriptor(Node *node) {
  Out << "Mn";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleTypeMetadata(Node *node) {
  Out << "M";
  mangleChildNodes(node); // directness, type
}

void Remangler::manglePartialApplyForwarder(Node *node) {
  Out << "PA__T";
  mangleSingleChildNode(node); // global
}

void Remangler::manglePartialApplyObjCForwarder(Node *node) {
  Out << "PAo__T";
  mangleSingleChildNode(node); // global
}

void Remangler::mangleDirectness(Node *node) {
  auto getChar = [](Directness d) -> char {
    switch (d) {
    case Directness::Direct: return 'd';
    case Directness::Indirect: return 'i';
    }
    unreachable("bad directness kind");
  };
  Out << getChar(Directness(node->getIndex()));
}

void Remangler::mangleValueWitness(Node *node) {
  auto getString = [](ValueWitnessKind kind) -> StringRef {
    switch (kind) {
    case ValueWitnessKind::AllocateBuffer: return "al";
    case ValueWitnessKind::AssignWithCopy: return "ca";
    case ValueWitnessKind::AssignWithTake: return "ta";
    case ValueWitnessKind::DeallocateBuffer: return "de";
    case ValueWitnessKind::Destroy: return "xx";
    case ValueWitnessKind::DestroyBuffer: return "XX";
    case ValueWitnessKind::InitializeBufferWithCopyOfBuffer: return "CP";
    case ValueWitnessKind::InitializeBufferWithCopy: return "Cp";
    case ValueWitnessKind::InitializeWithCopy: return "cp";
    case ValueWitnessKind::InitializeBufferWithTake: return "Tk";
    case ValueWitnessKind::InitializeWithTake: return "tk";
    case ValueWitnessKind::ProjectBuffer: return "pr";
    case ValueWitnessKind::InitializeBufferWithTakeOfBuffer: return "TK";
    case ValueWitnessKind::DestroyArray: return "Xx";
    case ValueWitnessKind::InitializeArrayWithCopy: return "Cc";
    case ValueWitnessKind::InitializeArrayWithTakeFrontToBack: return "Tt";
    case ValueWitnessKind::InitializeArrayWithTakeBackToFront: return "tT";
    case ValueWitnessKind::StoreExtraInhabitant: return "xs";
    case ValueWitnessKind::GetExtraInhabitantIndex: return "xg";
    case ValueWitnessKind::GetEnumTag: return "ug";
    case ValueWitnessKind::InplaceProjectEnumData: return "up";
    }
    unreachable("bad value witness kind");
  };
  Out << 'w' << getString(ValueWitnessKind(node->getIndex()));
  mangleSingleChildNode(node); // type
}

void Remangler::mangleValueWitnessTable(Node *node) {
  Out << "WV";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleWitnessTableOffset(Node *node) {
  Out << "Wo";
  mangleSingleChildNode(node); // entity
}

void Remangler::mangleFieldOffset(Node *node) {
  Out << "Wv";
  mangleChildNodes(node); // directness, entity
}

void Remangler::mangleProtocolWitnessTable(Node *node) {
  Out << "WP";
  mangleSingleChildNode(node); // protocol conformance
}

void Remangler::mangleLazyProtocolWitnessTableAccessor(Node *node) {
  Out << "WZ";
  mangleSingleChildNode(node); // protocol conformance
}

void Remangler::mangleLazyProtocolWitnessTableTemplate(Node *node) {
  Out << "Wz";
  mangleSingleChildNode(node); // protocol conformance
}

void Remangler::mangleDependentProtocolWitnessTableGenerator(Node *node) {
  Out << "WD";
  mangleSingleChildNode(node); // protocol conformance
}

void Remangler::mangleDependentProtocolWitnessTableTemplate(Node *node) {
  Out << "Wd";
  mangleSingleChildNode(node); // protocol conformance
}

void Remangler::mangleReabstractionThunkHelper(Node *node) {
  Out << "TR";
  if (node->getNumChildren() == 3) Out << 'G';
  mangleChildNodes(node); // generic signature?, type, type
}

void Remangler::mangleReabstractionThunk(Node *node) {
  Out << "Tr";
  if (node->getNumChildren() == 3) Out << 'G';
  mangleChildNodes(node); // generic signature?, type, type
}

void Remangler::mangleProtocolWitness(Node *node) {
  Out << "TW";
  mangleChildNodes(node); // protocol conformance, entity
}

void Remangler::mangleFunction(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "", ctx);
}

void Remangler::mangleVariable(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'v', "", ctx);
}

void Remangler::mangleSubscript(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 's', "", ctx);
}

void Remangler::mangleInitializer(Node *node, EntityContext &ctx) {
  mangleSimpleEntity(node, 'I', "i", ctx);
}

void Remangler::mangleDefaultArgumentInitializer(Node *node,
                                                 EntityContext &ctx) {
  mangleNamedEntity(node, 'I', "A", ctx);
}

void Remangler::mangleDeallocator(Node *node, EntityContext &ctx) {
  mangleSimpleEntity(node, 'F', "D", ctx);
}

void Remangler::mangleDestructor(Node *node, EntityContext &ctx) {
  mangleSimpleEntity(node, 'F', "d", ctx);
}

void Remangler::mangleAllocator(Node *node, EntityContext &ctx) {
  mangleTypedEntity(node, 'F', "C", ctx);
}

void Remangler::mangleConstructor(Node *node, EntityContext &ctx) {
  mangleTypedEntity(node, 'F', "c", ctx);
}

void Remangler::mangleIVarInitializer(Node *node, EntityContext &ctx) {
  mangleSimpleEntity(node, 'F', "e", ctx);
}

void Remangler::mangleIVarDestroyer(Node *node, EntityContext &ctx) {
  mangleSimpleEntity(node, 'F', "E", ctx);
}

void Remangler::mangleGetter(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "g", ctx);
}

void Remangler::mangleGlobalGetter(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "G", ctx);
}

void Remangler::mangleSetter(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "s", ctx);
}

void Remangler::mangleMaterializeForSet(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "m", ctx);
}

void Remangler::mangleWillSet(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "w", ctx);
}

void Remangler::mangleDidSet(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "W", ctx);
}

void Remangler::mangleOwningMutableAddressor(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "aO", ctx);
}

void Remangler::mangleNativeOwningMutableAddressor(Node *node,
                                                   EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "ao", ctx);
}

void Remangler::mangleNativePinningMutableAddressor(Node *node,
                                                    EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "ap", ctx);
}

void Remangler::mangleUnsafeMutableAddressor(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "au", ctx);
}

void Remangler::mangleOwningAddressor(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "lO", ctx);
}

void Remangler::mangleNativeOwningAddressor(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "lo", ctx);
}

void Remangler::mangleNativePinningAddressor(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "lp", ctx);
}

void Remangler::mangleUnsafeAddressor(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "lu", ctx);
}

void Remangler::mangleExplicitClosure(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "U", ctx); // name is index
}

void Remangler::mangleImplicitClosure(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "u", ctx); // name is index
}

void Remangler::mangleStatic(Node *node) {
  Out << 'Z';
  mangle(node->getChild(0).get());
}

void Remangler::mangleSimpleEntity(Node *node, char basicKind,
                                   StringRef entityKind,
                                   EntityContext &ctx) {
  assert(node->getNumChildren() == 1);
  Out << basicKind;
  mangleEntityContext(node->begin()[0].get(), ctx);
  Out << entityKind;
}

void Remangler::mangleNamedEntity(Node *node, char basicKind,
                                  StringRef entityKind,
                                  EntityContext &ctx) {
  assert(node->getNumChildren() == 2);
  if (basicKind != '\0') Out << basicKind;
  mangleEntityContext(node->begin()[0].get(), ctx);
  Out << entityKind;
  mangleChildNode(node, 1); // decl name / index
}

void Remangler::mangleTypedEntity(Node *node, char basicKind,
                                  StringRef entityKind,
                                  EntityContext &ctx) {
  assert(node->getNumChildren() == 2);
  Out << basicKind;
  mangleEntityContext(node->begin()[0].get(), ctx);
  Out << entityKind;
  mangleEntityType(node->begin()[1].get(), ctx);
}

void Remangler::mangleNamedAndTypedEntity(Node *node, char basicKind,
                                          StringRef entityKind,
                                          EntityContext &ctx) {
  assert(node->getNumChildren() == 3);
  Out << basicKind;
  mangleEntityContext(node->begin()[0].get(), ctx);
  Out << entityKind;
  mangleChildNode(node, 1); // decl name / index
  mangleEntityType(node->begin()[2].get(), ctx);
}

void Remangler::mangleEntityContext(Node *node, EntityContext &ctx) {
  // Remember that we're mangling a context.
  EntityContext::ManglingContextRAII raii(ctx);

  switch (node->getKind()) {
#define NODE(ID)                                \
  case Node::Kind::ID:
#define CONTEXT_NODE(ID)
#include "swift/Basic/DemangleNodes.def"
    unreachable("not a context node");

#define NODE(ID)
#define CONTEXT_NODE(ID)                        \
  case Node::Kind::ID:                          \
    return mangle##ID(node, ctx);
#include "swift/Basic/DemangleNodes.def"
  }
  unreachable("bad node kind");
}

void Remangler::mangleEntityType(Node *node, EntityContext &ctx) {
  assert(node->getKind() == Node::Kind::Type);
  assert(node->getNumChildren() == 1);
  node = node->begin()[0].get();

  // Expand certain kinds of type within the entity context.
  switch (node->getKind()) {
  case Node::Kind::GenericType:
    mangleEntityGenericType(node, ctx);
    return;
  case Node::Kind::FunctionType:
  case Node::Kind::UncurriedFunctionType: {
    Out << (node->getKind() == Node::Kind::FunctionType ? 'F' : 'f');
    mangle(node->begin()[0].get());
    auto returnType = node->begin()[1].get();
    assert(returnType->getKind() == Node::Kind::ReturnType);
    assert(returnType->getNumChildren() == 1);
    mangleEntityType(returnType->begin()[0].get(), ctx);
    return;
  }
  default:
    mangle(node);
    return;
  }
}

void Remangler::mangleLocalDeclName(Node *node) {
  Out << 'L';
  mangleChildNodes(node); // index, identifier
}

void Remangler::manglePrivateDeclName(Node *node) {
  Out << 'P';
  mangleChildNodes(node); // identifier, identifier
}

void Remangler::mangleTypeMangling(Node *node) {
  Out << 't';
  mangleSingleChildNode(node); // type
}

void Remangler::mangleType(Node *node) {
  mangleSingleChildNode(node);
}

template <size_t N> 
static bool stripPrefix(StringRef &string, const char (&data)[N]) {
  constexpr size_t prefixLength = N - 1;
  if (!string.startswith(StringRef(data, prefixLength)))
    return false;
  string = string.drop_front(prefixLength);
  return true;
}

void Remangler::mangleBuiltinTypeName(Node *node) {
  Out << 'B';
  StringRef text = node->getText();

  if (text == "Builtin.BridgeObject") {
    Out << 'b';
  } else if (text == "Builtin.UnsafeValueBuffer") {
    Out << 'B';
  } else if (text == "Builtin.UnknownObject") {
    Out << 'O';
  } else if (text == "Builtin.NativeObject") {
    Out << 'o';
  } else if (text == "Builtin.RawPointer") {
    Out << 'p';
  } else if (text == "Builtin.Word") {
    Out << 'w';
  } else if (stripPrefix(text, "Builtin.Int")) {
    Out << 'i' << text << '_';
  } else if (stripPrefix(text, "Builtin.Float")) {
    Out << 'f' << text << '_';
  } else if (stripPrefix(text, "Builtin.Vec")) {
    auto split = text.split('x');
    Out << 'v' << split.first << 'B';
    if (split.second == "RawPointer") {
      Out << 'p';
    } else if (stripPrefix(split.second, "Float")) {
      Out << 'f' << split.second << '_';
    } else if (stripPrefix(split.second, "Int")) {
      Out << 'i' << split.second << '_';
    } else {
      unreachable("unexpected builtin vector type");
    }
  } else {
    unreachable("unexpected builtin type");
  }
}

void Remangler::mangleTypeAlias(Node *node) {
  SubstitutionEntry entry;
  if (trySubstitution(node, entry)) return;
  Out << 'a';
  mangleChildNodes(node); // context, identifier
  addSubstitution(entry);
}

void Remangler::mangleFunctionType(Node *node) {
  Out << 'F';
  mangleChildNodes(node); // argument tuple, result type
}

void Remangler::mangleUncurriedFunctionType(Node *node) {
  Out << 'f';
  mangleChildNodes(node); // argument tuple, result type
}

void Remangler::mangleObjCBlock(Node *node) {
  Out << 'b';
  mangleChildNodes(node); // argument tuple, result type
}

void Remangler::mangleAutoClosureType(Node *node) {
  Out << 'K';
  mangleChildNodes(node); // argument tuple, result type
}

void Remangler::mangleThinFunctionType(Node *node) {
  Out << "Xf";
  mangleChildNodes(node); // argument tuple, result type
}

void Remangler::mangleArgumentTuple(Node *node) {
  mangleSingleChildNode(node);
}

void Remangler::mangleReturnType(Node *node) {
  mangleSingleChildNode(node);
}

void Remangler::mangleImplFunctionType(Node *node) {
  Out << "XF";
  auto i = node->begin(), e = node->end();
  if (i != e && i->get()->getKind() == Node::Kind::ImplConvention) {
    StringRef text = (i++)->get()->getText();
    if (text == "@callee_unowned") {
      Out << 'd';
    } else if (text == "@callee_guaranteed") {
      Out << 'g';
    } else if (text == "@callee_owned") {
      Out << 'o';
    } else {
      unreachable("bad callee convention");
    }
  } else {
    Out << 't';
  }
  for (; i != e &&
         i->get()->getKind() == Node::Kind::ImplFunctionAttribute; ++i) {
    mangle(i->get()); // impl function attribute
  }
  EntityContext ctx(*this);
  if (i != e && i->get()->getKind() == Node::Kind::Generics) {
    mangleGenerics((i++)->get(), ctx);
  }
  Out << '_';
  for (; i != e && i->get()->getKind() == Node::Kind::ImplParameter; ++i) {
    mangleImplParameter(i->get());
  }
  Out << '_';
  mangleNodes(i, e); // impl results
  Out << '_';
}

void Remangler::mangleImplFunctionAttribute(Node *node) {
  StringRef text = node->getText();
  if (text == "@objc_block") {
    Out << "Cb";
  } else if (text == "@cc(cdecl)") {
    Out << "Cc";
  } else if (text == "@cc(method)") {
    Out << "Cm";
  } else if (text == "@cc(objc_method)") {
    Out << "CO";
  } else if (text == "@cc(witness_method)") {
    Out << "Cw";
  } else if (text == "@noreturn") {
    Out << "CN";
  } else {
    unreachable("bad impl-function-attribute");
  }
}

void Remangler::mangleImplParameter(Node *node) {
  assert(node->getNumChildren() == 2);
  mangleChildNodes(node); // impl convention, type
}

void Remangler::mangleImplResult(Node *node) {
  assert(node->getNumChildren() == 2);
  mangleChildNodes(node); // impl convention, type
}

void Remangler::mangleImplConvention(Node *node) {
  assert(node->getKind() == Node::Kind::ImplConvention);
  StringRef text = node->getText();
  if (text == "@autoreleased") {
    Out << 'a';
  } else if (text == "@unowned") {
    Out << 'd';
  } else if (text == "@unowned_inner_pointer") {
    Out << 'd'; // only in results
  } else if (text == "@guaranteed") {
    Out << 'g';
  } else if (text == "@in") {
    Out << 'i'; // only in parameters
  } else if (text == "@out") {
    Out << 'i'; // only in results
  } else if (text == "@inout") {
    Out << 'l';
  } else if (text == "@owned") {
    Out << 'o';
  } else {
    unreachable("invalid impl convention");
  }
}

void Remangler::mangleDynamicSelf(Node *node) {
  Out << 'D';
  mangleSingleChildNode(node); // type
}

void Remangler::mangleErrorType(Node *node) {
  Out << "ERR";
}

void Remangler::mangleMetatype(Node *node) {
  if (node->getNumChildren() == 1) {
    Out << 'M';
    mangleSingleChildNode(node); // type
  } else {
    assert(node->getNumChildren() == 2);
    Out << "XM";
    mangleChildNodes(node); // metatype representation, type
  }
}

void Remangler::mangleExistentialMetatype(Node *node) {
  if (node->getNumChildren() == 1) {
    Out << "PM";
    mangleSingleChildNode(node); // type
  } else {
    assert(node->getNumChildren() == 2);
    Out << "XPM";
    mangleChildNodes(node); // metatype representation, type
  }
}

void Remangler::mangleMetatypeRepresentation(Node *node) {
  StringRef text = node->getText();
  if (text == "@thin") {
    Out << 't';
  } else if (text == "@thick") {
    Out << 'T';
  } else if (text == "@objc") {
    Out << 'o';
  } else {
    unreachable("bad metatype representation");
  }
}

void Remangler::mangleProtocolList(Node *node) {
  // In its usual use as a type, this gets a prefix 'P'.
  Out << 'P';
  mangleProtocolListWithoutPrefix(node);
}

void Remangler::mangleProtocolListWithoutPrefix(Node *node) {
  assert(node->getKind() == Node::Kind::ProtocolList);
  assert(node->getNumChildren() == 1);
  auto typeList = node->begin()[0].get();
  assert(typeList->getKind() == Node::Kind::TypeList);
  for (auto &child : *typeList) {
    mangleProtocolWithoutPrefix(child.get());
  }
  Out << '_';
}

void Remangler::mangleUnowned(Node *node) {
  Out << "Xo";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleUnmanaged(Node *node) {
  Out << "Xu";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleWeak(Node *node) {
  Out << "Xw";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleInOut(Node *node) {
  Out << 'R';
  mangleSingleChildNode(node); // type
}

void Remangler::mangleNonVariadicTuple(Node *node) {
  Out << 'T';
  mangleChildNodes(node); // tuple elements
  Out << '_';
}

void Remangler::mangleVariadicTuple(Node *node) {
  Out << 't';
  mangleChildNodes(node); // tuple elements
  Out << '_';
}

void Remangler::mangleTupleElement(Node *node) {
  mangleChildNodes(node); // tuple element name?, type
}

void Remangler::mangleTupleElementName(Node *node) {
  mangleIdentifier(node->getText(), OperatorKind::NotOperator);
}

void Remangler::mangleDependentGenericType(Node *node) {
  Out << 'u';
  mangleChildNodes(node); // generic signature, type
}

void Remangler::mangleGenericType(Node *node) {
  EntityContext ctx(*this);
  mangleEntityGenericType(node, ctx);
}

void Remangler::mangleEntityGenericType(Node *node, EntityContext &ctx) {
  assert(node->getKind() == Node::Kind::GenericType);

  Out << 'U';
  assert(node->getNumChildren() == 2);

  mangleGenerics(node->begin()[0].get(), ctx);
  mangleEntityType(node->begin()[1].get(), ctx);
}

void Remangler::mangleDependentGenericSignature(Node *node) {
  auto i = node->begin(), e = node->end();
  for (; i != e &&
         i->get()->getKind() == Node::Kind::DependentGenericParamCount; ++i) {
    auto count = i->get();
    mangleIndex(count->getIndex());
  }
  Out << 'R';
  mangleNodes(i, e); // generic requirements
  Out << '_';
}

void Remangler::mangleDependentGenericParamCount(Node *node) {
  unreachable("handled inline in DependentGenericSignature");
}

void Remangler::mangleDependentGenericConformanceRequirement(Node *node) {
  Out << 'P';
  mangleChildNodes(node); // type, type
}

void Remangler::mangleDependentGenericSameTypeRequirement(Node *node) {
  Out << 'E';
  mangleChildNodes(node); // type, type
}

void Remangler::mangleGenerics(Node *node) {
  unreachable("found independent generics node?");
}

void Remangler::mangleGenerics(Node *node, EntityContext &ctx) {
  assert(node->getKind() == Node::Kind::Generics);

  unsigned absoluteDepth = ++AbsoluteArchetypeDepth;

  auto i = node->begin(), e = node->end();
  unsigned index = 0;
  for (; i != e && i->get()->getKind() == Node::Kind::Archetype; ++i) {
    auto child = i->get();
    Archetypes[child->getText()] = ArchetypeInfo{index++, absoluteDepth};
    mangle(child); // archetype
  }
  if (i != e) {
    Out << 'U';
    mangleNodes(i, e); // associated types
    Out << '_';
  } else {
    Out << '_';
  }
}

void Remangler::mangleArchetype(Node *node) {
  if (node->hasChildren()) {
    assert(node->getNumChildren() == 1);
    mangleProtocolListWithoutPrefix(node->begin()->get());
  } else {
    Out << '_';
  }
}

void Remangler::mangleAssociatedType(Node *node) {
  if (node->hasChildren()) {
    assert(node->getNumChildren() == 1);
    mangleProtocolListWithoutPrefix(node->begin()->get());
  } else {
    Out << '_';
  }
}

void Remangler::mangleSelfTypeRef(Node *node) {
  SubstitutionEntry entry;
  if (trySubstitution(node, entry)) return;
  Out << "QP";
  assert(node->getNumChildren() == 1);
  mangleProtocolWithoutPrefix(node->begin()[0].get());
  addSubstitution(entry);
}

void Remangler::mangleArchetypeRef(Node *node) {
  auto it = Archetypes.find(node->getText());
  assert(it != Archetypes.end());

  ArchetypeInfo info = it->second;
  assert(AbsoluteArchetypeDepth >= info.AbsoluteDepth);

  Node::IndexType relativeDepth = (AbsoluteArchetypeDepth - info.AbsoluteDepth);
  Node::IndexType index = info.Index;

  Out << 'Q';
  if (relativeDepth != 0) {
    Out << 'd';
    mangleIndex(relativeDepth - 1);
  }
  mangleIndex(index);
}

void Remangler::mangleQualifiedArchetype(Node *node) {
  Out << "Qq";
  mangleChildNodes(node); // index, declcontext
}

void Remangler::mangleDeclContext(Node *node) {
  mangleSingleChildNode(node);
}

void Remangler::mangleExtension(Node *node, EntityContext &ctx) {
  Out << 'E';
  assert(node->getNumChildren() == 2);
  mangleEntityContext(node->begin()[0].get(), ctx); // module
  mangleEntityContext(node->begin()[1].get(), ctx); // context
}

void Remangler::mangleModule(Node *node, EntityContext &ctx) {
  SubstitutionEntry entry;
  if (trySubstitution(node, entry)) return;

  // Module types get an M prefix, but module contexts don't.
  if (!ctx.isAsContext()) Out << 'M';
  mangleIdentifier(node->getText(), OperatorKind::NotOperator);
  addSubstitution(entry);
}

void Remangler::mangleAssociatedTypeRef(Node *node) {
  SubstitutionEntry entry;
  if (trySubstitution(node, entry)) return;
  Out << "Q";
  mangleChildNodes(node); // type, identifier
  addSubstitution(entry);
}

void Remangler::mangleDependentMemberType(Node *node) {
  Out << 'q';
  mangleSingleChildNode(node); // type
  mangleIdentifier(node->getText(), OperatorKind::NotOperator);
}

void Remangler::mangleDependentGenericParamType(Node *node) {
  Out << 'q';
  StringRef text = node->getText();
  bool stripped = stripPrefix(text, "T_");
  assert(stripped); (void) stripped;
  auto split = text.split('_');
  auto depth = (Node::IndexType) atoi(split.first.data());
  auto index = (Node::IndexType) atoi(split.second.data());
  if (depth != 0) {
    Out << 'd';
    mangleIndex(depth - 1);
  }
  mangleIndex(index);
}

void Remangler::mangleProtocol(Node *node, EntityContext &ctx) {
  mangleNominalType(node, 'P', ctx);
}

void Remangler::mangleProtocolWithoutPrefix(Node *node) {
  if (node->getKind() == Node::Kind::Type) {
    assert(node->getNumChildren() == 1);
    node = node->begin()[0].get();
  }

  assert(node->getKind() == Node::Kind::Protocol);
  EntityContext ctx(*this);
  mangleNominalType(node, '\0', ctx);
}

void Remangler::mangleStructure(Node *node, EntityContext &ctx) {
  mangleNominalType(node, 'V', ctx);
}

void Remangler::mangleEnum(Node *node, EntityContext &ctx) {
  mangleNominalType(node, 'O', ctx);
}

void Remangler::mangleClass(Node *node, EntityContext &ctx) {
  mangleNominalType(node, 'C', ctx);
}

void Remangler::mangleNominalType(Node *node, char kind, EntityContext &ctx) {
  SubstitutionEntry entry;
  if (trySubstitution(node, entry)) return;
  mangleNamedEntity(node, kind, "", ctx);
  addSubstitution(entry);
}

void Remangler::mangleBoundGenericClass(Node *node) {
  Out << 'G';
  mangleChildNodes(node); // type, type list
}

void Remangler::mangleBoundGenericStructure(Node *node) {
  Out << 'G';
  mangleChildNodes(node); // type, type list
}

void Remangler::mangleBoundGenericEnum(Node *node) {
  Out << 'G';
  mangleChildNodes(node); // type, type list
}

void Remangler::mangleTypeList(Node *node) {
  mangleChildNodes(node); // all types
  Out << '_';
}

/// The top-level interface to the remangler.
std::string Demangle::mangleNode(const NodePointer &node) {
  if (!node) return "";

  std::ostringstream str;
  Remangler(str).mangle(node.get());
  return str.str();
}
