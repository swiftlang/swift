//===--- NodePrinter.cpp - Swift Demangling Node Printer ------------------===//
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
//  This file implements the node printer for demangle node trees.
//
//===----------------------------------------------------------------------===//

#include "swift/Demangling/Demangle.h"
#include "swift/Strings.h"
#include <cstdio>
#include <cstdlib>

using namespace swift;
using namespace Demangle;
using llvm::StringRef;

[[noreturn]]
static void printer_unreachable(const char *Message) {
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

std::string Demangle::archetypeName(Node::IndexType index,
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

struct QuotedString {
  std::string Value;

  explicit QuotedString(std::string Value) : Value(Value) {}
};

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

static StringRef toString(Directness d) {
  switch (d) {
  case Directness::Direct:
    return "direct";
  case Directness::Indirect:
    return "indirect";
  }
  printer_unreachable("bad directness");
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
  printer_unreachable("bad value witness kind");
}

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
    printer_unreachable("bad node kind");
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
        case Results: printer_unreachable("no state after Results");
        }
        printer_unreachable("bad state");
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
    printer_unreachable("not an entity");
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
      printer_unreachable("option sets should have been handled earlier");
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
    printer_unreachable("should be printed as a child of a "
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
    } else if (c == 'C') {
      name = "_Class";
    } else if (c == 'D') {
      name = "_NativeClass";
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
  printer_unreachable("bad node kind!");
}

std::string Demangle::nodeToString(NodePointer root,
                                   const DemangleOptions &options) {
  if (!root)
    return "";

  return NodePrinter(options).printRoot(root);
}
