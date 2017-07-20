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
  bool SpecializationPrefixPrinted = false;

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
  
  void printChildren(NodePointer Node, const char *sep = nullptr) {
    if (!Node)
      return;
    Node::iterator begin = Node->begin(), end = Node->end();
    printChildren(begin, end, sep);
  }
  
  NodePointer getFirstChildOfKind(NodePointer Node, Node::Kind kind) {
    if (!Node)
      return nullptr;
    for (NodePointer &child : *Node) {
      if (child && child->getKind() == kind)
        return child;
    }
    return nullptr;
  }

  void printBoundGenericNoSugar(NodePointer Node) {
    if (Node->getNumChildren() < 2)
      return;
    NodePointer typelist = Node->getChild(1);
    print(Node->getChild(0));
    Printer << "<";
    printChildren(typelist, ", ");
    Printer << ">";
  }

  static bool isSwiftModule(NodePointer node) {
    return (node->getKind() == Node::Kind::Module &&
            node->getText() == STDLIB_NAME);
  }
  
  bool printContext(NodePointer Context) {
    if (!Options.QualifyEntities)
      return false;

    if (Context->getKind() == Node::Kind::Module &&
        Context->getText().startswith(LLDB_EXPRESSIONS_MODULE_NAME_PREFIX)) {
      return Options.DisplayDebuggerGeneratedModule;
    }
    return true;
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

  enum class TypePrinting {
    NoType,
    WithColon,
    FunctionStyle
  };

  /// Determine whether this is a "simple" type, from the type-simple
  /// production.
  bool isSimpleType(NodePointer Node) {
    switch (Node->getKind()) {
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
    case Node::Kind::Tuple:
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
      return true;

    case Node::Kind::ProtocolList:
      return Node->getChild(0)->getNumChildren() <= 1;

    case Node::Kind::ProtocolListWithAnyObject:
      return Node->getChild(0)->getChild(0)->getNumChildren() == 0;

    case Node::Kind::ProtocolListWithClass:
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
    case Node::Kind::KeyPathGetterThunkHelper:
    case Node::Kind::KeyPathSetterThunkHelper:
    case Node::Kind::LazyProtocolWitnessTableAccessor:
    case Node::Kind::LazyProtocolWitnessTableCacheVariable:
    case Node::Kind::LocalDeclName:
    case Node::Kind::PrivateDeclName:
    case Node::Kind::MaterializeForSet:
    case Node::Kind::MergedFunction:
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
    case Node::Kind::Shared:
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
    case Node::Kind::VTableThunk:
    case Node::Kind::Weak:
    case Node::Kind::WillSet:
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
    case Node::Kind::OutlinedRetain:
    case Node::Kind::OutlinedRelease:
      return false;
    }
    printer_unreachable("bad node kind");
  }

  SugarType findSugar(NodePointer Node) {
    if (Node->getNumChildren() == 1 &&
        Node->getKind() == Node::Kind::Type)
      return findSugar(Node->getChild(0));
    
    if (Node->getNumChildren() != 2)
      return SugarType::None;
    
    if (Node->getKind() != Node::Kind::BoundGenericEnum &&
        Node->getKind() != Node::Kind::BoundGenericStructure)
      return SugarType::None;

    auto unboundType = Node->getChild(0)->getChild(0); // drill through Type
    auto typeArgs = Node->getChild(1);
    
    if (Node->getKind() == Node::Kind::BoundGenericEnum) {
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

    assert(Node->getKind() == Node::Kind::BoundGenericStructure);

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
  
  void printBoundGeneric(NodePointer Node) {
    if (Node->getNumChildren() < 2)
      return;
    if (Node->getNumChildren() != 2) {
      printBoundGenericNoSugar(Node);
      return;
    }

    if (!Options.SynthesizeSugarOnTypes ||
        Node->getKind() == Node::Kind::BoundGenericClass)
    {
      // no sugar here
      printBoundGenericNoSugar(Node);
      return;
    }

    SugarType sugarType = findSugar(Node);
    
    switch (sugarType) {
      case SugarType::None:
        printBoundGenericNoSugar(Node);
        break;
      case SugarType::Optional:
      case SugarType::ImplicitlyUnwrappedOptional: {
        NodePointer type = Node->getChild(1)->getChild(0);
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
        NodePointer type = Node->getChild(1)->getChild(0);
        Printer << "[";
        print(type);
        Printer << "]";
        break;
      }
      case SugarType::Dictionary: {
        NodePointer keyType = Node->getChild(1)->getChild(0);
        NodePointer valueType = Node->getChild(1)->getChild(1);
        Printer << "[";
        print(keyType);
        Printer << " : ";
        print(valueType);
        Printer << "]";
        break;
      }
    }
  }

  void printFunctionType(NodePointer node) {
    assert(node->getNumChildren() == 2 || node->getNumChildren() == 3);
    unsigned startIndex = 0;
    bool throws = false;
    if (node->getNumChildren() == 3) {
      assert(node->getChild(0)->getKind() == Node::Kind::ThrowsAnnotation);
      startIndex++;
      throws = true;
    }
    if (Options.ShowFunctionArgumentTypes) {
      print(node->getChild(startIndex));
      if (throws) Printer << " throws";
      print(node->getChild(startIndex+1));
      return;
    }
    // Print simplified arguments
    NodePointer Args = node->getChild(startIndex);
    assert(Args->getKind() == Node::Kind::ArgumentTuple);
    Args = Args->getFirstChild();
    assert(Args->getKind() == Node::Kind::Type);
    Args = Args->getFirstChild();
    if (Args->getKind() != Node::Kind::Tuple) {
      // only a single not-named argument
      Printer << "(_:)";
      return;
    }
    Printer << '(';
    for (NodePointer Arg : *Args) {
      assert(Arg->getKind() == Node::Kind::TupleElement);
      unsigned NameIdx = 0;
      if (Arg->getFirstChild()->getKind() == Node::Kind::VariadicMarker)
        NameIdx = 1;
      if (Arg->getChild(NameIdx)->getKind() == Node::Kind::TupleElementName) {
        Printer << Arg->getChild(NameIdx)->getText();
      } else {
        Printer << '_';
      }
      Printer << ':';
    }
    Printer << ')';
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

  unsigned printFunctionSigSpecializationParam(NodePointer Node,
                                               unsigned Idx);

  void printSpecializationPrefix(NodePointer node, StringRef Description,
                                 StringRef ParamPrefix = StringRef());

  /// The main big print function.
  NodePointer print(NodePointer Node, bool asPrefixContext = false);

  /// Utility function to print entities.
  ///
  /// \param Entity The entity node to print
  /// \param asPrefixContext Should the entity printed as a context which as a
  ///        prefix to another entity, e.g. the Abc in Abc.def()
  /// \param TypePr How should the type of the entity be printed, if at all.
  ///        E.g. with a colon for properties or as a function type.
  /// \param hasName Does the entity has a name, e.g. a function in contrast to
  ///        an initializer.
  /// \param ExtraName An extra name added to the entity name (if any).
  /// \param ExtraIndex An extra index added to the entity name (if any),
  ///        e.g. closure #1
  /// \return If a non-null node is returned it's a context which must be
  ///         printed in postfix-form after the entity: "<entity> in <context>".
  NodePointer printEntity(NodePointer Entity, bool asPrefixContext,
                          TypePrinting TypePr, bool hasName,
                          StringRef ExtraName = "", int ExtraIndex = -1);
};
} // end anonymous namespace

static bool isExistentialType(NodePointer node) {
  return (node->getKind() == Node::Kind::ExistentialMetatype ||
          node->getKind() == Node::Kind::ProtocolList ||
          node->getKind() == Node::Kind::ProtocolListWithClass ||
          node->getKind() == Node::Kind::ProtocolListWithAnyObject);
}

/// Print the relevant parameters and return the new index.
unsigned NodePrinter::printFunctionSigSpecializationParam(NodePointer Node,
                                                          unsigned Idx) {
  NodePointer firstChild = Node->getChild(Idx);
  unsigned V = firstChild->getIndex();
  auto K = FunctionSigSpecializationParamKind(V);
  switch (K) {
  case FunctionSigSpecializationParamKind::BoxToValue:
  case FunctionSigSpecializationParamKind::BoxToStack:
    print(Node->getChild(Idx++));
    return Idx;
  case FunctionSigSpecializationParamKind::ConstantPropFunction:
  case FunctionSigSpecializationParamKind::ConstantPropGlobal: {
    Printer << "[";
    print(Node->getChild(Idx++));
    Printer << " : ";
    const auto &text = Node->getChild(Idx++)->getText();
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
    print(Node->getChild(Idx++));
    Printer << " : ";
    print(Node->getChild(Idx++));
    Printer << "]";
    return Idx;
  case FunctionSigSpecializationParamKind::ConstantPropString:
    Printer << "[";
    print(Node->getChild(Idx++));
    Printer << " : ";
    print(Node->getChild(Idx++));
    Printer << "'";
    print(Node->getChild(Idx++));
    Printer << "'";
    Printer << "]";
    return Idx;
  case FunctionSigSpecializationParamKind::ClosureProp:
    Printer << "[";
    print(Node->getChild(Idx++));
    Printer << " : ";
    print(Node->getChild(Idx++));
    Printer << ", Argument Types : [";
    for (unsigned e = Node->getNumChildren(); Idx < e;) {
      NodePointer child = Node->getChild(Idx);
      // Until we no longer have a type node, keep demangling.
      if (child->getKind() != Node::Kind::Type)
        break;
      print(child);
      ++Idx;

      // If we are not done, print the ", ".
      if (Idx < e && Node->getChild(Idx)->hasText())
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
  print(Node->getChild(Idx++));
  return Idx;
}

void NodePrinter::printSpecializationPrefix(NodePointer node,
                                            StringRef Description,
                                            StringRef ParamPrefix) {
  if (!Options.DisplayGenericSpecializations) {
    if (!SpecializationPrefixPrinted) {
      Printer << "specialized ";
      SpecializationPrefixPrinted = true;
    }
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

static bool isClassType(NodePointer Node) {
  return Node->getKind() == Node::Kind::Class;
}

static bool needSpaceBeforeType(NodePointer Type) {
  switch (Type->getKind()) {
    case Node::Kind::Type:
      return needSpaceBeforeType(Type->getFirstChild());
    case Node::Kind::FunctionType:
    case Node::Kind::UncurriedFunctionType:
    case Node::Kind::DependentGenericType:
      return false;
    default:
      return true;
  }
}

NodePointer NodePrinter::print(NodePointer Node, bool asPrefixContext) {
  switch (Node->getKind()) {
  case Node::Kind::Static:
    Printer << "static ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::CurryThunk:
    Printer << "curry thunk of ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::OutlinedCopy:
    Printer << "outlined copy of ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::OutlinedConsume:
    Printer << "outlined consume of ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::OutlinedRetain:
    Printer << "outlined retain of ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::OutlinedRelease:
    Printer << "outlined release of ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::Directness:
    Printer << toString(Directness(Node->getIndex())) << " ";
    return nullptr;
  case Node::Kind::Extension:
    assert((Node->getNumChildren() == 2 || Node->getNumChildren() == 3)
           && "Extension expects 2 or 3 children.");
    if (Options.QualifyEntities && Options.DisplayExtensionContexts) {
      Printer << "(extension in ";
      // Print the module where extension is defined.
      print(Node->getChild(0), true);
      Printer << "):";
    }
    print(Node->getChild(1));
    if (Node->getNumChildren() == 3)
      print(Node->getChild(2));
    return nullptr;
  case Node::Kind::Variable:
    return printEntity(Node, asPrefixContext, TypePrinting::WithColon,
                       /*hasName*/true);
  case Node::Kind::Function:
  case Node::Kind::Subscript:
    return printEntity(Node, asPrefixContext, TypePrinting::FunctionStyle,
                       /*hasName*/true);
  case Node::Kind::GenericTypeParamDecl:
    return printEntity(Node, asPrefixContext, TypePrinting::NoType,
                       /*hasName*/true);
  case Node::Kind::ExplicitClosure:
    return printEntity(Node, asPrefixContext,
                       Options.ShowFunctionArgumentTypes ?
                         TypePrinting::FunctionStyle : TypePrinting::NoType,
                       /*hasName*/false, "closure #",
                       (int)Node->getChild(1)->getIndex() + 1);
  case Node::Kind::ImplicitClosure:
    return printEntity(Node, asPrefixContext,
                       Options.ShowFunctionArgumentTypes ?
                         TypePrinting::FunctionStyle : TypePrinting::NoType,
                       /*hasName*/false, "implicit closure #",
                       (int)Node->getChild(1)->getIndex() + 1);
  case Node::Kind::Global:
    printChildren(Node);
    return nullptr;
  case Node::Kind::Suffix:
    if (Options.DisplayUnmangledSuffix) {
      Printer << " with unmangled suffix " << QuotedString(Node->getText());
    }
    return nullptr;
  case Node::Kind::Initializer:
    return printEntity(Node, asPrefixContext, TypePrinting::NoType,
                       /*hasName*/false, "variable initialization expression");
  case Node::Kind::DefaultArgumentInitializer:
    return printEntity(Node, asPrefixContext, TypePrinting::NoType,
                       /*hasName*/false, "default argument ",
                       (int)Node->getChild(1)->getIndex());
  case Node::Kind::DeclContext:
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::Type:
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::TypeMangling:
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::Class:
  case Node::Kind::Structure:
  case Node::Kind::Enum:
  case Node::Kind::Protocol:
  case Node::Kind::TypeAlias:
    return printEntity(Node, asPrefixContext, TypePrinting::NoType,
                       /*hasName*/true);
  case Node::Kind::LocalDeclName:
    print(Node->getChild(1));
    Printer << " #" << (Node->getChild(0)->getIndex() + 1);
    return nullptr;
  case Node::Kind::PrivateDeclName:
    if (Node->getNumChildren() > 1) {
      if (Options.ShowPrivateDiscriminators)
        Printer << '(';

      print(Node->getChild(1));

      if (Options.ShowPrivateDiscriminators)
        Printer << " in " << Node->getChild(0)->getText() << ')';
    } else {
      if (Options.ShowPrivateDiscriminators) {
        Printer << "(in " << Node->getChild(0)->getText() << ')';
      }
    }
    return nullptr;
  case Node::Kind::Module:
    if (Options.DisplayModuleNames)
      Printer << Node->getText();
    return nullptr;
  case Node::Kind::Identifier:
    Printer << Node->getText();
    return nullptr;
  case Node::Kind::Index:
    Printer << Node->getIndex();
    return nullptr;
  case Node::Kind::AutoClosureType:
    Printer << "@autoclosure ";
    printFunctionType(Node);
    return nullptr;
  case Node::Kind::ThinFunctionType:
    Printer << "@convention(thin) ";
    printFunctionType(Node);
    return nullptr;
  case Node::Kind::FunctionType:
  case Node::Kind::UncurriedFunctionType:
    printFunctionType(Node);
    return nullptr;
  case Node::Kind::ArgumentTuple: {
    bool need_parens = false;
    if (Node->getNumChildren() > 1)
      need_parens = true;
    else {
      if (!Node->hasChildren())
        need_parens = true;
      else {
        Node::Kind child0_kind = Node->getChild(0)->getKind();
        if (child0_kind == Node::Kind::Type)
          child0_kind = Node->getChild(0)->getChild(0)->getKind();

        if (child0_kind != Node::Kind::Tuple)
          need_parens = true;
      }
    }
    if (need_parens)
      Printer << "(";
    printChildren(Node);
    if (need_parens)
      Printer << ")";
    return nullptr;
  }
  case Node::Kind::Tuple: {
    Printer << "(";
    printChildren(Node, ", ");
    Printer << ")";
    return nullptr;
  }
  case Node::Kind::TupleElement: {
    unsigned Idx = 0;
    bool isVariadic = false;
    if (Node->getNumChildren() >= 1 &&
        Node->getFirstChild()->getKind() == Node::Kind::VariadicMarker) {
      isVariadic = true;
      Idx++;
    }
    NodePointer type = nullptr;
    if (Node->getNumChildren() == Idx + 1) {
      type = Node->getChild(Idx);
    } else if (Node->getNumChildren() == Idx + 2) {
      NodePointer id = Node->getChild(Idx);
      type = Node->getChild(Idx + 1);
      print(id);
    }
    if (isVariadic) {
      SugarType Sugar = findSugar(type);
      if (Sugar == SugarType::Array)
        type = type->getFirstChild()->getChild(1)->getFirstChild();
      print(type);
      Printer << "...";
    } else {
      print(type);
    }
    return nullptr;
  }
  case Node::Kind::TupleElementName:
    Printer << Node->getText() << ": ";
    return nullptr;
  case Node::Kind::ReturnType:
    if (Node->getNumChildren() == 0)
      Printer << " -> " << Node->getText();
    else {
      Printer << " -> ";
      printChildren(Node);
    }
    return nullptr;
  case Node::Kind::Weak:
    Printer << "weak ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::Unowned:
    Printer << "unowned ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::Unmanaged:
    Printer << "unowned(unsafe) ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::InOut:
    Printer << "inout ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::Shared:
    Printer << "__shared ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::NonObjCAttribute:
    Printer << "@nonobjc ";
    return nullptr;
  case Node::Kind::ObjCAttribute:
    Printer << "@objc ";
    return nullptr;
  case Node::Kind::DirectMethodReferenceAttribute:
    Printer << "super ";
    return nullptr;
  case Node::Kind::DynamicAttribute:
    Printer << "dynamic ";
    return nullptr;
  case Node::Kind::VTableAttribute:
    Printer << "override ";
    return nullptr;
  case Node::Kind::FunctionSignatureSpecialization:
    printSpecializationPrefix(Node, "function signature specialization");
    return nullptr;
  case Node::Kind::GenericPartialSpecialization:
    printSpecializationPrefix(Node, "generic partial specialization",
                              "Signature = ");
    return nullptr;
  case Node::Kind::GenericPartialSpecializationNotReAbstracted:
    printSpecializationPrefix(Node,
            "generic not-reabstracted partial specialization", "Signature = ");
    return nullptr;
  case Node::Kind::GenericSpecialization:
    printSpecializationPrefix(Node, "generic specialization");
    return nullptr;
  case Node::Kind::GenericSpecializationNotReAbstracted:
    printSpecializationPrefix(Node, "generic not re-abstracted specialization");
    return nullptr;
  case Node::Kind::SpecializationIsFragile:
    Printer << "preserving fragile attribute";
    return nullptr;
  case Node::Kind::GenericSpecializationParam:
    print(Node->getChild(0));
    for (unsigned i = 1, e = Node->getNumChildren(); i < e; ++i) {
      if (i == 1)
        Printer << " with ";
      else
        Printer << " and ";
      print(Node->getChild(i));
    }
    return nullptr;
  case Node::Kind::FunctionSignatureSpecializationParam: {
    uint64_t argNum = Node->getIndex();

    Printer << "Arg[" << argNum << "] = ";

    unsigned Idx = printFunctionSigSpecializationParam(Node, 0);

    for (unsigned e = Node->getNumChildren(); Idx < e;) {
      Printer << " and ";
      Idx = printFunctionSigSpecializationParam(Node, Idx);
    }

    return nullptr;
  }
  case Node::Kind::FunctionSignatureSpecializationParamPayload: {
    std::string demangledName = demangleSymbolAsString(Node->getText());
    if (demangledName.empty()) {
      Printer << Node->getText();
    } else {
      Printer << demangledName;
    }
    return nullptr;
  }
  case Node::Kind::FunctionSignatureSpecializationParamKind: {
    uint64_t raw = Node->getIndex();

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
      return nullptr;
    }

    if (printedOptionSet)
      return nullptr;

    switch (FunctionSigSpecializationParamKind(raw)) {
    case FunctionSigSpecializationParamKind::BoxToValue:
      Printer << "Value Promoted from Box";
      return nullptr;
    case FunctionSigSpecializationParamKind::BoxToStack:
      Printer << "Stack Promoted from Box";
      return nullptr;
    case FunctionSigSpecializationParamKind::ConstantPropFunction:
      Printer << "Constant Propagated Function";
      return nullptr;
    case FunctionSigSpecializationParamKind::ConstantPropGlobal:
      Printer << "Constant Propagated Global";
      return nullptr;
    case FunctionSigSpecializationParamKind::ConstantPropInteger:
      Printer << "Constant Propagated Integer";
      return nullptr;
    case FunctionSigSpecializationParamKind::ConstantPropFloat:
      Printer << "Constant Propagated Float";
      return nullptr;
    case FunctionSigSpecializationParamKind::ConstantPropString:
      Printer << "Constant Propagated String";
      return nullptr;
    case FunctionSigSpecializationParamKind::ClosureProp:
      Printer << "Closure Propagated";
      return nullptr;
    case FunctionSigSpecializationParamKind::Dead:
    case FunctionSigSpecializationParamKind::OwnedToGuaranteed:
    case FunctionSigSpecializationParamKind::SROA:
      printer_unreachable("option sets should have been handled earlier");
    }
    return nullptr;
  }
  case Node::Kind::SpecializationPassID:
    Printer << Node->getIndex();
    return nullptr;
  case Node::Kind::BuiltinTypeName:
    Printer << Node->getText();
    return nullptr;
  case Node::Kind::Number:
    Printer << Node->getIndex();
    return nullptr;
  case Node::Kind::InfixOperator:
    Printer << Node->getText() << " infix";
    return nullptr;
  case Node::Kind::PrefixOperator:
    Printer << Node->getText() << " prefix";
    return nullptr;
  case Node::Kind::PostfixOperator:
    Printer << Node->getText() << " postfix";
    return nullptr;
  case Node::Kind::LazyProtocolWitnessTableAccessor:
    Printer << "lazy protocol witness table accessor for type ";
    print(Node->getChild(0));
    Printer << " and conformance ";
    print(Node->getChild(1));
    return nullptr;
  case Node::Kind::LazyProtocolWitnessTableCacheVariable:
    Printer << "lazy protocol witness table cache variable for type ";
    print(Node->getChild(0));
    Printer << " and conformance ";
    print(Node->getChild(1));
    return nullptr;
  case Node::Kind::ProtocolWitnessTableAccessor:
    Printer << "protocol witness table accessor for ";
    print(Node->getFirstChild());
    return nullptr;
  case Node::Kind::ProtocolWitnessTable:
    Printer << "protocol witness table for ";
    print(Node->getFirstChild());
    return nullptr;
  case Node::Kind::GenericProtocolWitnessTable:
    Printer << "generic protocol witness table for ";
    print(Node->getFirstChild());
    return nullptr;
  case Node::Kind::GenericProtocolWitnessTableInstantiationFunction:
    Printer << "instantiation function for generic protocol witness table for ";
    print(Node->getFirstChild());
    return nullptr;
  case Node::Kind::VTableThunk: {
    Printer << "vtable thunk for ";
    print(Node->getChild(1));
    Printer << " dispatching to ";
    print(Node->getChild(0));
    return nullptr;
  }
  case Node::Kind::ProtocolWitness: {
    Printer << "protocol witness for ";
    print(Node->getChild(1));
    Printer << " in conformance ";
    print(Node->getChild(0));
    return nullptr;
  }
  case Node::Kind::PartialApplyForwarder:
    if (Options.ShortenPartialApply)
      Printer << "partial apply";
    else
      Printer << "partial apply forwarder";

    if (Node->hasChildren()) {
      Printer << " for ";
      print(Node->getFirstChild());
    }
    return nullptr;
  case Node::Kind::PartialApplyObjCForwarder:
    if (Options.ShortenPartialApply)
      Printer << "partial apply";
    else
      Printer << "partial apply ObjC forwarder";

    if (Node->hasChildren()) {
      Printer << " for ";
      print(Node->getFirstChild());
    }
    return nullptr;
  case Node::Kind::KeyPathGetterThunkHelper:
    Printer << "key path getter for ";
    print(Node->getChild(0));
    Printer << " : ";
    if (Node->getNumChildren() == 2) {
      print(Node->getChild(1));
    } else {
      print(Node->getChild(1));
      print(Node->getChild(2));
    }
    return nullptr;
  case Node::Kind::KeyPathSetterThunkHelper:
    Printer << "key path setter for ";
    print(Node->getChild(0));
    Printer << " : ";
    if (Node->getNumChildren() == 2) {
      print(Node->getChild(1));
    } else {
      print(Node->getChild(1));
      print(Node->getChild(2));
    }
    return nullptr;
  case Node::Kind::FieldOffset: {
    print(Node->getChild(0)); // directness
    Printer << "field offset for ";
    auto entity = Node->getChild(1);
    print(entity, /*asContext*/ false);
    return nullptr;
  }
  case Node::Kind::ReabstractionThunk:
  case Node::Kind::ReabstractionThunkHelper: {
    if (Options.ShortenThunk) {
      Printer << "thunk for ";
      print(Node->getChild(Node->getNumChildren() - 2));
      return nullptr;
    }
    Printer << "reabstraction thunk ";
    if (Node->getKind() == Node::Kind::ReabstractionThunkHelper)
      Printer << "helper ";
    auto generics = getFirstChildOfKind(Node, Node::Kind::DependentGenericSignature);
    assert(Node->getNumChildren() == 2 + unsigned(generics != nullptr));
    if (generics) {
      print(generics);
      Printer << " ";
    }
    Printer << "from ";
    print(Node->getChild(Node->getNumChildren() - 2));
    Printer << " to ";
    print(Node->getChild(Node->getNumChildren() - 1));
    return nullptr;
  }
  case Node::Kind::MergedFunction:
    if (!Options.ShortenThunk) {
      Printer << "merged ";
    }
    return nullptr;
  case Node::Kind::GenericTypeMetadataPattern:
    Printer << "generic type metadata pattern for ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::Metaclass:
    Printer << "metaclass for ";
    print(Node->getFirstChild());
    return nullptr;
  case Node::Kind::ProtocolDescriptor:
    Printer << "protocol descriptor for ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::FullTypeMetadata:
    Printer << "full type metadata for ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::TypeMetadata:
    Printer << "type metadata for ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::TypeMetadataAccessFunction:
    Printer << "type metadata accessor for ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::TypeMetadataLazyCache:
    Printer << "lazy cache variable for type metadata for ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::AssociatedTypeMetadataAccessor:
    Printer << "associated type metadata accessor for ";
    print(Node->getChild(1));
    Printer << " in ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::AssociatedTypeWitnessTableAccessor:
    Printer << "associated type witness table accessor for ";
    print(Node->getChild(1));
    Printer << " : ";
    print(Node->getChild(2));
    Printer << " in ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::NominalTypeDescriptor:
    Printer << "nominal type descriptor for ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::ValueWitness:
    Printer << toString(ValueWitnessKind(Node->getIndex()));
    if (Options.ShortenValueWitness) Printer << " for ";
    else Printer << " value witness for ";
    print(Node->getFirstChild());
    return nullptr;
  case Node::Kind::ValueWitnessTable:
    Printer << "value witness table for ";
    print(Node->getFirstChild());
    return nullptr;
  case Node::Kind::BoundGenericClass:
  case Node::Kind::BoundGenericStructure:
  case Node::Kind::BoundGenericEnum:
    printBoundGeneric(Node);
    return nullptr;
  case Node::Kind::DynamicSelf:
    Printer << "Self";
    return nullptr;
  case Node::Kind::CFunctionPointer: {
    Printer << "@convention(c) ";
    printFunctionType(Node);
    return nullptr;
  }
  case Node::Kind::ObjCBlock: {
    Printer << "@convention(block) ";
    printFunctionType(Node);
    return nullptr;
  }
  case Node::Kind::SILBoxType: {
    Printer << "@box ";
    NodePointer type = Node->getChild(0);
    print(type);
    return nullptr;
  }
  case Node::Kind::Metatype: {
    unsigned Idx = 0;
    if (Node->getNumChildren() == 2) {
      NodePointer repr = Node->getChild(Idx);
      print(repr);
      Printer << " ";
      Idx++;
    }
    NodePointer type = Node->getChild(Idx)->getChild(0);
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
    return nullptr;
  }
  case Node::Kind::ExistentialMetatype: {
    unsigned Idx = 0;
    if (Node->getNumChildren() == 2) {
      NodePointer repr = Node->getChild(Idx);
      print(repr);
      Printer << " ";
      Idx++;
    }

    NodePointer type = Node->getChild(Idx);
    print(type);
    Printer << ".Type";
    return nullptr;
  }
  case Node::Kind::MetatypeRepresentation: {
    Printer << Node->getText();
    return nullptr;
  }
  case Node::Kind::AssociatedTypeRef:
    print(Node->getChild(0));
    Printer << '.' << Node->getChild(1)->getText();
    return nullptr;
  case Node::Kind::ProtocolList: {
    NodePointer type_list = Node->getChild(0);
    if (!type_list)
      return nullptr;
    if (type_list->getNumChildren() == 0)
      Printer << "Any";
    else
      printChildren(type_list, " & ");
    return nullptr;
  }
  case Node::Kind::ProtocolListWithClass: {
    if (Node->getNumChildren() < 2)
      return nullptr;
    NodePointer protocols = Node->getChild(0);
    NodePointer superclass = Node->getChild(1);
    print(superclass);
    Printer << " & ";
    if (protocols->getNumChildren() < 1)
      return nullptr;
    NodePointer type_list = protocols->getChild(0);
    printChildren(type_list, " & ");
    return nullptr;
  }
  case Node::Kind::ProtocolListWithAnyObject: {
    if (Node->getNumChildren() < 1)
      return nullptr;
    NodePointer protocols = Node->getChild(0);
    if (protocols->getNumChildren() < 1)
      return nullptr;
    NodePointer type_list = protocols->getChild(0);
    if (type_list->getNumChildren() > 0) {
      printChildren(type_list, " & ");
      Printer << " & ";
    }
    if (Options.QualifyEntities)
      Printer << "Swift.";
    Printer << "AnyObject";
    return nullptr;
  }
  case Node::Kind::AssociatedType:
    // Don't print for now.
    return nullptr;
  case Node::Kind::QualifiedArchetype: {
    if (Options.ShortenArchetype) {
      Printer << "(archetype)";
      return nullptr;
    }
    if (Node->getNumChildren() < 2)
      return nullptr;
    NodePointer number = Node->getChild(0);
    NodePointer decl_ctx = Node->getChild(1);
    Printer << "(archetype " << number->getIndex() << " of ";
    print(decl_ctx);
    Printer << ")";
    return nullptr;
  }
  case Node::Kind::OwningAddressor:
    return printEntity(Node, asPrefixContext, TypePrinting::WithColon,
                       /*hasName*/true, "owningAddressor");
  case Node::Kind::OwningMutableAddressor:
    return printEntity(Node, asPrefixContext, TypePrinting::WithColon,
                       /*hasName*/true, "owningMutableAddressor");
  case Node::Kind::NativeOwningAddressor:
    return printEntity(Node, asPrefixContext, TypePrinting::WithColon,
                       /*hasName*/true, "nativeOwningAddressor");
  case Node::Kind::NativeOwningMutableAddressor:
    return printEntity(Node, asPrefixContext, TypePrinting::WithColon,
                       /*hasName*/true, "nativeOwningMutableAddressor");
  case Node::Kind::NativePinningAddressor:
    return printEntity(Node, asPrefixContext, TypePrinting::WithColon,
                       /*hasName*/true, "nativePinningAddressor");
  case Node::Kind::NativePinningMutableAddressor:
    return printEntity(Node, asPrefixContext, TypePrinting::WithColon,
                       /*hasName*/true, "nativePinningMutableAddressor");
  case Node::Kind::UnsafeAddressor:
    return printEntity(Node, asPrefixContext, TypePrinting::WithColon,
                       /*hasName*/true, "unsafeAddressor");
  case Node::Kind::UnsafeMutableAddressor:
    return printEntity(Node, asPrefixContext, TypePrinting::WithColon,
                       /*hasName*/true, "unsafeMutableAddressor");
  case Node::Kind::GlobalGetter:
    return printEntity(Node, asPrefixContext, TypePrinting::WithColon,
                       /*hasName*/true, "getter");
  case Node::Kind::Getter:
    return printEntity(Node, asPrefixContext, TypePrinting::WithColon,
                       /*hasName*/true, "getter");
  case Node::Kind::Setter:
    return printEntity(Node, asPrefixContext, TypePrinting::WithColon,
                       /*hasName*/true, "setter");
  case Node::Kind::MaterializeForSet:
    return printEntity(Node, asPrefixContext, TypePrinting::WithColon,
                       /*hasName*/true, "materializeForSet");
  case Node::Kind::WillSet:
    return printEntity(Node, asPrefixContext, TypePrinting::WithColon,
                       /*hasName*/true, "willset");
  case Node::Kind::DidSet:
    return printEntity(Node, asPrefixContext, TypePrinting::WithColon,
                       /*hasName*/true, "didset");
  case Node::Kind::Allocator:
    return printEntity(Node, asPrefixContext, TypePrinting::FunctionStyle,
                       /*hasName*/false, isClassType(Node->getChild(0)) ?
                                         "__allocating_init" : "init");
  case Node::Kind::Constructor:
    return printEntity(Node, asPrefixContext, TypePrinting::FunctionStyle,
                       /*hasName*/Node->getNumChildren() > 2, "init");
  case Node::Kind::Destructor:
    return printEntity(Node, asPrefixContext, TypePrinting::NoType,
                       /*hasName*/false, "deinit");
  case Node::Kind::Deallocator:
    return printEntity(Node, asPrefixContext, TypePrinting::NoType,
                       /*hasName*/false, isClassType(Node->getChild(0)) ?
                                         "__deallocating_deinit" : "deinit");
  case Node::Kind::IVarInitializer:
    return printEntity(Node, asPrefixContext, TypePrinting::NoType,
                       /*hasName*/false, "__ivar_initializer");
  case Node::Kind::IVarDestroyer:
    return printEntity(Node, asPrefixContext, TypePrinting::NoType,
                       /*hasName*/false, "__ivar_destroyer");
  case Node::Kind::ProtocolConformance: {
    NodePointer child0 = Node->getChild(0);
    NodePointer child1 = Node->getChild(1);
    NodePointer child2 = Node->getChild(2);
    if (Node->getNumChildren() == 4) {
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
    return nullptr;
  }
  case Node::Kind::TypeList:
    printChildren(Node);
    return nullptr;
  case Node::Kind::ImplConvention:
    Printer << Node->getText();
    return nullptr;
  case Node::Kind::ImplFunctionAttribute:
    Printer << Node->getText();
    return nullptr;
  case Node::Kind::ImplErrorResult:
    Printer << "@error ";
    LLVM_FALLTHROUGH;
  case Node::Kind::ImplParameter:
  case Node::Kind::ImplResult:
    printChildren(Node, " ");
    return nullptr;
  case Node::Kind::ImplFunctionType:
    printImplFunctionType(Node);
    return nullptr;
  case Node::Kind::ErrorType:
    Printer << "<ERROR TYPE>";
    return nullptr;
      
  case Node::Kind::DependentPseudogenericSignature:
  case Node::Kind::DependentGenericSignature: {
    Printer << '<';
    
    unsigned depth = 0;
    unsigned numChildren = Node->getNumChildren();
    for (;
         depth < numChildren
           && Node->getChild(depth)->getKind()
               == Node::Kind::DependentGenericParamCount;
         ++depth) {
      if (depth != 0)
        Printer << "><";
      
      unsigned count = Node->getChild(depth)->getIndex();
      for (unsigned index = 0; index < count; ++index) {
        if (index != 0)
          Printer << ", ";
        // FIXME: Depth won't match when a generic signature applies to a
        // method in generic type context.
        Printer << archetypeName(index, depth);
      }
    }
    
    if (depth != numChildren) {
      if (Options.DisplayWhereClauses) {
        Printer << " where ";
        for (unsigned i = depth; i < numChildren; ++i) {
          if (i > depth)
            Printer << ", ";
          print(Node->getChild(i));
        }
      }
    }
    Printer << '>';
    return nullptr;
  }
  case Node::Kind::DependentGenericParamCount:
    printer_unreachable("should be printed as a child of a "
                        "DependentGenericSignature");
  case Node::Kind::DependentGenericConformanceRequirement: {
    NodePointer type = Node->getChild(0);
    NodePointer reqt = Node->getChild(1);
    print(type);
    Printer << ": ";
    print(reqt);
    return nullptr;
  }
  case Node::Kind::DependentGenericLayoutRequirement: {
    NodePointer type = Node->getChild(0);
    NodePointer layout = Node->getChild(1);
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
      name = "AnyObject";
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
    if (Node->getNumChildren() > 2) {
      Printer << "(";
      print(Node->getChild(2));
      if (Node->getNumChildren() > 3) {
        Printer << ", ";
        print(Node->getChild(3));
      }
      Printer << ")";
    }
    return nullptr;
  }
  case Node::Kind::DependentGenericSameTypeRequirement: {
    NodePointer fst = Node->getChild(0);
    NodePointer snd = Node->getChild(1);
    
    print(fst);
    Printer << " == ";
    print(snd);
    return nullptr;
  }
  case Node::Kind::DependentGenericParamType: {
    Printer << Node->getText();
    return nullptr;
  }
  case Node::Kind::DependentGenericType: {
    NodePointer sig = Node->getChild(0);
    NodePointer depTy = Node->getChild(1);
    print(sig);
    if (needSpaceBeforeType(depTy))
      Printer << ' ';
    print(depTy);
    return nullptr;
  }
  case Node::Kind::DependentMemberType: {
    NodePointer base = Node->getChild(0);
    print(base);
    Printer << '.';
    NodePointer assocTy = Node->getChild(1);
    print(assocTy);
    return nullptr;
  }
  case Node::Kind::DependentAssociatedTypeRef: {
    Printer << Node->getText();
    return nullptr;
  }
  case Node::Kind::ReflectionMetadataBuiltinDescriptor:
    Printer << "reflection metadata builtin descriptor ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::ReflectionMetadataFieldDescriptor:
    Printer << "reflection metadata field descriptor ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::ReflectionMetadataAssocTypeDescriptor:
    Printer << "reflection metadata associated type descriptor ";
    print(Node->getChild(0));
    return nullptr;
  case Node::Kind::ReflectionMetadataSuperclassDescriptor:
    Printer << "reflection metadata superclass descriptor ";
    print(Node->getChild(0));
    return nullptr;

  case Node::Kind::ThrowsAnnotation:
    Printer<< " throws ";
    return nullptr;
  case Node::Kind::EmptyList:
    Printer << " empty-list ";
    return nullptr;
  case Node::Kind::FirstElementMarker:
    Printer << " first-element-marker ";
    return nullptr;
  case Node::Kind::VariadicMarker:
    Printer << " variadic-marker ";
    return nullptr;
  case Node::Kind::SILBoxTypeWithLayout: {
    assert(Node->getNumChildren() == 1 || Node->getNumChildren() == 3);
    NodePointer layout = Node->getChild(0);
    assert(layout->getKind() == Node::Kind::SILBoxLayout);
    NodePointer signature, genericArgs = nullptr;
    if (Node->getNumChildren() == 3) {
      signature = Node->getChild(1);
      assert(signature->getKind() == Node::Kind::DependentGenericSignature);
      genericArgs = Node->getChild(2);
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
    return nullptr;
  }
  case Node::Kind::SILBoxLayout:
    Printer << '{';
    for (unsigned i = 0; i < Node->getNumChildren(); ++i) {
      if (i > 0)
        Printer << ',';
      Printer << ' ';
      print(Node->getChild(i));
    }
    Printer << " }";
    return nullptr;
  case Node::Kind::SILBoxImmutableField:
  case Node::Kind::SILBoxMutableField:
    Printer << (Node->getKind() == Node::Kind::SILBoxImmutableField
      ? "let "
      : "var ");
    assert(Node->getNumChildren() == 1
           && Node->getChild(0)->getKind() == Node::Kind::Type);
    print(Node->getChild(0));
    return nullptr;
  }
  printer_unreachable("bad node kind!");
}

NodePointer NodePrinter::
printEntity(NodePointer Entity, bool asPrefixContext, TypePrinting TypePr,
            bool hasName, StringRef ExtraName, int ExtraIndex) {
  // Either we print the context in prefix form "<context>.<name>" or in
  // suffix form "<name> in <context>".
  bool MultiWordName = ExtraName.contains(' ');
  // Also a local name (e.g. Mystruct #1) does not look good if its context is
  // printed in prefix form.
  if (hasName &&
      Entity->getChild(1)->getKind() == Node::Kind::LocalDeclName)
    MultiWordName = true;

  if (asPrefixContext && (TypePr != TypePrinting::NoType || MultiWordName)) {
      // If the context has a type to be printed, we can't use the prefix form.
      return Entity;
  }

  NodePointer PostfixContext = nullptr;
  NodePointer Context = Entity->getChild(0);
  if (printContext(Context)) {
    if (MultiWordName) {
      // If the name contains some spaces we don't print the context now but
      // later in suffix form.
      PostfixContext = Context;
    } else {
      size_t CurrentPos = Printer.getStringRef().size();
      PostfixContext = print(Context, /*asPrefixContext*/true);

      // Was the context printed as prefix?
      if (Printer.getStringRef().size() != CurrentPos)
        Printer << '.';
    }
  }

  if (hasName) {
    assert(ExtraIndex < 0 && "Can't have a name and extra index");
    if (!ExtraName.empty() && MultiWordName) {
      Printer << ExtraName;
      Printer << " of ";
      ExtraName = "";
    }
    size_t CurrentPos = Printer.getStringRef().size();
    print(Entity->getChild(1));
    if (Printer.getStringRef().size() != CurrentPos && !ExtraName.empty())
      Printer << '.';
  }
  if (!ExtraName.empty()) {
    Printer << ExtraName;
    if (ExtraIndex >= 0)
      Printer << ExtraIndex;
  }
  if (TypePr != TypePrinting::NoType) {
    NodePointer type = Entity->getChild(1);
    if (type->getKind() != Node::Kind::Type)
      type = Entity->getChild(2);
    assert(type->getKind() == Node::Kind::Type);
    type = type->getChild(0);
    if (TypePr == TypePrinting::FunctionStyle) {
      // We expect to see a function type here, but if we don't, use the colon.
      NodePointer t = type;
      while (t->getKind() == Node::Kind::DependentGenericType)
        t = t->getChild(1)->getChild(0);
      if (t->getKind() != Node::Kind::FunctionType &&
          t->getKind() != Node::Kind::UncurriedFunctionType &&
          t->getKind() != Node::Kind::CFunctionPointer &&
          t->getKind() != Node::Kind::ThinFunctionType) {
        TypePr = TypePrinting::WithColon;
      }
    }
    if (TypePr == TypePrinting::WithColon) {
      if (Options.DisplayEntityTypes) {
        Printer << " : ";
        print(type);
      }
    } else {
      assert(TypePr == TypePrinting::FunctionStyle);
      if (MultiWordName || needSpaceBeforeType(type))
        Printer << ' ';

      print(type);
    }
  }
  if (!asPrefixContext && PostfixContext) {
    // Print any left over context which couldn't be printed in prefix form.
    if (Entity->getKind() == Node::Kind::DefaultArgumentInitializer ||
        Entity->getKind() == Node::Kind::Initializer) {
      Printer << " of ";
    } else {
      Printer << " in ";
    }
    print(PostfixContext);
    PostfixContext = nullptr;
  }
  return PostfixContext;
};

std::string Demangle::nodeToString(NodePointer root,
                                   const DemangleOptions &options) {
  if (!root)
    return "";

  return NodePrinter(options).printRoot(root);
}
