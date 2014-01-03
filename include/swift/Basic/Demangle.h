//===--- Demangle.h - Interface to Swift symbol demangling -------*- C++ -*-==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_DEMANGLE_H
#define SWIFT_BASIC_DEMANGLE_H

#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "swift/Basic/LLVM.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {
namespace Demangle {

struct DemangleOptions
{
  bool SynthesizeSugarOnTypes = false;
  bool DisplayTypeOfIVarFieldOffset = true;

  DemangleOptions () : SynthesizeSugarOnTypes(false), DisplayTypeOfIVarFieldOffset(true) {}
};

class Node;
typedef llvm::IntrusiveRefCntPtr<Node> NodePointer;

class Node : public llvm::RefCountedBase<Node> {
public:
  enum class Kind : uint16_t {
    Failure = 0,
    Addressor,
    Allocator,
    ArchetypeAndProtocol,
    ArchetypeRef,
    ArgumentTuple,
    ArrayType,
    AssociatedTypeRef,
    BoundGenericClass,
    BoundGenericEnum,
    BoundGenericStructure,
    BridgeToBlockFunction,
    BuiltinTypeName,
    Class,
    Constructor,
    Deallocator,
    DeclContext,
    DefaultArgumentInitializer,
    DependentProtocolWitnessTableGenerator,
    DependentProtocolWitnessTableTemplate,
    Destructor,
    Directness,
    Enum,
    ErrorType,
    FieldOffset,
    Function,
    FunctionType,
    Generics,
    GenericType,
    GenericTypeMetadataPattern,
    Getter,
    Global,
    Identifier,
    ImplConvention,
    ImplFunctionAttribute,
    ImplFunctionType,
    ImplParameter,
    ImplResult,
    InOut,
    InfixOperator,
    Initializer,
    LazyProtocolWitnessTableAccessor,
    LazyProtocolWitnessTableTemplate,
    LocalDeclName,
    LocalEntity,
    Metatype,
    Metaclass,
    Module,
    NominalTypeDescriptor,
    NonVariadicTuple,
    Number,
    ObjCAttribute,
    ObjCBlock,
    PostfixOperator,
    PrefixOperator,
    Protocol,
    ProtocolConformance,
    ProtocolList,
    ProtocolWitness,
    ProtocolWitnessTable,
    QualifiedArchetype,
    ReabstractionThunk,
    ReabstractionThunkHelper,
    ReturnType,
    SelfTypeRef,
    Setter,
    Structure,
    Suffix,
    TupleElement,
    TupleElementName,
    TupleElementType,
    Type,
    TypeAlias,
    TypeList,
    TypeMetadata,
    UncurriedFunctionType,
    Unknown,
    Unowned,
    ValueWitnessKind,
    ValueWitnessTable,
    Variable,
    VariadicTuple,
    Weak,
    WitnessTableOffset
  };

  typedef uint64_t IndexType;

private:
  Kind NodeKind;

  enum class PayloadKind : uint8_t {
    None, Text, Index
  };
  PayloadKind NodePayloadKind;

  union {
    std::string TextPayload;
    IndexType IndexPayload;
  };

  // It might even be worthwhile to use TinyPtrVector here.
  typedef llvm::SmallVector<NodePointer, 2> NodeVector;
  NodeVector Children;

  Node(Kind k)
      : NodeKind(k), NodePayloadKind(PayloadKind::None) {
  }
  Node(Kind k, StringRef t)
      : NodeKind(k), NodePayloadKind(PayloadKind::Text) {
    new (&TextPayload) std::string(t);
  }
  Node(Kind k, std::string &&t)
      : NodeKind(k), NodePayloadKind(PayloadKind::Text) {
    new (&TextPayload) std::string(std::move(t));
  }
  Node(Kind k, IndexType index)
      : NodeKind(k), NodePayloadKind(PayloadKind::Index) {
    IndexPayload = index;
  }
  Node(const Node &) = delete;
  Node &operator=(const Node &) = delete;
public:  
  static NodePointer create(Kind k) {
    return NodePointer(new Node(k));
  }
  static NodePointer create(Kind k, IndexType index) {
    return NodePointer(new Node(k, index));
  }
  static NodePointer create(Kind k, llvm::StringRef text) {
    return NodePointer(new Node(k, text));
  }
  static NodePointer create(Kind k, std::string &&text) {
    return NodePointer(new Node(k, std::move(text)));
  }
  template <size_t N>
  static NodePointer create(Kind k, const char (&text)[N]) {
    return NodePointer(new Node(k, llvm::StringRef(text)));
  }

  ~Node();

  Kind getKind() const { return NodeKind; }

  bool hasText() const { return NodePayloadKind == PayloadKind::Text; }
  const std::string &getText() const {
    assert(hasText());
    return TextPayload;
  }

  bool hasIndex() const { return NodePayloadKind == PayloadKind::Index; }
  uint64_t getIndex() const {
    assert(hasIndex());
    return IndexPayload;
  }
  
  typedef NodeVector::iterator iterator;
  typedef NodeVector::const_iterator const_iterator;
  typedef NodeVector::size_type size_type;

  bool hasChildren() const { return !Children.empty(); }
  size_t getNumChildren() const { return Children.size(); }
  iterator begin() { return Children.begin(); }
  iterator end() { return Children.end(); }
  const_iterator begin() const { return Children.begin(); }
  const_iterator end() const { return Children.end(); }

  Node *getFirstChild() const { return Children.front().getPtr(); }
  Node *getChild(size_t index) const { return Children[index].getPtr(); }

  /// Add a new node as a child of this one.
  ///
  /// \param child - should have no parent or siblings
  /// \returns child
  Node *addChild(NodePointer child) {
    assert(child && "adding null child!");
    auto childRaw = child.getPtr();
    Children.push_back(std::move(child));
    return childRaw;
  }

  /// A convenience method for adding two children at once.
  void addChildren(NodePointer child1, NodePointer child2) {
    addChild(std::move(child1));
    addChild(std::move(child2));
  }

  void dump() const;
  void print(llvm::raw_ostream &out) const;
};
  
/// \brief Demangle the given string as a Swift symbol.
///
/// Typical usage:
/// \code
///   NodePointer aDemangledName =
/// swift::Demangler::demangleSymbol("SomeSwiftMangledName")
/// \endcode
///
/// \param mangled The mangled string.
/// \param options An object encapsulating options to use to perform this demangling.
///
///
/// \returns A parse tree for the demangled string - or a Failure node on
/// failure.
///
NodePointer demangleSymbolAsNode(llvm::StringRef mangled, const DemangleOptions& options = DemangleOptions());

/// \brief Transform the node structure in a string.
///
/// Typical usage:
/// \code
///   std::string aDemangledName =
/// swift::Demangler::nodeToString(aNode)
/// \endcode
///
/// \param pointer A pointer to a parse tree generated by the demangler.
/// \param options An object encapsulating options to use to perform this demangling.
///
/// \returns A string representing the demangled name.
///
std::string nodeToString(NodePointer pointer, const DemangleOptions& options = DemangleOptions());

/// \brief Demangle the given string as a Swift symbol.
///
/// Typical usage:
/// \code
///   std::string aDemangledName =
/// swift::Demangler::demangleSymbol("SomeSwiftMangledName")
/// \endcode
///
/// \param mangled The mangled string.
/// \param options An object encapsulating options to use to perform this demangling.
///
///
/// \returns A string representing the demangled name.
///
std::string demangleSymbolAsString(llvm::StringRef mangled, const DemangleOptions& options = DemangleOptions());

} // end namespace Demangle
} // end namespace swift

#endif
