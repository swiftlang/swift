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

#include <memory>
#include <string>
#include <vector>
#include <cassert>
#include <iosfwd>

namespace llvm {
  class raw_ostream;
}

namespace swift {
namespace Demangle {

struct DemangleOptions {
  bool SynthesizeSugarOnTypes = false;
  bool DisplayTypeOfIVarFieldOffset = true;
  bool DisplayDebuggerGeneratedModule = true;
  bool Simplified = false;

  DemangleOptions() {}
};

class Node;
typedef std::shared_ptr<Node> NodePointer;

enum class FunctionSigSpecializationParamKind : unsigned {
  // Option Flags use bits 0-5. This give us 6 bits implying 64 entries to
  // work with.
  Dead = 0,
  ConstantPropFunction = 1,
  ConstantPropGlobal = 2,
  ConstantPropInteger = 3,
  ConstantPropFloat = 4,
  ConstantPropString = 5,
  ClosureProp = 6,
  InOutToValue = 7,

  // Option Set Flags use bits 6-31. This gives us 26 bits to use for option
  // flags.
  OwnedToGuaranteed = 1 << 6,
  SROA = 1 << 7,
};

/// The pass that caused the specialization to occur. We use this to make sure
/// that two passes that generate similar changes do not yield the same
/// mangling. This currently can not happen, so this is just a safety measure
/// that creates separate name spaces.
enum class SpecializationPass : uint8_t {
  AllocBoxToStack,
  ClosureSpecializer,
  CapturePromotion,
  CapturePropagation,
  FunctionSignatureOpts,
  GenericSpecializer,
};

static inline char encodeSpecializationPass(SpecializationPass Pass) {
  return char(uint8_t(Pass)) + 48;
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
  InitializeBufferWithTakeOfBuffer,
  DestroyArray,
  InitializeArrayWithCopy,
  InitializeArrayWithTakeFrontToBack,
  InitializeArrayWithTakeBackToFront,
  StoreExtraInhabitant,
  GetExtraInhabitantIndex,
  GetEnumTag,
  InplaceProjectEnumData
};

enum class Directness {
  Direct, Indirect
};

class Node : public std::enable_shared_from_this<Node> {
public:
  enum class Kind : uint16_t {
#define NODE(ID) ID,
#include "swift/Basic/DemangleNodes.def"
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

  // FIXME: use allocator.
  typedef std::vector<NodePointer> NodeVector;
  NodeVector Children;

  Node(Kind k)
      : NodeKind(k), NodePayloadKind(PayloadKind::None) {
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

  friend struct NodeFactory;

public:
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

  NodePointer getFirstChild() const { return Children.front(); }
  NodePointer getChild(size_t index) const { return Children[index]; }

  /// Add a new node as a child of this one.
  ///
  /// \param child - should have no parent or siblings
  /// \returns child
  NodePointer addChild(NodePointer child) {
    assert(child && "adding null child!");
    Children.push_back(child);
    return child;
  }

  /// A convenience method for adding two children at once.
  void addChildren(NodePointer child1, NodePointer child2) {
    addChild(std::move(child1));
    addChild(std::move(child2));
  }
};

/// \brief Demangle the given string as a Swift symbol.
///
/// Typical usage:
/// \code
///   NodePointer aDemangledName =
/// swift::Demangler::demangleSymbolAsNode("SomeSwiftMangledName")
/// \endcode
///
/// \param mangledName The mangled string.
/// \param options An object encapsulating options to use to perform this demangling.
///
///
/// \returns A parse tree for the demangled string - or a null pointer
/// on failure.
///
NodePointer
demangleSymbolAsNode(const char *mangledName, size_t mangledNameLength,
                     const DemangleOptions &options = DemangleOptions());

inline NodePointer
demangleSymbolAsNode(const std::string &mangledName,
                     const DemangleOptions &options = DemangleOptions()) {
  return demangleSymbolAsNode(mangledName.data(), mangledName.size(), options);
}

/// \brief Demangle the given string as a Swift symbol.
///
/// Typical usage:
/// \code
///   std::string aDemangledName =
/// swift::Demangler::demangleSymbol("SomeSwiftMangledName")
/// \endcode
///
/// \param mangledName The mangled string.
/// \param options An object encapsulating options to use to perform this demangling.
///
///
/// \returns A string representing the demangled name.
///
std::string
demangleSymbolAsString(const char *mangledName, size_t mangledNameLength,
                       const DemangleOptions &options = DemangleOptions());

inline std::string
demangleSymbolAsString(const std::string &mangledName,
                       const DemangleOptions &options = DemangleOptions()) {
  return demangleSymbolAsString(mangledName.data(), mangledName.size(),
                                options);
}

/// \brief Demangle the given string as a Swift type.
///
/// Typical usage:
/// \code
///   NodePointer aDemangledName =
/// swift::Demangler::demangleTypeAsNode("SomeSwiftMangledName")
/// \endcode
///
/// \param mangledName The mangled string.
/// \param options An object encapsulating options to use to perform this demangling.
///
///
/// \returns A parse tree for the demangled string - or a null pointer
/// on failure.
///
NodePointer
demangleTypeAsNode(const char *mangledName, size_t mangledNameLength,
                   const DemangleOptions &options = DemangleOptions());

inline NodePointer
demangleTypeAsNode(const std::string &mangledName,
                   const DemangleOptions &options = DemangleOptions()) {
  return demangleTypeAsNode(mangledName.data(), mangledName.size(), options);
}

/// \brief Demangle the given string as a Swift type mangling.
///
/// \param mangledName The mangled string.
/// \param options An object encapsulating options to use to perform this demangling.
///
///
/// \returns A string representing the demangled name.
std::string
demangleTypeAsString(const char *mangledName, size_t mangledNameLength,
                     const DemangleOptions &options = DemangleOptions());

inline std::string
demangleTypeAsString(const std::string &mangledName,
                     const DemangleOptions &options = DemangleOptions()) {
  return demangleTypeAsString(mangledName.data(), mangledName.size(), options);
}

enum class OperatorKind {
  NotOperator,
  Prefix,
  Postfix,
  Infix
};

/// \brief Mangle an identifier using Swift's mangling rules.
void mangleIdentifier(const char *data, size_t length,
                      OperatorKind operatorKind, std::ostream &out,
                      bool usePunycode = true);

/// \brief Remangle a demangled parse tree.
///
/// This should always round-trip perfectly with demangleSymbolAsNode.
std::string mangleNode(const NodePointer &root);

/// \brief Write a mangling of the given parse tree into the given stream.
///
/// This should always round-trip perfectly with demangleSymbolAsNode.
void mangleNode(NodePointer root, std::ostream &out);

/// \brief Transform the node structure in a string.
///
/// Typical usage:
/// \code
///   std::string aDemangledName =
/// swift::Demangler::nodeToString(aNode)
/// \endcode
///
/// \param Root A pointer to a parse tree generated by the demangler.
/// \param Options An object encapsulating options to use to perform this demangling.
///
/// \returns A string representing the demangled name.
///
std::string nodeToString(NodePointer Root,
                         const DemangleOptions &Options = DemangleOptions());

} // end namespace Demangle
} // end namespace swift

#endif

