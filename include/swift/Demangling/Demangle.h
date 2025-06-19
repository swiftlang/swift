//===--- Demangle.h - Interface to Swift symbol demangling ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file is the public API of the demangler library.
// Tools which use the demangler library (like lldb) must include this - and
// only this - header file.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DEMANGLING_DEMANGLE_H
#define SWIFT_DEMANGLING_DEMANGLE_H

#include "swift/Demangling/Errors.h"
#include "swift/Demangling/ManglingFlavor.h"
#include "swift/Demangling/NamespaceMacros.h"
#include "swift/Strings.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Compiler.h"

#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <functional>
#include <memory>
#include <string>

namespace swift {
namespace Demangle {
SWIFT_BEGIN_INLINE_NAMESPACE

enum class SymbolicReferenceKind : uint8_t;

/// A simple default implementation that assigns letters to type parameters in
/// alphabetic order.
std::string genericParameterName(uint64_t depth, uint64_t index);

/// Display style options for the demangler.
struct DemangleOptions {
  bool SynthesizeSugarOnTypes = false;
  bool QualifyEntities = true;
  bool DisplayExtensionContexts = true;
  bool DisplayUnmangledSuffix = true;
  bool DisplayModuleNames = true;
  bool DisplayGenericSpecializations = true;
  bool DisplayProtocolConformances = true;
  bool DisplayWhereClauses = true;
  bool DisplayEntityTypes = true;
  bool DisplayLocalNameContexts = true;
  bool ShortenPartialApply = false;
  bool ShortenThunk = false;
  bool ShortenValueWitness = false;
  bool ShortenArchetype = false;
  bool ShowPrivateDiscriminators = true;
  bool ShowFunctionArgumentTypes = true;
  bool DisplayDebuggerGeneratedModule = true;
  bool DisplayStdlibModule = true;
  bool DisplayObjCModule = true;
  bool PrintForTypeName = false;
  bool ShowAsyncResumePartial = true;
  bool ShowClosureSignature = true;

  /// If this is nonempty, entities in this module name will not be qualified.
  llvm::StringRef HidingCurrentModule;
  /// A function to render generic parameter names.
  std::function<std::string(uint64_t, uint64_t)> GenericParameterName =
      genericParameterName;

  DemangleOptions() {}

  static DemangleOptions SimplifiedUIDemangleOptions() {
    auto Opt = DemangleOptions();
    Opt.SynthesizeSugarOnTypes = true;
    Opt.QualifyEntities = true;
    Opt.DisplayExtensionContexts = false;
    Opt.DisplayUnmangledSuffix = false;
    Opt.DisplayModuleNames = false;
    Opt.DisplayGenericSpecializations = false;
    Opt.DisplayProtocolConformances = false;
    Opt.DisplayWhereClauses = false;
    Opt.DisplayEntityTypes = false;
    Opt.ShortenPartialApply = true;
    Opt.ShortenThunk = true;
    Opt.ShortenValueWitness = true;
    Opt.ShortenArchetype = true;
    Opt.ShowPrivateDiscriminators = false;
    Opt.ShowFunctionArgumentTypes = false;
    Opt.ShowAsyncResumePartial = false;
    return Opt;
  };
};

class Node;
using NodePointer = Node *;

enum class FunctionSigSpecializationParamKind : unsigned {
  // Option Flags use bits 0-5. This give us 6 bits implying 64 entries to
  // work with.
  ConstantPropFunction = 0,
  ConstantPropGlobal = 1,
  ConstantPropInteger = 2,
  ConstantPropFloat = 3,
  ConstantPropString = 4,
  ClosureProp = 5,
  BoxToValue = 6,
  BoxToStack = 7,
  InOutToOut = 8,
  ConstantPropKeyPath = 9,

  // Option Set Flags use bits 6-31. This gives us 26 bits to use for option
  // flags.
  Dead = 1 << 6,
  OwnedToGuaranteed = 1 << 7,
  SROA = 1 << 8,
  GuaranteedToOwned = 1 << 9,
  ExistentialToGeneric = 1 << 10,
};

enum class AutoDiffFunctionKind : char {
  JVP = 'f',
  VJP = 'r',
  Differential = 'd',
  Pullback = 'p',
};

enum class MangledDifferentiabilityKind : char {
  NonDifferentiable = 0,
  Forward = 'f',
  Reverse = 'r',
  Normal = 'd',
  Linear = 'l',
};

enum class MangledSILThunkKind : char {
  Invalid = 0,
  Identity = 'I',
};

/// The pass that caused the specialization to occur. We use this to make sure
/// that two passes that generate similar changes do not yield the same
/// mangling. This currently cannot happen, so this is just a safety measure
/// that creates separate name spaces.
///
/// The number of entries is limited! See `Demangler::demangleSpecAttributes`.
/// If you exceed the max, you'll need to upgrade the mangling.
enum class SpecializationPass : uint8_t {
  AllocBoxToStack = 0,
  ClosureSpecializer,
  CapturePromotion,
  CapturePropagation,
  FunctionSignatureOpts,
  GenericSpecializer,
  MoveDiagnosticInOutToOut,
  AsyncDemotion,
  LAST = AsyncDemotion
};

constexpr uint8_t MAX_SPECIALIZATION_PASS = 10;
static_assert((uint8_t)SpecializationPass::LAST < MAX_SPECIALIZATION_PASS);

static inline char encodeSpecializationPass(SpecializationPass Pass) {
  return char(uint8_t(Pass)) + '0';
}

enum class ValueWitnessKind {
#define VALUE_WITNESS(MANGLING, NAME) \
  NAME,
#include "swift/Demangling/ValueWitnessMangling.def"
};

enum class Directness {
  Direct, Indirect
};

class NodeFactory;
class Context;

class Node {
public:
  enum class Kind : uint16_t {
#define NODE(ID) ID,
#include "swift/Demangling/DemangleNodes.def"
  };

  using IndexType = uint64_t;

  friend class NodeFactory;
  
private:

  struct NodeVector {
    NodePointer *Nodes;
    uint32_t Number = 0;
    uint32_t Capacity = 0;
  };

  union {
    llvm::StringRef Text;
    IndexType Index;
    NodePointer InlineChildren[2];
    NodeVector Children;
  };


  Kind NodeKind;

  enum class PayloadKind : uint8_t {
    None = 0, OneChild = 1, TwoChildren = 2,
    Text, Index, ManyChildren
  };
  PayloadKind NodePayloadKind;

  Node(Kind k)
      : NodeKind(k), NodePayloadKind(PayloadKind::None) {
  }
  Node(Kind k, llvm::StringRef t)
      : NodeKind(k), NodePayloadKind(PayloadKind::Text) {
    Text = t;
  }
  Node(Kind k, IndexType index)
      : NodeKind(k), NodePayloadKind(PayloadKind::Index) {
    Index = index;
  }
  Node(const Node &) = delete;
  Node &operator=(const Node &) = delete;

public:
  Kind getKind() const { return NodeKind; }

  bool isSimilarTo(const Node *other) const {
    if (NodeKind != other->NodeKind
        || NodePayloadKind != other->NodePayloadKind)
      return false;
    switch (NodePayloadKind) {
    case PayloadKind::ManyChildren:
      return Children.Number == other->Children.Number;
    case PayloadKind::Index:
      return Index == other->Index;
    case PayloadKind::Text:
      return Text == other->Text;
    default:
      return true;
    }
  }

  static bool deepEquals(const Node *lhs, const Node *rhs) {
    if (lhs == rhs)
      return true;
    if ((!lhs && rhs) || (lhs && !rhs))
      return false;
    if (!lhs->isSimilarTo(rhs))
      return false;
    for (auto li = lhs->begin(), ri = rhs->begin(), le = lhs->end(); li != le;
         ++li, ++ri) {
      if (!deepEquals(*li, *ri))
        return false;
    }
    return true;
  }

  bool isDeepEqualTo(const Node *other) const {
    return deepEquals(this, other);
  }

  bool hasText() const { return NodePayloadKind == PayloadKind::Text; }
  llvm::StringRef getText() const {
    assert(hasText());
    return Text;
  }

  bool hasIndex() const { return NodePayloadKind == PayloadKind::Index; }
  uint64_t getIndex() const {
    assert(hasIndex());
    return Index;
  }

  using iterator = const NodePointer *;

  size_t getNumChildren() const {
    switch (NodePayloadKind) {
    case PayloadKind::OneChild: return 1;
    case PayloadKind::TwoChildren: return 2;
    case PayloadKind::ManyChildren: return Children.Number;
    default: return 0;
    }
  }

  bool hasChildren() const { return getNumChildren() != 0; }

  iterator begin() const {
    switch (NodePayloadKind) {
    case PayloadKind::OneChild:
    case PayloadKind::TwoChildren:
      return &InlineChildren[0];
    case PayloadKind::ManyChildren:
      return Children.Nodes;
    default:
      return nullptr;
    }
  }

  iterator end() const {
    switch (NodePayloadKind) {
    case PayloadKind::OneChild:
      return &InlineChildren[1];
    case PayloadKind::TwoChildren:
      return &InlineChildren[2];
    case PayloadKind::ManyChildren:
      return Children.Nodes + Children.Number;
    default:
      return nullptr;
    }
  }

  NodePointer getFirstChild() const {
    return getChild(0);
  }
  NodePointer getLastChild() const {
    return getChild(getNumChildren() - 1);
  }
  NodePointer getChild(size_t index) const {
    if (index >= getNumChildren())
      return nullptr;
    return begin()[index];
  }

  // Only to be used by the demangler parsers.
  void addChild(NodePointer Child, NodeFactory &Factory);
  // Only to be used by the demangler parsers.
  void removeChildAt(unsigned Pos);

  void replaceChild(unsigned Pos, NodePointer Child);

  // Reverses the order of children.
  void reverseChildren(size_t StartingAt = 0);

  // Find a node by its kind, traversing the node depth-first,
  // and bailing out early if not found at the 'maxDepth'.
  NodePointer findByKind(Node::Kind kind, int maxDepth);

  /// Prints the whole node tree in readable form to stderr.
  ///
  /// Useful to be called from the debugger.
  void dump() LLVM_ATTRIBUTE_USED;
};

/// Returns the length of the swift mangling prefix of the \p SymbolName.
///
/// Returns 0 if \p SymbolName is not a mangled swift (>= swift 4.x) name.
int getManglingPrefixLength(llvm::StringRef mangledName);

/// Returns true if \p SymbolName is a mangled swift name.
///
/// This does not include the old (<= swift 3.x) mangling prefix "_T".
inline bool isMangledName(llvm::StringRef mangledName) {
  return getManglingPrefixLength(mangledName) != 0;
}

/// Returns true if the mangledName starts with the swift mangling prefix.
///
/// This includes the old (<= swift 3.x) mangling prefix "_T".
bool isSwiftSymbol(llvm::StringRef mangledName);

/// Returns true if the mangledName starts with the swift mangling prefix.
///
/// This includes the old (<= swift 3.x) mangling prefix "_T".
bool isSwiftSymbol(const char *mangledName);

/// Drops the Swift mangling prefix from the given mangled name, if there is
/// one.
///
/// This does not include the old (<= swift 3.x) mangling prefix "_T".
llvm::StringRef dropSwiftManglingPrefix(llvm::StringRef mangledName);

/// Returns true if the mangled name is an alias type name.
///
/// \param mangledName A null-terminated string containing a mangled name.
bool isAlias(llvm::StringRef mangledName);

/// Returns true if the mangled name is a class type name.
///
/// \param mangledName A null-terminated string containing a mangled name.
bool isClass(llvm::StringRef mangledName);

/// Returns true if the mangled name is an enum type name.
///
/// \param mangledName A null-terminated string containing a mangled name.
bool isEnum(llvm::StringRef mangledName);

/// Returns true if the mangled name is a protocol type name.
///
/// \param mangledName A null-terminated string containing a mangled name.
bool isProtocol(llvm::StringRef mangledName);

/// Returns true if the mangled name is a structure type name.
///
/// \param mangledName A null-terminated string containing a mangled name.
bool isStruct(llvm::StringRef mangledName);

/// Returns true if the mangled name is an Objective-C symbol.
///
/// \param mangledName A null-terminated string containing a mangled name.
bool isObjCSymbol(llvm::StringRef mangledName);

/// Returns true if the mangled name has the old scheme of function type
/// mangling where labels are part of the type.
///
/// \param mangledName A null-terminated string containing a mangled name.
bool isOldFunctionTypeMangling(llvm::StringRef mangledName);

class Demangler;

/// The demangler context.
///
/// It owns the allocated nodes which are created during demangling.
/// It is always preferable to use the demangling via this context class as it
/// ensures efficient memory management. Especially if demangling is done for
/// multiple symbols. Typical usage:
/// \code
///   Context Ctx;
///   for (...) {
///      NodePointer Root = Ctx.demangleSymbolAsNode(MangledName);
///      // Do something with Root
///      Ctx.clear(); // deallocates Root
///   }
/// \endcode
/// Declaring the context out of the loop minimizes the amount of needed memory
/// allocations.
///
class Context {
  Demangler *D;

  friend class Node;

public:
  Context();

  ~Context();

  /// Demangle the given symbol and return the parse tree.
  ///
  /// \param MangledName The mangled symbol string, which start a mangling
  /// prefix: _T, _T0, $S, _$S.
  ///
  /// \returns A parse tree for the demangled string - or a null pointer
  /// on failure.
  /// The lifetime of the returned node tree ends with the lifetime of the
  /// context or with a call of clear().
  NodePointer demangleSymbolAsNode(llvm::StringRef MangledName);

  /// Demangle the given type and return the parse tree.
  ///
  /// \param MangledName The mangled symbol string, which start a mangling
  /// prefix: _T, _T0, $S, _$S.
  ///
  /// \returns A parse tree for the demangled string - or a null pointer
  /// on failure.
  /// The lifetime of the returned node tree ends with the lifetime of the
  /// context or with a call of clear().
  NodePointer demangleTypeAsNode(llvm::StringRef MangledName);
  
  /// Demangle the given symbol and return the readable name.
  ///
  /// \param MangledName The mangled symbol string, which start a mangling
  /// prefix: _T, _T0, $S, _$S.
  ///
  /// \returns The demangled string.
  std::string demangleSymbolAsString(
      llvm::StringRef MangledName,
      const DemangleOptions &Options = DemangleOptions());

  /// Demangle the given type and return the readable name.
  ///
  /// \param MangledName The mangled type string, which does _not_ start with
  /// a mangling prefix.
  ///
  /// \returns The demangled string.
  std::string
  demangleTypeAsString(llvm::StringRef MangledName,
                       const DemangleOptions &Options = DemangleOptions());

  /// Returns true if the mangledName refers to a thunk function.
  ///
  /// Thunk functions are either (ObjC) partial apply forwarder, swift-as-ObjC
  /// or ObjC-as-swift thunks or allocating init functions.
  bool isThunkSymbol(llvm::StringRef MangledName);

  /// Returns the mangled name of the target of a thunk.
  ///
  /// \returns Returns the remaining name after removing the thunk mangling
  /// characters from \p MangledName. If \p MangledName is not a thunk symbol
  /// or the thunk target cannot be derived from the mangling, an empty string
  /// is returned.
  std::string getThunkTarget(llvm::StringRef MangledName);

  /// Returns true if the \p mangledName refers to a function which conforms to
  /// the Swift calling convention.
  ///
  /// The return value is unspecified if the \p MangledName does not refer to a
  /// function symbol.
  bool hasSwiftCallingConvention(llvm::StringRef MangledName);

  /// Demangle the given symbol and return the module name of the symbol.
  ///
  /// \param mangledName The mangled symbol string, which start a mangling
  /// prefix: _T, _T0, $S, _$S.
  ///
  /// \returns The module name.
  std::string getModuleName(llvm::StringRef mangledName);

  /// Deallocates all nodes.
  ///
  /// The memory which is used for nodes is not freed but recycled for the next
  /// demangling operation.
  void clear();
};

/// Standalone utility function to demangle the given symbol as string.
///
/// If performance is an issue when demangling multiple symbols,
/// Context::demangleSymbolAsString should be used instead.
/// \param mangledName The mangled name string pointer.
/// \param mangledNameLength The length of the mangledName string.
/// \returns The demangled string.
std::string
demangleSymbolAsString(const char *mangledName, size_t mangledNameLength,
                       const DemangleOptions &options = DemangleOptions());

/// Standalone utility function to demangle the given symbol as string.
///
/// If performance is an issue when demangling multiple symbols,
/// Context::demangleSymbolAsString should be used instead.
/// \param mangledName The mangled name string.
/// \returns The demangled string.
inline std::string
demangleSymbolAsString(const std::string &mangledName,
                       const DemangleOptions &options = DemangleOptions()) {
  return demangleSymbolAsString(mangledName.data(), mangledName.size(),
                                options);
}
  
/// Standalone utility function to demangle the given symbol as string.
///
/// If performance is an issue when demangling multiple symbols,
/// Context::demangleSymbolAsString should be used instead.
/// \param MangledName The mangled name string.
/// \returns The demangled string.
inline std::string
demangleSymbolAsString(llvm::StringRef MangledName,
                       const DemangleOptions &Options = DemangleOptions()) {
  return demangleSymbolAsString(MangledName.data(),
                                MangledName.size(), Options);
}

/// Standalone utility function to demangle the given type as string.
///
/// If performance is an issue when demangling multiple symbols,
/// Context::demangleTypeAsString should be used instead.
/// \param mangledName The mangled name string pointer.
/// \param mangledNameLength The length of the mangledName string.
/// \returns The demangled string.
std::string
demangleTypeAsString(const char *mangledName, size_t mangledNameLength,
                     const DemangleOptions &options = DemangleOptions());

/// Standalone utility function to demangle the given type as string.
///
/// If performance is an issue when demangling multiple symbols,
/// Context::demangleTypeAsString should be used instead.
/// \param mangledName The mangled name string.
/// \returns The demangled string.
inline std::string
demangleTypeAsString(const std::string &mangledName,
                     const DemangleOptions &options = DemangleOptions()) {
  return demangleTypeAsString(mangledName.data(), mangledName.size(), options);
}

/// Standalone utility function to demangle the given type as string.
///
/// If performance is an issue when demangling multiple symbols,
/// Context::demangleTypeAsString should be used instead.
/// \param MangledName The mangled name string.
/// \returns The demangled string.
inline std::string
demangleTypeAsString(llvm::StringRef MangledName,
                     const DemangleOptions &Options = DemangleOptions()) {
  return demangleTypeAsString(MangledName.data(),
                              MangledName.size(), Options);
}
  

enum class OperatorKind {
  NotOperator,
  Prefix,
  Postfix,
  Infix,
};

/// A mangling error, which consists of an error code and a Node pointer
struct [[nodiscard]] ManglingError {
  enum Code {
    Success = 0,
    AssertionFailed,
    Uninitialized,
    TooComplex,
    BadNodeKind,
    BadNominalTypeKind,
    NotAStorageNode,
    UnsupportedNodeKind,
    UnexpectedBuiltinVectorType,
    UnexpectedBuiltinType,
    MultipleChildNodes,
    WrongNodeType,
    WrongDependentMemberType,
    BadDirectness,
    UnknownEncoding,
    InvalidImplCalleeConvention,
    InvalidImplDifferentiability,
    InvalidImplCoroutineKind,
    InvalidImplFunctionAttribute,
    InvalidImplParameterConvention,
    InvalidImplParameterSending,
    InvalidMetatypeRepresentation,
    MultiByteRelatedEntity,
    BadValueWitnessKind,
    NotAContextNode,
  };

  Code        code;
  NodePointer node;
  unsigned    line;

  ManglingError() : code(Uninitialized), node(nullptr) {}
  ManglingError(Code c) : code(c), node(nullptr), line(0) {}
  ManglingError(Code c, NodePointer n, unsigned l) : code(c), node(n), line(l) {}

  bool isSuccess() const { return code == Success; }
};

#define MANGLING_ERROR(c,n)     ManglingError((c), (n), __LINE__)

/// Used as a return type for mangling functions that may fail
template <typename T>
class [[nodiscard]] ManglingErrorOr {
private:
  ManglingError err_;
  T             value_;

public:
  ManglingErrorOr() : err_() {}
  ManglingErrorOr(ManglingError::Code code,
                  NodePointer node = nullptr,
                  unsigned line = 0)
  : err_(code, node, line) {}
  ManglingErrorOr(const ManglingError &err) : err_(err) {}
  ManglingErrorOr(const T &t) : err_(ManglingError::Success), value_(t) {}
  ManglingErrorOr(T &&t) : err_(ManglingError::Success), value_(std::move(t)) {}

  bool isSuccess() const { return err_.code == ManglingError::Success; }

  const ManglingError &error() const { return err_; }

  const T &result() const {
    assert(isSuccess());
    return value_;
  }
};

/// Remangle a demangled parse tree.
ManglingErrorOr<std::string>
mangleNode(NodePointer root,
           Mangle::ManglingFlavor Flavor = Mangle::ManglingFlavor::Default);

using SymbolicResolver = llvm::function_ref<Demangle::NodePointer(
    SymbolicReferenceKind, const void *)>;

/// Remangle a demangled parse tree, using a callback to resolve
/// symbolic references.
ManglingErrorOr<std::string>
mangleNode(NodePointer root, SymbolicResolver resolver,
           Mangle::ManglingFlavor Flavor = Mangle::ManglingFlavor::Default);

/// Remangle a demangled parse tree, using a callback to resolve
/// symbolic references.
///
/// The returned string is owned by \p Factory. This means \p Factory must stay
/// alive as long as the returned string is used.
ManglingErrorOr<llvm::StringRef>
mangleNode(NodePointer root, SymbolicResolver resolver, NodeFactory &Factory,
           Mangle::ManglingFlavor Flavor = Mangle::ManglingFlavor::Default);

/// Remangle in the old mangling scheme.
///
/// This is only used for objc-runtime names.
ManglingErrorOr<std::string> mangleNodeOld(NodePointer root);

/// Remangle in the old mangling scheme.
///
/// This is only used for objc-runtime names.
/// The returned string is owned by \p Factory. This means \p Factory must stay
/// alive as long as the returned string is used.
ManglingErrorOr<llvm::StringRef> mangleNodeOld(NodePointer node,
                                               NodeFactory &Factory);

/// Remangle in the old mangling scheme and embed the name in "_Tt<name>_".
///
/// The returned string is null terminated and owned by \p Factory. This means
/// \p Factory must stay alive as long as the returned string is used.
ManglingErrorOr<const char *> mangleNodeAsObjcCString(NodePointer node,
                                                      NodeFactory &Factory);

/// Transform the node structure to a string.
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

/// Transforms a mangled key path accessor thunk helper
/// into the identfier/subscript that would be used to invoke it in swift code.
std::string keyPathSourceString(const char *MangledName,
                                size_t MangledNameLength);

/// A class for printing to a std::string.
class DemanglerPrinter {
public:
  DemanglerPrinter() = default;

  DemanglerPrinter &operator<<(llvm::StringRef Value) & {
    Stream.append(Value.data(), Value.size());
    return *this;
  }
  
  DemanglerPrinter &operator<<(char c) & {
    Stream.push_back(c);
    return *this;
  }
  DemanglerPrinter &operator<<(unsigned long long n) &;
  DemanglerPrinter &operator<<(long long n) &;
  DemanglerPrinter &operator<<(unsigned long n) & {
    return *this << (unsigned long long)n;
  }
  DemanglerPrinter &operator<<(long n) & {
    return *this << (long long)n;
  }
  DemanglerPrinter &operator<<(unsigned n) & {
    return *this << (unsigned long long)n;
  }
  DemanglerPrinter &operator<<(int n) & {
    return *this << (long long)n;
  }
  
  template<typename T>
  DemanglerPrinter &&operator<<(T &&x) && {
    return std::move(*this << std::forward<T>(x));
  }
  
  DemanglerPrinter &writeHex(unsigned long long n) &;
 
  std::string &&str() && { return std::move(Stream); }

  llvm::StringRef getStringRef() const { return Stream; }

  /// Shrinks the buffer.
  void resetSize(size_t toPos) {
    assert(toPos <= Stream.size());
    Stream.resize(toPos);
  }
private:
  std::string Stream;
};

/// Returns a the node kind \p k as string.
const char *getNodeKindString(swift::Demangle::Node::Kind k);

/// Prints the whole node tree \p Root in readable form into a std::string.
///
/// Useful for debugging.
std::string getNodeTreeAsString(NodePointer Root);

bool nodeConsumesGenericArgs(Node *node);

bool isSpecialized(Node *node);

ManglingErrorOr<NodePointer> getUnspecialized(Node *node, NodeFactory &Factory);

/// Returns true if the node \p kind refers to a context node, e.g. a nominal
/// type or a function.
bool isContext(Node::Kind kind);

/// Returns true if the node \p kind refers to a node which is placed before a
/// function node, e.g. a specialization attribute.
bool isFunctionAttr(Node::Kind kind);

/// Form a StringRef around the mangled name starting at base, if the name may
/// contain symbolic references.
llvm::StringRef makeSymbolicMangledNameStringRef(const char *base);

/// Produce the mangled name for the nominal type descriptor of a type
/// referenced by its module and type name.
std::string mangledNameForTypeMetadataAccessor(
    llvm::StringRef moduleName, llvm::StringRef typeName, Node::Kind typeKind,
    Mangle::ManglingFlavor Flavor = Mangle::ManglingFlavor::Default);

class NodePrinter {
private:
  DemanglerPrinter Printer;
  DemangleOptions Options;
  bool SpecializationPrefixPrinted = false;
  bool isValid = true;

public:
  NodePrinter(DemangleOptions options) : Options(options) {}

  std::string printRoot(NodePointer root) {
    isValid = true;
    print(root, 0);
    if (isValid)
      return std::move(Printer).str();
    return "";
  }

private:
  static const unsigned MaxDepth = 768;

  /// Called when the node tree in valid.
  ///
  /// The demangler already catches most error cases and mostly produces valid
  /// node trees. But some cases are difficult to catch in the demangler and
  /// instead the NodePrinter bails.
  void setInvalid() { isValid = false; }

  void printChildren(Node::iterator begin, Node::iterator end, unsigned depth,
                     const char *sep = nullptr);

  void printChildren(NodePointer Node, unsigned depth,
                     const char *sep = nullptr);

  NodePointer getFirstChildOfKind(NodePointer Node, Node::Kind kind);

  void printBoundGenericNoSugar(NodePointer Node, unsigned depth);

  void printOptionalIndex(NodePointer node);

  static bool isSwiftModule(NodePointer node) {
    return (node->getKind() == Node::Kind::Module &&
            node->getText() == STDLIB_NAME);
  }

  bool printContext(NodePointer Context);

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

  enum class TypePrinting { NoType, WithColon, FunctionStyle };

  /// Determine whether this is a "simple" type, from the type-simple
  /// production.
  bool isSimpleType(NodePointer Node);

  void printWithParens(NodePointer type, unsigned depth);

  SugarType findSugar(NodePointer Node);

  void printBoundGeneric(NodePointer Node, unsigned depth);

  NodePointer getChildIf(NodePointer Node, Node::Kind Kind);

  void printFunctionParameters(NodePointer LabelList, NodePointer ParameterType,
                               unsigned depth, bool showTypes);

  void printFunctionType(NodePointer LabelList, NodePointer node,
                         unsigned depth);

  void printImplFunctionType(NodePointer fn, unsigned depth);

  void printGenericSignature(NodePointer Node, unsigned depth);

  void printFunctionSigSpecializationParams(NodePointer Node, unsigned depth);

  void printSpecializationPrefix(NodePointer node, StringRef Description,
                                 unsigned depth,
                                 StringRef ParamPrefix = StringRef());

  /// The main big print function.
  NodePointer print(NodePointer Node, unsigned depth,
                    bool asPrefixContext = false);

  NodePointer printAbstractStorage(NodePointer Node, unsigned depth,
                                   bool asPrefixContent, StringRef ExtraName);

  /// Utility function to print entities.
  ///
  /// \param Entity The entity node to print
  /// \param depth The depth in the print() call tree.
  /// \param asPrefixContext Should the entity printed as a context which as a
  ///        prefix to another entity, e.g. the Abc in Abc.def()
  /// \param TypePr How should the type of the entity be printed, if at all.
  ///        E.g. with a colon for properties or as a function type.
  /// \param hasName Does the entity has a name, e.g. a function in contrast to
  ///        an initializer.
  /// \param ExtraName An extra name added to the entity name (if any).
  /// \param ExtraIndex An extra index added to the entity name (if any),
  ///        e.g. closure #1
  /// \param OverwriteName If non-empty, print this name instead of the one
  ///        provided by the node. Gets printed even if hasName is false.
  /// \return If a non-null node is returned it's a context which must be
  ///         printed in postfix-form after the entity: "<entity> in <context>".
  NodePointer printEntity(NodePointer Entity, unsigned depth,
                          bool asPrefixContext, TypePrinting TypePr,
                          bool hasName, StringRef ExtraName = "",
                          int ExtraIndex = -1, StringRef OverwriteName = "");

  /// Print the type of an entity.
  ///
  /// \param Entity The entity.
  /// \param type The type of the entity.
  /// \param genericFunctionTypeList If not null, the generic argument types
  ///           which is printed in the generic signature.
  /// \param depth The depth in the print() call tree.
  void printEntityType(NodePointer Entity, NodePointer type,
                       NodePointer genericFunctionTypeList, unsigned depth);
};

SWIFT_END_INLINE_NAMESPACE
} // end namespace Demangle
} // end namespace swift

#endif // SWIFT_DEMANGLING_DEMANGLE_H
