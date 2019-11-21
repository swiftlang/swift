//===--- Demangler.h - String to Node-Tree Demangling -----------*- C++ -*-===//
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
// This file is the compiler-private API of the demangler.
// It should only be used within the swift compiler or runtime library, but not
// by external tools which use the demangler library (like lldb).
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DEMANGLING_DEMANGLER_H
#define SWIFT_DEMANGLING_DEMANGLER_H

#include "swift/Demangling/Demangle.h"

//#define NODE_FACTORY_DEBUGGING

using namespace swift::Demangle;
using llvm::StringRef;

namespace swift {
namespace Demangle {

class CharVector;
  
/// The allocator for demangling nodes and other demangling-internal stuff.
///
/// It implements a simple bump-pointer allocator.
class NodeFactory {

  /// Position in the current slab.
  char *CurPtr = nullptr;

  /// The end of the current slab.
  char *End = nullptr;

  struct Slab {
    // The previously allocated slab.
    Slab *Previous;
    // Tail allocated memory starts here.
  };
  
  /// The head of the single-linked slab list.
  Slab *CurrentSlab = nullptr;

  /// The size of the previously allocated slab.
  ///
  /// The slab size can only grow, even clear() does not reset the slab size.
  /// This initial size is good enough to fit most de-manglings.
  size_t SlabSize = 100 * sizeof(Node);

  static char *align(char *Ptr, size_t Alignment) {
    assert(Alignment > 0);
    return (char*)(((uintptr_t)Ptr + Alignment - 1)
                     & ~((uintptr_t)Alignment - 1));
  }

  static void freeSlabs(Slab *slab);

  /// If not null, the NodeFactory from which this factory borrowed free memory.
  NodeFactory *BorrowedFrom = nullptr;

  /// True if some other NodeFactory borrowed free memory from this factory.
  bool isBorrowed = false;

#ifdef NODE_FACTORY_DEBUGGING
  size_t allocatedMemory = 0;
  static int nestingLevel;
  std::string indent() { return std::string(nestingLevel * 2, ' '); }
#endif

public:

  NodeFactory() {
#ifdef NODE_FACTORY_DEBUGGING
    fprintf(stderr, "%s## New NodeFactory\n", indent().c_str());
    nestingLevel++;
#endif
  }

  /// Provide pre-allocated memory, e.g. memory on the stack.
  ///
  /// Only if this memory overflows, the factory begins to malloc.
  void providePreallocatedMemory(char *Memory, size_t Size) {
#ifdef NODE_FACTORY_DEBUGGING
    fprintf(stderr, "%s++ provide preallocated memory, size = %zu\n", indent().c_str(), Size);
#endif
    assert(!CurPtr && !End && !CurrentSlab);
    CurPtr = Memory;
    End = CurPtr + Size;
  }

  /// Borrow free memory from another factory \p BorrowFrom.
  ///
  /// While this factory is alive, no allocations can be done in the
  /// \p BorrowFrom factory.
  void providePreallocatedMemory(NodeFactory &BorrowFrom) {
    assert(!CurPtr && !End && !CurrentSlab);
    assert(!BorrowFrom.isBorrowed && !BorrowedFrom);
    BorrowFrom.isBorrowed = true;
    BorrowedFrom = &BorrowFrom;
    CurPtr = BorrowFrom.CurPtr;
    End = BorrowFrom.End;
#ifdef NODE_FACTORY_DEBUGGING
    fprintf(stderr, "%s++ borrow memory, size = %zu\n", indent().c_str(), (End - CurPtr));
#endif
  }

  virtual ~NodeFactory() {
    freeSlabs(CurrentSlab);
#ifdef NODE_FACTORY_DEBUGGING
    nestingLevel--;
    fprintf(stderr, "%s## Delete NodeFactory: allocated memory = %zu\n", indent().c_str(), allocatedMemory)
#endif
    if (BorrowedFrom) {
      BorrowedFrom->isBorrowed = false;
    }
  }
  
  virtual void clear();
  
  /// Allocates an object of type T or an array of objects of type T.
  template<typename T> T *Allocate(size_t NumObjects = 1) {
    assert(!isBorrowed);
    size_t ObjectSize = NumObjects * sizeof(T);
    CurPtr = align(CurPtr, alignof(T));
#ifdef NODE_FACTORY_DEBUGGING
    fprintf(stderr, "%salloc %zu, CurPtr = %p\n", indent().c_str(), ObjectSize, (void *)CurPtr)
    allocatedMemory += ObjectSize;
#endif

    // Do we have enough space in the current slab?
    if (CurPtr + ObjectSize > End) {
      // No. We have to malloc a new slab.
      // We double the slab size for each allocated slab.
      SlabSize = std::max(SlabSize * 2, ObjectSize + alignof(T));
      size_t AllocSize = sizeof(Slab) + SlabSize;
      Slab *newSlab = (Slab *)malloc(AllocSize);

      // Insert the new slab in the single-linked list of slabs.
      newSlab->Previous = CurrentSlab;
      CurrentSlab = newSlab;

      // Initialize the pointers to the new slab.
      CurPtr = align((char *)(newSlab + 1), alignof(T));
      End = (char *)newSlab + AllocSize;
      assert(CurPtr + ObjectSize <= End);
#ifdef NODE_FACTORY_DEBUGGING
      fprintf(stderr, "%s** new slab %p, allocsize = %zu, CurPtr = %p, End = %p\n",
            indent().c_str(), newSlab, AllocSize, (void *)CurPtr, (void *)End);
#endif
    }
    T *AllocatedObj = (T *)CurPtr;
    CurPtr += ObjectSize;
    return AllocatedObj;
  }

  /// Tries to enlarge the \p Capacity of an array of \p Objects.
  ///
  /// If \p Objects is allocated at the end of the current slab and the slab
  /// has enough free space, the \p Capacity is simply enlarged and no new
  /// allocation needs to be done.
  /// Otherwise a new array of objects is allocated and \p Objects is set to the
  /// new memory address.
  /// The \p Capacity is enlarged at least by \p MinGrowth, but can also be
  /// enlarged by a bigger value.
  template<typename T> void Reallocate(T *&Objects, uint32_t &Capacity,
                                       size_t MinGrowth) {
    assert(!isBorrowed);
    size_t OldAllocSize = Capacity * sizeof(T);
    size_t AdditionalAlloc = MinGrowth * sizeof(T);

#ifdef NODE_FACTORY_DEBUGGING
    fprintf(stderr, "%srealloc: capacity = %d (size = %zu), growth = %zu (size = %zu)\n",
          indent().c_str(), Capacity, OldAllocSize, MinGrowth, AdditionalAlloc);
#endif
    if ((char *)Objects + OldAllocSize == CurPtr
        && CurPtr + AdditionalAlloc <= End) {
      // The existing array is at the end of the current slab and there is
      // enough space. So we are fine.
      CurPtr += AdditionalAlloc;
      Capacity += MinGrowth;
#ifdef NODE_FACTORY_DEBUGGING
      fprintf(stderr, "%s** can grow: %p\n", indent().c_str(), (void *)CurPtr);
      allocatedMemory += AdditionalAlloc;
#endif
      return;
    }
    // We need a new allocation.
    size_t Growth = (MinGrowth >= 4 ? MinGrowth : 4);
    if (Growth < Capacity * 2)
      Growth = Capacity * 2;
    T *NewObjects = Allocate<T>(Capacity + Growth);
    memcpy(NewObjects, Objects, OldAllocSize);
    Objects = NewObjects;
    Capacity += Growth;
  }

  /// Creates a node of kind \p K.
  NodePointer createNode(Node::Kind K);

  /// Creates a node of kind \p K with an \p Index payload.
  NodePointer createNode(Node::Kind K, Node::IndexType Index);

  /// Creates a node of kind \p K with a \p Text payload.
  ///
  /// The \p Text string must be already allocated with the Factory and therefore
  /// it is _not_ copied.
  NodePointer createNodeWithAllocatedText(Node::Kind K, llvm::StringRef Text);

  /// Creates a node of kind \p K with a \p Text payload.
  ///
  /// The \p Text string is copied.
  NodePointer createNode(Node::Kind K, llvm::StringRef Text) {
    return createNodeWithAllocatedText(K, Text.copy(*this));
  }

  /// Creates a node of kind \p K with a \p Text payload.
  ///
  /// The \p Text string is already allocated with the Factory and therefore
  /// it is _not_ copied.
  NodePointer createNode(Node::Kind K, const CharVector &Text);
  
  /// Creates a node of kind \p K with a \p Text payload, which must be a C
  /// string literal.
  ///
  /// The \p Text string is _not_ copied.
  NodePointer createNode(Node::Kind K, const char *Text);
};

/// A vector with a storage managed by a NodeFactory.
///
/// This Vector class only provides the minimal functionality needed by the
/// Demangler.
template<typename T> class Vector {

protected:
  T *Elems = nullptr;
  uint32_t NumElems = 0;
  uint32_t Capacity = 0;

public:
  using iterator = T *;

  Vector() { }

  /// Construct a vector with an initial capacity.
  explicit Vector(NodeFactory &Factory, size_t InitialCapacity) {
    init(Factory, InitialCapacity);
  }

  /// Clears the content and re-allocates the buffer with an initial capacity.
  void init(NodeFactory &Factory, size_t InitialCapacity) {
    Elems = Factory.Allocate<T>(InitialCapacity);
    NumElems = 0;
    Capacity = InitialCapacity;
  }
  
  void free() {
    Capacity = 0;
    Elems = 0;
  }

  void clear() {
    NumElems = 0;
  }

  iterator begin() { return Elems; }
  iterator end() { return Elems + NumElems; }
  
  T &operator[](size_t Idx) {
    assert(Idx < NumElems);
    return Elems[Idx];
  }

  const T &operator[](size_t Idx) const {
    assert(Idx < NumElems);
    return Elems[Idx];
  }
  
  size_t size() const { return NumElems; }

  bool empty() const { return NumElems == 0; }

  T &back() { return (*this)[NumElems - 1]; }

  void resetSize(size_t toPos) {
    assert(toPos <= NumElems);
    NumElems = toPos;
  }

  void push_back(const T &NewElem, NodeFactory &Factory) {
    if (NumElems >= Capacity)
      Factory.Reallocate(Elems, Capacity, /*Growth*/ 1);
    assert(NumElems < Capacity);
    Elems[NumElems++] = NewElem;
  }

  T pop_back_val() {
    if (empty())
      return T();
    T Val = (*this)[NumElems - 1];
    NumElems--;
    return Val;
  }
};

/// A vector of chars (a string) with a storage managed by a NodeFactory.
///
/// This CharVector class only provides the minimal functionality needed by the
/// Demangler.
class CharVector : public Vector<char> {
public:
  // Append another string.
  void append(StringRef Rhs, NodeFactory &Factory);

  // Append an integer as readable number.
  void append(int Number, NodeFactory &Factory);

  // Append an unsigned 64 bit integer as readable number.
  void append(unsigned long long Number, NodeFactory &Factory);

  StringRef str() const {
    return StringRef(Elems, NumElems);
  }
};

/// Kinds of symbolic reference supported.
enum class SymbolicReferenceKind : uint8_t {
  /// A symbolic reference to a context descriptor, representing the
  /// (unapplied generic) context.
  Context,
  /// A symbolic reference to an accessor function, which can be executed in
  /// the process to get a pointer to the referenced entity.
  AccessorFunctionReference,
};

using SymbolicReferenceResolver_t = NodePointer (SymbolicReferenceKind,
                                                 Directness,
                                                 int32_t, const void *);

/// The demangler.
///
/// It de-mangles a string and it also owns the returned node-tree. This means
/// The nodes of the tree only live as long as the Demangler itself.
class Demangler : public NodeFactory {
protected:
  StringRef Text;
  size_t Pos = 0;

  /// Mangling style where function type would have
  /// labels attached to it, instead of having them
  /// as part of the name.
  bool IsOldFunctionTypeMangling = false;

  Vector<NodePointer> NodeStack;
  Vector<NodePointer> Substitutions;

  static const int MaxNumWords = 26;
  StringRef Words[MaxNumWords];
  int NumWords = 0;
  
  std::function<SymbolicReferenceResolver_t> SymbolicReferenceResolver;

  bool nextIf(StringRef str) {
    if (!Text.substr(Pos).startswith(str)) return false;
    Pos += str.size();
    return true;
  }

  char peekChar() {
    if (Pos >= Text.size())
      return 0;
    return Text[Pos];
  }

  char nextChar() {
    if (Pos >= Text.size())
      return 0;
    return Text[Pos++];
  }

  bool nextIf(char c) {
    if (peekChar() != c)
      return false;
    Pos++;
    return true;
  }

  void pushBack() {
    assert(Pos > 0);
    Pos--;
  }

  StringRef consumeAll() {
    StringRef str = Text.drop_front(Pos);
    Pos = Text.size();
    return str;
  }

  void pushNode(NodePointer Nd) {
    NodeStack.push_back(Nd, *this);
  }

  NodePointer popNode() {
    return NodeStack.pop_back_val();
  }

  NodePointer popNode(Node::Kind kind) {
    if (NodeStack.empty())
      return nullptr;

    Node::Kind NdKind = NodeStack.back()->getKind();
    if (NdKind != kind)
      return nullptr;

    return popNode();
  }

  template <typename Pred> NodePointer popNode(Pred pred) {
    if (NodeStack.empty())
      return nullptr;

    Node::Kind NdKind = NodeStack.back()->getKind();
    if (!pred(NdKind))
      return nullptr;
    
    return popNode();
  }

  /// This class handles preparing the initial state for a demangle job in a reentrant way, pushing the
  /// existing state back when a demangle job is completed.
  class DemangleInitRAII {
    Demangler &Dem;
    Vector<NodePointer> NodeStack;
    Vector<NodePointer> Substitutions;
    int NumWords;
    StringRef Text;
    size_t Pos;
    std::function<SymbolicReferenceResolver_t> SymbolicReferenceResolver;
    
  public:
    DemangleInitRAII(Demangler &Dem, StringRef MangledName,
         std::function<SymbolicReferenceResolver_t> SymbolicReferenceResolver);
    ~DemangleInitRAII();
  };
  friend DemangleInitRAII;
  
  void addSubstitution(NodePointer Nd) {
    if (Nd)
      Substitutions.push_back(Nd, *this);
  }

  NodePointer addChild(NodePointer Parent, NodePointer Child);
  NodePointer createWithChild(Node::Kind kind, NodePointer Child);
  NodePointer createType(NodePointer Child);
  NodePointer createWithChildren(Node::Kind kind, NodePointer Child1,
                                 NodePointer Child2);
  NodePointer createWithChildren(Node::Kind kind, NodePointer Child1,
                                 NodePointer Child2, NodePointer Child3);
  NodePointer createWithChildren(Node::Kind kind, NodePointer Child1,
                                 NodePointer Child2, NodePointer Child3,
                                 NodePointer Child4);
  NodePointer createWithPoppedType(Node::Kind kind) {
    return createWithChild(kind, popNode(Node::Kind::Type));
  }

  bool parseAndPushNodes();

  NodePointer changeKind(NodePointer Node, Node::Kind NewKind);

  NodePointer demangleOperator();

  int demangleNatural();
  int demangleIndex();
  NodePointer demangleIndexAsNode();
  NodePointer demangleDependentConformanceIndex();
  NodePointer demangleIdentifier();
  NodePointer demangleOperatorIdentifier();

  std::string demangleBridgedMethodParams();

  NodePointer demangleMultiSubstitutions();
  NodePointer pushMultiSubstitutions(int RepeatCount, size_t SubstIdx);
  NodePointer createSwiftType(Node::Kind typeKind, const char *name);
  NodePointer demangleStandardSubstitution();
  NodePointer createStandardSubstitution(char Subst);
  NodePointer demangleLocalIdentifier();

  NodePointer popModule();
  NodePointer popContext();
  NodePointer popTypeAndGetChild();
  NodePointer popTypeAndGetAnyGeneric();
  NodePointer demangleBuiltinType();
  NodePointer demangleAnyGenericType(Node::Kind kind);
  NodePointer demangleExtensionContext();
  NodePointer demanglePlainFunction();
  NodePointer popFunctionType(Node::Kind kind);
  NodePointer popFunctionParams(Node::Kind kind);
  NodePointer popFunctionParamLabels(NodePointer FuncType);
  NodePointer popTuple();
  NodePointer popTypeList();
  NodePointer popProtocol();
  NodePointer demangleBoundGenericType();
  NodePointer demangleBoundGenericArgs(NodePointer nominalType,
                                    const Vector<NodePointer> &TypeLists,
                                    size_t TypeListIdx);
  NodePointer popAnyProtocolConformanceList();
  NodePointer demangleRetroactiveConformance();
  NodePointer demangleInitializer();
  NodePointer demangleImplParamConvention();
  NodePointer demangleImplResultConvention(Node::Kind ConvKind);
  NodePointer demangleImplFunctionType();
  NodePointer demangleMetatype();
  NodePointer demanglePrivateContextDescriptor();
  NodePointer createArchetypeRef(int depth, int i);
  NodePointer demangleArchetype();
  NodePointer demangleAssociatedTypeSimple(NodePointer GenericParamIdx);
  NodePointer demangleAssociatedTypeCompound(NodePointer GenericParamIdx);

  NodePointer popAssocTypeName();
  NodePointer popAssocTypePath();
  NodePointer getDependentGenericParamType(int depth, int index);
  NodePointer demangleGenericParamIndex();
  NodePointer popProtocolConformance();
  NodePointer demangleRetroactiveProtocolConformanceRef();
  NodePointer popAnyProtocolConformance();
  NodePointer demangleConcreteProtocolConformance();
  NodePointer popDependentProtocolConformance();
  NodePointer demangleDependentProtocolConformanceRoot();
  NodePointer demangleDependentProtocolConformanceInherited();
  NodePointer popDependentAssociatedConformance();
  NodePointer demangleDependentProtocolConformanceAssociated();
  NodePointer demangleThunkOrSpecialization();
  NodePointer demangleGenericSpecialization(Node::Kind SpecKind);
  NodePointer demangleFunctionSpecialization();
  NodePointer demangleFuncSpecParam(Node::Kind Kind);
  NodePointer addFuncSpecParamNumber(NodePointer Param,
                              FunctionSigSpecializationParamKind Kind);

  NodePointer demangleSpecAttributes(Node::Kind SpecKind);

  NodePointer demangleWitness();
  NodePointer demangleSpecialType();
  NodePointer demangleMetatypeRepresentation();
  NodePointer demangleAccessor(NodePointer ChildNode);
  NodePointer demangleFunctionEntity();
  NodePointer demangleEntity(Node::Kind Kind);
  NodePointer demangleVariable();
  NodePointer demangleSubscript();
  NodePointer demangleProtocolList();
  NodePointer demangleProtocolListType();
  NodePointer demangleGenericSignature(bool hasParamCounts);
  NodePointer demangleGenericRequirement();
  NodePointer demangleGenericType();
  NodePointer demangleValueWitness();

  NodePointer demangleObjCTypeName();
  NodePointer demangleTypeMangling();
  NodePointer demangleSymbolicReference(unsigned char rawKind,
                                        const void *at);

  bool demangleBoundGenerics(Vector<NodePointer> &TypeListList,
                             NodePointer &RetroactiveConformances);
  
  void dump();

public:
  Demangler() {}
  
  void clear() override;

  /// Demangle the given symbol and return the parse tree.
  ///
  /// \param MangledName The mangled symbol string, which start with the
  /// mangling prefix $S.
  /// \param SymbolicReferenceResolver A function invoked to resolve symbolic references in
  /// the string. If null, then symbolic references will cause the demangle to fail.
  ///
  /// \returns A parse tree for the demangled string - or a null pointer
  /// on failure.
  /// The lifetime of the returned node tree ends with the lifetime of the
  /// Demangler or with a call of clear().
  NodePointer demangleSymbol(StringRef MangledName,
            std::function<SymbolicReferenceResolver_t> SymbolicReferenceResolver
               = nullptr);

  /// Demangle the given type and return the parse tree.
  ///
  /// \param MangledName The mangled type string, which does _not_ start with
  /// the mangling prefix $S.
  /// \param SymbolicReferenceResolver A function invoked to resolve symbolic references in
  /// the string. If null, then symbolic references will cause the demangle to fail.
  ///
  /// \returns A parse tree for the demangled string - or a null pointer
  /// on failure.
  /// The lifetime of the returned node tree ends with the lifetime of the
  /// Demangler or with a call of clear().
  NodePointer demangleType(StringRef MangledName,
            std::function<SymbolicReferenceResolver_t> SymbolicReferenceResolver
              = nullptr);
};

/// A demangler which uses stack space for its initial memory.
///
/// The \p Size paramter specifies the size of the stack space.
template <size_t Size> class StackAllocatedDemangler : public Demangler {
  char StackSpace[Size];

public:
  StackAllocatedDemangler() {
    providePreallocatedMemory(StackSpace, Size);
  }
};

NodePointer demangleOldSymbolAsNode(StringRef MangledName,
                                    NodeFactory &Factory);
} // end namespace Demangle
} // end namespace swift

#endif // SWIFT_DEMANGLING_DEMANGLER_H
