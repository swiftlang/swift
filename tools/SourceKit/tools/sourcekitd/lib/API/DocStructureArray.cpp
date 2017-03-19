//===--- DocStructureArray.cpp --------------------------------------------===//
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

#include "sourcekitd/DocStructureArray.h"
#include "DictionaryKeys.h"
#include "SourceKit/Core/LLVM.h"
#include "SourceKit/Support/UIdent.h"
#include "sourcekitd/CompactArray.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

using namespace SourceKit;
using namespace sourcekitd;

namespace {
struct Node {
  // Scalars
  unsigned Offset;
  unsigned Length;
  UIdent Kind;
  UIdent AccessLevel;
  UIdent SetterAccessLevel;
  unsigned NameOffset;
  unsigned NameLength;
  unsigned BodyOffset;
  unsigned BodyLength;
  std::string DisplayName;
  std::string TypeName;
  std::string RuntimeName;
  std::string SelectorName;
  // Arrays
  unsigned InheritedTypesOffset;
  unsigned AttrsOffset;

  // Children
  struct Element {
    UIdent Kind;
    unsigned Offset;
    unsigned Length;
  };
  SmallVector<Element, 4> elements;
  SmallVector<unsigned, 4> childIndices;
};
}

struct DocStructureArrayBuilder::Implementation {
  typedef CompactArrayBuilder<StringRef> InheritedTypesBuilder;
  SmallVector<char, 256> inheritedTypesBuffer;
  typedef CompactArrayBuilder<UIdent> AttrsBuilder;
  SmallVector<char, 256> attrsBuffer;
  typedef CompactArrayBuilder<UIdent, unsigned, unsigned> ElementsBuilder;
  SmallVector<char, 256> elementsBuffer;
  typedef CompactArrayBuilder<unsigned> StructureArrayBuilder;
  SmallVector<char, 256> structureArrayBuffer;

  CompactArrayBuilder<unsigned,            // Offset
                      unsigned,            // Length
                      UIdent,              // Kind
                      UIdent,              // AccessLevel
                      UIdent,              // SetterAccessLevel
                      unsigned,            // NameOffset
                      unsigned,            // NameLength
                      unsigned,            // BodyOffset
                      unsigned,            // BodyLength
                      Optional<StringRef>, // DisplayName
                      Optional<StringRef>, // TypeName
                      Optional<StringRef>, // RuntimeName
                      Optional<StringRef>, // SelectorName
                      unsigned,            // InheritedTypesOffset
                      unsigned,            // AttrsOffset
                      unsigned,            // ElementsOffset
                      unsigned             // ChildrenOffset
                      >
      structureBuilder;

  SmallVector<Node, 8> structure;
  SmallVector<unsigned, 16> topIndices;

  unsigned addInheritedTypes(ArrayRef<StringRef> inheritedTypes);
  unsigned addAttrs(ArrayRef<UIdent> attrs);
  unsigned addElements(ArrayRef<Node::Element> elements);
  unsigned addChildren(ArrayRef<unsigned> offsets);

  Implementation();
};

DocStructureArrayBuilder::Implementation::Implementation() {
  // For each kind of compact array, fill in the first entry in the buffer as
  // empty so we don't duplicate empty arrays anywhere.
  InheritedTypesBuilder().appendTo(inheritedTypesBuffer);
  AttrsBuilder().appendTo(attrsBuffer);
  ElementsBuilder().appendTo(elementsBuffer);
  StructureArrayBuilder().appendTo(structureArrayBuffer);
}

unsigned DocStructureArrayBuilder::Implementation::addInheritedTypes(
    ArrayRef<StringRef> inheritedTypes) {
  if (inheritedTypes.empty())
    return 0;

  InheritedTypesBuilder builder;
  for (StringRef type : inheritedTypes)
    builder.addEntry(type);

  unsigned offset = inheritedTypesBuffer.size();
  builder.appendTo(inheritedTypesBuffer);
  return offset;
}

unsigned
DocStructureArrayBuilder::Implementation::addAttrs(ArrayRef<UIdent> attrs) {
  if (attrs.empty())
    return 0;

  AttrsBuilder builder;
  for (UIdent uid : attrs)
    builder.addEntry(uid);

  unsigned offset = attrsBuffer.size();
  builder.appendTo(attrsBuffer);
  return offset;
}

unsigned DocStructureArrayBuilder::Implementation::addElements(
    ArrayRef<Node::Element> elements) {
  if (elements.empty())
    return 0;

  ElementsBuilder builder;
  for (auto &element : elements)
    builder.addEntry(element.Kind, element.Offset, element.Length);

  unsigned offset = elementsBuffer.size();
  builder.appendTo(elementsBuffer);
  return offset;
}

unsigned DocStructureArrayBuilder::Implementation::addChildren(
    ArrayRef<unsigned> offsets) {
  if (offsets.empty())
    return 0;

  StructureArrayBuilder builder;
  for (unsigned value : offsets)
    builder.addEntry(value);

  unsigned offset = structureArrayBuffer.size();
  builder.appendTo(structureArrayBuffer);
  return offset;
}

DocStructureArrayBuilder::DocStructureArrayBuilder()
    : impl(*new Implementation()) {}
DocStructureArrayBuilder::~DocStructureArrayBuilder() { delete &impl; }

void DocStructureArrayBuilder::beginSubStructure(
    unsigned Offset, unsigned Length, SourceKit::UIdent Kind,
    SourceKit::UIdent AccessLevel, SourceKit::UIdent SetterAccessLevel,
    unsigned NameOffset, unsigned NameLength, unsigned BodyOffset,
    unsigned BodyLength, StringRef DisplayName, StringRef TypeName,
    StringRef RuntimeName, StringRef SelectorName,
    ArrayRef<StringRef> InheritedTypes, ArrayRef<UIdent> Attrs) {

  Node node = {
      Offset,
      Length,
      Kind,
      AccessLevel,
      SetterAccessLevel,
      NameOffset,
      NameLength,
      BodyOffset,
      BodyLength,
      DisplayName,
      TypeName,
      RuntimeName,
      SelectorName,
      impl.addInheritedTypes(InheritedTypes),
      impl.addAttrs(Attrs),
      {}, // elements
      {}, // children
  };

  impl.structure.push_back(std::move(node));
}

void DocStructureArrayBuilder::addElement(SourceKit::UIdent Kind,
                                          unsigned Offset, unsigned Length) {
  impl.structure.back().elements.push_back({Kind, Offset, Length});
}

void DocStructureArrayBuilder::endSubStructure() {
  Node node = impl.structure.pop_back_val();
  unsigned index = impl.structureBuilder.size();
  if (!impl.structure.empty()) {
    impl.structure.back().childIndices.push_back(index);
  } else {
    impl.topIndices.push_back(index);
  }

  // Canonicalize empty strings to None for the CompactArray.
  auto str = [](StringRef str) -> Optional<StringRef> {
    return str.empty() ? None : Optional<StringRef>(str);
  };

  impl.structureBuilder.addEntry(
      node.Offset, node.Length, node.Kind, node.AccessLevel,
      node.SetterAccessLevel, node.NameOffset, node.NameLength, node.BodyOffset,
      node.BodyLength, str(node.DisplayName), str(node.TypeName),
      str(node.RuntimeName), str(node.SelectorName), node.InheritedTypesOffset,
      node.AttrsOffset, impl.addElements(node.elements),
      impl.addChildren(node.childIndices));
}

std::unique_ptr<llvm::MemoryBuffer> DocStructureArrayBuilder::createBuffer() {
  assert(impl.structure.empty());
  uint64_t topOffset = impl.addChildren(impl.topIndices);

  size_t inheritedTypesBufferSize = impl.inheritedTypesBuffer.size();
  size_t attrsBufferSize = impl.attrsBuffer.size();
  size_t elementsBufferSize = impl.elementsBuffer.size();
  size_t structureArrayBufferSize = impl.structureArrayBuffer.size();
  size_t structureBufferSize = impl.structureBuilder.sizeInBytes();

  // Header:
  // * offset of each section start (5)
  // * offset of top structure array (relative to structure array section) (1)
  size_t headerSize = sizeof(uint64_t) * 6;

  auto result = llvm::MemoryBuffer::getNewUninitMemBuffer(
      inheritedTypesBufferSize + attrsBufferSize + elementsBufferSize +
      structureArrayBufferSize + structureBufferSize + headerSize);

  char *start = const_cast<char *>(result->getBufferStart());
  char *headerPtr = start;
  char *ptr = start + headerSize;

  auto addBuffer = [&](ArrayRef<char> buffer) {
    uint64_t offset = ptr - start;
    memcpy(headerPtr, &offset, sizeof(offset));
    headerPtr += sizeof(offset);
    auto bytes = sizeof(buffer[0]) * buffer.size();
    memcpy(ptr, buffer.data(), bytes);
    ptr += bytes;
  };
  addBuffer(impl.structureArrayBuffer);
  SmallVector<char, 256> structureBuffer;
  impl.structureBuilder.appendTo(structureBuffer);
  addBuffer(structureBuffer);
  addBuffer(impl.elementsBuffer);
  addBuffer(impl.attrsBuffer);
  addBuffer(impl.inheritedTypesBuffer);

  assert(ptr == result->getBufferEnd());
  assert(headerPtr == start + (headerSize - sizeof(topOffset)));
  memcpy(headerPtr, &topOffset, sizeof(topOffset));

  return result;
}

namespace {
struct OutNode {
  // Scalars
  unsigned Offset;
  unsigned Length;
  sourcekitd_uid_t Kind;
  sourcekitd_uid_t AccessLevel;
  sourcekitd_uid_t SetterAccessLevel;
  unsigned NameOffset;
  unsigned NameLength;
  unsigned BodyOffset;
  unsigned BodyLength;
  const char *DisplayName;
  const char *TypeName;
  const char *RuntimeName;
  const char *SelectorName;
  // Arrays
  unsigned InheritedTypesOffset;
  unsigned AttrsOffset;
  unsigned ElementsOffset;
  unsigned ChildIndicesOffset;
};

class DocStructureArrayReader {
public:
  DocStructureArrayReader(void *buffer);

  OutNode readStructure(size_t index);
  CompactArrayReader<unsigned> readStructureArray(size_t offset);

  void *getElementsBuffer(size_t offset) const {
    return (char *)getElementsBufferStart() + offset;
  }
  void *getInheritedTypesBuffer(size_t offset) const {
    return (char *)getInheritedTypesBufferStart() + offset;
  }
  void *getAttrsBuffer(size_t offset) const {
    return (char *)getAttrsBufferStart() + offset;
  }

private:
  void *getStructureArrayBufferStart() const { return getBufferStart(0); }
  void *getStructureBufferStart() const { return getBufferStart(1); }
  void *getElementsBufferStart() const { return getBufferStart(2); }
  void *getAttrsBufferStart() const { return getBufferStart(3); }
  void *getInheritedTypesBufferStart() const { return getBufferStart(4); }
  size_t getTopStructureArrayOffset() const { return getHeaderValue(5); }

  uint64_t getHeaderValue(unsigned index) const;
  void *getBufferStart(unsigned index) const;

private:
  void *buffer;

  typedef CompactArrayReader<unsigned,         // Offset
                             unsigned,         // Length
                             sourcekitd_uid_t, // Kind
                             sourcekitd_uid_t, // AccessLevel
                             sourcekitd_uid_t, // SetterAccessLevel
                             unsigned,         // NameOffset
                             unsigned,         // NameLength
                             unsigned,         // BodyOffset
                             unsigned,         // BodyLength
                             const char *,     // DisplayName
                             const char *,     // TypeName
                             const char *,     // RuntimeName
                             const char *,     // SelectorName
                             unsigned,         // InheritedTypesOffset
                             unsigned,         // AttrsOffset
                             unsigned,         // ElementsOffset
                             unsigned          // ChildrenOffset
                             >
      StructureReader;
};
}

DocStructureArrayReader::DocStructureArrayReader(void *buffer)
    : buffer(buffer) {}

OutNode DocStructureArrayReader::readStructure(size_t index) {
  OutNode result;
  StructureReader reader(getStructureBufferStart());
  reader.readEntries(
      index, result.Offset, result.Length, result.Kind, result.AccessLevel,
      result.SetterAccessLevel, result.NameOffset, result.NameLength,
      result.BodyOffset, result.BodyLength, result.DisplayName, result.TypeName,
      result.RuntimeName, result.SelectorName, result.InheritedTypesOffset,
      result.AttrsOffset, result.ElementsOffset, result.ChildIndicesOffset);
  return result;
}

CompactArrayReader<unsigned>
DocStructureArrayReader::readStructureArray(size_t offset) {
  if (offset == ~size_t(0))
    offset = getTopStructureArrayOffset();
  void *arrayBuffer = (char *)getStructureArrayBufferStart() + offset;
  return CompactArrayReader<unsigned>(arrayBuffer);
}

void *DocStructureArrayReader::getBufferStart(unsigned index) const {
  return (char *)buffer + getHeaderValue(index);
}

uint64_t DocStructureArrayReader::getHeaderValue(unsigned index) const {
  uint64_t headerField;
  memcpy(&headerField, (uint64_t*)buffer + index, sizeof(headerField));
  return headerField;
}

#define APPLY(K, Ty, Field)                                                    \
  do {                                                                         \
    sourcekitd_uid_t key = SKDUIDFromUIdent(K);                                \
    sourcekitd_variant_t var = make##Ty##Variant(Field);                       \
    if (!applier(key, var))                                                    \
      return false;                                                            \
  } while (0)

namespace {
struct ElementReader {
  typedef CompactArrayReader<sourcekitd_uid_t, unsigned, unsigned>
      CompactArrayReaderTy;

  static bool
  dictionary_apply(void *buffer, size_t index,
                   llvm::function_ref<bool(sourcekitd_uid_t,
                                           sourcekitd_variant_t)> applier) {

    CompactArrayReaderTy reader(buffer);
    sourcekitd_uid_t kind;
    unsigned offset;
    unsigned length;
    reader.readEntries(index, kind, offset, length);
    APPLY(KeyKind, UID, kind);
    APPLY(KeyOffset, Int, offset);
    APPLY(KeyLength, Int, length);
    return true;
  }
};

struct InheritedTypeReader {
  typedef CompactArrayReader<const char *> CompactArrayReaderTy;

  static bool
  dictionary_apply(void *buffer, size_t index,
                   llvm::function_ref<bool(sourcekitd_uid_t,
                                           sourcekitd_variant_t)> applier) {

    CompactArrayReaderTy reader(buffer);
    const char *value = nullptr;
    reader.readEntries(index, value);
    if (value)
      APPLY(KeyName, String, value);
    return true;
  }
};

struct AttributesReader {
  typedef CompactArrayReader<sourcekitd_uid_t> CompactArrayReaderTy;

  static bool
  dictionary_apply(void *buffer, size_t index,
                   llvm::function_ref<bool(sourcekitd_uid_t,
                                           sourcekitd_variant_t)> applier) {

    CompactArrayReaderTy reader(buffer);
    sourcekitd_uid_t value;
    reader.readEntries(index, value);
    APPLY(KeyAttribute, UID, value);
    return true;
  }
};

struct DocStructureReader {
  static bool
  dictionary_apply(void *buffer, size_t index,
                   llvm::function_ref<bool(sourcekitd_uid_t,
                                           sourcekitd_variant_t)> applier) {
    auto reader = DocStructureArrayReader(buffer);
    auto node = reader.readStructure(index);

    APPLY(KeyOffset, Int, node.Offset);
    APPLY(KeyLength, Int, node.Length);
    APPLY(KeyKind, UID, node.Kind);
    if (node.AccessLevel)
      APPLY(KeyAccessibility, UID, node.AccessLevel);
    if (node.SetterAccessLevel)
      APPLY(KeySetterAccessibility, UID, node.SetterAccessLevel);
    APPLY(KeyNameOffset, Int, node.NameOffset);
    APPLY(KeyNameLength, Int, node.NameLength);
    if (node.BodyOffset || node.BodyLength) {
      APPLY(KeyBodyOffset, Int, node.BodyOffset);
      APPLY(KeyBodyLength, Int, node.BodyLength);
    }
    if (node.DisplayName)
      APPLY(KeyName, String, node.DisplayName);
    if (node.TypeName)
      APPLY(KeyTypeName, String, node.TypeName);
    if (node.RuntimeName)
      APPLY(KeyRuntimeName, String, node.RuntimeName);
    if (node.SelectorName)
      APPLY(KeySelectorName, String, node.SelectorName);

#define APPLY_ARRAY(Kind, Buf, Key, Off)                                       \
  do {                                                                         \
    sourcekitd_uid_t key = SKDUIDFromUIdent(Key);                              \
    sourcekitd_variant_t var = {                                               \
        {(uintptr_t)getVariantFunctionsFor##Kind##Array(), (uintptr_t)Buf,     \
         Off}};                                                                \
    if (!applier(key, var))                                                    \
      return false;                                                            \
  } while (0)

    if (node.InheritedTypesOffset) {
      void *buf = reader.getInheritedTypesBuffer(node.InheritedTypesOffset);
      APPLY_ARRAY(InheritedTypes, buf, KeyInheritedTypes, 0);
    }
    if (node.AttrsOffset) {
      void *buf = reader.getAttrsBuffer(node.AttrsOffset);
      APPLY_ARRAY(Attributes, buf, KeyAttributes, 0);
    }
    if (node.ElementsOffset) {
      void *buf = reader.getElementsBuffer(node.ElementsOffset);
      APPLY_ARRAY(DocStructureElement, buf, KeyElements, 0);
    }
    if (node.ChildIndicesOffset) {
      APPLY_ARRAY(DocStructure, buffer, KeySubStructure,
                  node.ChildIndicesOffset);
    }
    return true;
  }
};

// data[0] = DocStructureArrayFuncs::funcs
// data[1] = buffer for DocStructureArrayReader
// data[2] = structure array offset
struct DocStructureArrayFuncs {
  static sourcekitd_variant_type_t get_type(sourcekitd_variant_t var) {
    return SOURCEKITD_VARIANT_TYPE_ARRAY;
  }

  static size_t array_get_count(sourcekitd_variant_t array) {
    void *buffer = (void *)array.data[1];
    size_t offset = array.data[2];
    return DocStructureArrayReader(buffer)
        .readStructureArray(offset)
        .getCount();
  }

  static sourcekitd_variant_t array_get_value(sourcekitd_variant_t array,
                                              size_t index) {
    void *buffer = (void *)array.data[1];
    size_t offset = array.data[2];

    auto reader = DocStructureArrayReader(buffer).readStructureArray(offset);
    assert(index < reader.getCount());
    unsigned structureIndex;
    reader.readEntries(index, structureIndex);

    return {{(uintptr_t)&CompactVariantFuncs<DocStructureReader>::Funcs,
             (uintptr_t)buffer, structureIndex}};
  }

  static VariantFunctions funcs;
};
} // end anonymous namespace

VariantFunctions DocStructureArrayFuncs::funcs = {
    get_type,
    nullptr /*AnnotArray_array_apply*/,
    nullptr /*AnnotArray_array_get_bool*/,
    array_get_count,
    nullptr /*AnnotArray_array_get_int64*/,
    nullptr /*AnnotArray_array_get_string*/,
    nullptr /*AnnotArray_array_get_uid*/,
    array_get_value,
    nullptr /*AnnotArray_bool_get_value*/,
    nullptr /*AnnotArray_dictionary_apply*/,
    nullptr /*AnnotArray_dictionary_get_bool*/,
    nullptr /*AnnotArray_dictionary_get_int64*/,
    nullptr /*AnnotArray_dictionary_get_string*/,
    nullptr /*AnnotArray_dictionary_get_value*/,
    nullptr /*AnnotArray_dictionary_get_uid*/,
    nullptr /*AnnotArray_string_get_length*/,
    nullptr /*AnnotArray_string_get_ptr*/,
    nullptr /*AnnotArray_int64_get_value*/,
    nullptr /*AnnotArray_uid_get_value*/
};

VariantFunctions *sourcekitd::getVariantFunctionsForDocStructureElementArray() {
  return &CompactArrayFuncs<ElementReader>::Funcs;
}

VariantFunctions *sourcekitd::getVariantFunctionsForInheritedTypesArray() {
  return &CompactArrayFuncs<InheritedTypeReader>::Funcs;
}

VariantFunctions *sourcekitd::getVariantFunctionsForAttributesArray() {
  return &CompactArrayFuncs<AttributesReader>::Funcs;
}

VariantFunctions *sourcekitd::getVariantFunctionsForDocStructureArray() {
  return &DocStructureArrayFuncs::funcs;
}
