//===--- ObjectBuilder.h ---------------------------------------*- C++ -*--===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines ObjectBuilder, a class that's useful for building objects
// with relative references to other objects.
//
//===----------------------------------------------------------------------===//

#ifndef OBJECT_BUILDER_H
#define OBJECT_BUILDER_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MathExtras.h"
#include <vector>

namespace swift {

class RootObjectBuilder;

template <class T>
class SubobjectBuilder;

template <class T>
class ObjectRef;

class AnyObjectBuilder {
protected:
  struct Object;
  template <class T> friend class ObjectRef;

  using RelativeReferenceType = int32_t;

  struct Reference {
    /// The offset at which the reference is stored in the data.
    size_t offset;

    /// The target of the reference.
    Object *referent;

    /// A value to add to the value of the reference.  Generally used for
    /// things like storing small integers in alignment bits.
    size_t addend;

    /// Return a copy of this reference for a containing object in
    /// which the referring object has been placed at the given offset.
    Reference atOffset(size_t offsetInContainer) const {
      return { offsetInContainer + offset, referent, addend };
    }
  };

  struct Object {
    static constexpr size_t invalidOffset = -1;
    std::vector<char> data;
    std::vector<Reference> refs;
    size_t requiredAlignment;
    size_t assignedOffset = invalidOffset;

    Object(size_t requiredAlignment) : requiredAlignment(requiredAlignment) {
      assert(llvm::isPowerOf2_64(requiredAlignment));
    }
    Object(const Object &) = delete;
    Object &operator=(const Object &) = delete;

    bool hasAssignedOffset() const {
      return assignedOffset != invalidOffset;
    }

    /// Given that this object has been merged into the topmost object,
    /// free any memory associated with it.
    void clearMemory() {
      assert(hasAssignedOffset());
      data = std::vector<char>();
      if (!refs.empty())
        refs = std::vector<Reference>();
    }
  };

  RootObjectBuilder *root;
  Object *self;

  AnyObjectBuilder(RootObjectBuilder *root, Object *self)
    : root(root), self(self) {}

public:
  AnyObjectBuilder(const AnyObjectBuilder &) = delete;
  AnyObjectBuilder &operator=(const AnyObjectBuilder &) = delete;
  AnyObjectBuilder &operator=(AnyObjectBuilder &&) = delete;

  AnyObjectBuilder(AnyObjectBuilder &&other)
      : root(other.root), self(other.self) {
    // Flag that the other builder has been moved from.
    other.root = nullptr;
  }

  /// Round the current offset up to a particular alignment.
  void padToAlignment(size_t alignment);

  /// Add a fixed-size value (in native byte ordering).
  void add8(uint8_t value);
  void add16(uint16_t value);
  void add32(uint32_t value);
  void add64(uint64_t value);

  /// Add a sequence of bytes.
  void addBytes(llvm::StringRef value);
  void addBytes(const void *data, size_t length);

  /// Add a string of ('\0'-terminated) bytes.
  void addString(llvm::StringRef value);

  /// Add an indirect reference to a string of '\0'-terminated bytes.
  void addRelativeReferenceToString(llvm::StringRef value,
                                    size_t alignment = alignof(char),
                                    size_t addend = 0);

  /// Create a object which will be laid out somewhere near this
  /// object.  A relative reference can be built to the object,
  /// but this method doesn't itself do so; it must be added later
  /// with `addRelativeReference(subBuilder.ref())`.
  template <class T>
  SubobjectBuilder<T> createSubobject(size_t alignment = alignof(T));

  /// Add a relative reference to the given subobject.
  template <class T>
  void addRelativeReference(ObjectRef<T> object, size_t addend = 0);

  /// Add a relative reference to a variable that holds the
  /// given pointer.
  void addRelativeIndirectReference(const void *value, size_t addend = 0);

  /// Add an absolute pointer.
  void addPointer(const void *value);
};

class RootObjectBuilder : public AnyObjectBuilder {
  friend class AnyObjectBuilder;
  std::vector<std::unique_ptr<Object>> subobjects;

  void fillReference(const Reference &ref);

protected:
  char *finishedPointer = nullptr;
  void *finishImpl();

public:
  RootObjectBuilder(size_t alignment)
    : AnyObjectBuilder(this, new Object(alignment)) {
    // We directly retain ownership of self instead of adding it to
    // subobjects.
  }

  // We can't copy/move a root builder because subbuilders will have
  // unsafe pointers to it.
  RootObjectBuilder(const RootObjectBuilder &other) = delete;
  RootObjectBuilder &operator=(const RootObjectBuilder &other) = delete;

  ~RootObjectBuilder() {
    delete self;
  }

  bool isFinished() const {
    return finishedPointer != nullptr;
  }

  template <class T>
  T *findObject(ObjectRef<T> object) {
    assert(isFinished() && "finding object in unfinished builder");
    assert(object.object->hasAssignedOffset());
    return reinterpret_cast<T*>(
                      finishedPointer + object.object->assignedOffset);
  }

  template <class T>
  const T *findObject(ObjectRef<T> object) const {
    assert(isFinished() && "finding object in unfinished builder");
    assert(object.object->hasAssignedOffset());
    return reinterpret_cast<const T *>(
                      finishedPointer + object.object->assignedOffset);
  }
};

/// The main type to interact with.
template <class T>
class ObjectBuilder : public RootObjectBuilder {
public:
  ObjectBuilder(size_t alignment = alignof(T))
    : RootObjectBuilder(alignment) {}

  ObjectRef<T> ref() const {
    return ObjectRef<T>(root, self);
  }

  T *finish() {
    return static_cast<T*>(finishImpl());
  }

  T *get() {
    assert(isFinished());
    return reinterpret_cast<T*>(finishedPointer);
  }

  const T *get() const {
    assert(isFinished());
    return reinterpret_cast<const T*>(finishedPointer);
  }
};

template <class T>
class SubobjectBuilder : public AnyObjectBuilder {
  friend class AnyObjectBuilder;

  SubobjectBuilder(RootObjectBuilder *root, Object *self)
    : AnyObjectBuilder(root, self) {}

public:
  ObjectRef<T> ref() const {
    return ObjectRef<T>(root, self);
  }
};

template <class T>
class ObjectRef {
  friend class AnyObjectBuilder;

  RootObjectBuilder *root;
  AnyObjectBuilder::Object *object;

public:
  // Only really callable from builder subclasses.
  ObjectRef(RootObjectBuilder *root, AnyObjectBuilder::Object *object)
    : root(root), object(object) {
    assert(root && object);
  }

  constexpr ObjectRef() : root(nullptr), object(nullptr) {}

  T *get() const {
    return root->findObject(*this);
  }
};

inline void AnyObjectBuilder::padToAlignment(size_t alignment) {
  assert(root && "builder was moved from");
  assert(!root->isFinished());
  assert(llvm::isPowerOf2_64(alignment));
  size_t rawOffset = self->data.size();
  size_t offset = (rawOffset + alignment - 1) & ~(alignment - 1);
  if (offset != rawOffset)
    self->data.resize(offset);
}

inline void AnyObjectBuilder::addBytes(const void *data, size_t length) {
  assert(root && "builder was moved from");
  assert(!root->isFinished());
  self->data.insert(self->data.end(),
                    ((const char *) data),
                    ((const char *) data) + length);
}

inline void AnyObjectBuilder::add8(uint8_t value) {
  assert(root && "builder was moved from");
  assert(!root->isFinished());
  self->data.push_back(value);
}

inline void AnyObjectBuilder::add16(uint16_t value) {
  addBytes(&value, sizeof(value));
}
inline void AnyObjectBuilder::add32(uint32_t value) {
  addBytes(&value, sizeof(value));
}
inline void AnyObjectBuilder::add64(uint64_t value) {
  addBytes(&value, sizeof(value));
}
inline void AnyObjectBuilder::addBytes(llvm::StringRef value) {
  addBytes(value.begin(), value.size());
}
inline void AnyObjectBuilder::addString(llvm::StringRef value) {
  addBytes(value);
  add8(0);
}
inline void AnyObjectBuilder::addPointer(const void *object) {
  addBytes(&object, sizeof(object));
}

template <class T>
inline void AnyObjectBuilder::addRelativeReference(ObjectRef<T> object,
                                                   size_t addend) {
  assert(root && "builder was moved from");
  assert(root == object.root &&
         "adding reference to object from different builder");

  // This assertion can be weakened if we ever need to do relative
  // references to the interior of subobjects.
  assert(addend < object.object->requiredAlignment &&
         "addend too large for requested alignment; will be ambiguous");

  auto offset = self->data.size();
  RelativeReferenceType ref = 0;
  addBytes(&ref, sizeof(ref));
  self->refs.push_back({offset, object.object, addend});
}

inline void AnyObjectBuilder::addRelativeIndirectReference(const void *value,
                                                           size_t addend) {
  auto sub = createSubobject<const void *>();
  sub.addPointer(value);
  addRelativeReference(sub.ref(), addend);
}

inline void
AnyObjectBuilder::addRelativeReferenceToString(llvm::StringRef value,
                                               size_t alignment,
                                               size_t addend) {
  auto sub = createSubobject<const char>(alignment);
  sub.addString(value);
  addRelativeReference(sub.ref(), addend);
}

template <class T>
inline SubobjectBuilder<T>
AnyObjectBuilder::createSubobject(size_t alignment) {
  auto object = new Object(alignment);
  root->subobjects.emplace_back(object);
  return SubobjectBuilder<T>(root, object);
}

inline void *RootObjectBuilder::finishImpl() {
  assert(!isFinished());

  // Add all the subobjects to the buffer in self.
  for (auto &ownedObject: subobjects) {
    auto obj = ownedObject.get();

    // Determine the offset of the object.
    assert(!obj->hasAssignedOffset());
    padToAlignment(obj->requiredAlignment);
    size_t offset = self->data.size();
    obj->assignedOffset = offset;

    // Add the object data.
    self->data.insert(self->data.end(), obj->data.begin(), obj->data.end());

    // Handle refs in the object.
    for (auto &objRef : obj->refs) {
      auto ref = objRef.atOffset(offset);
      if (ref.referent->hasAssignedOffset())
        fillReference(ref);
      else
        self->refs.push_back(ref);
    }

    obj->clearMemory();
  }

  // Handle all the unresolved references.
  for (auto &ref : self->refs) {
    fillReference(ref);
  }

  auto leadingOffset = [&] {
    // Dynamically align the start of the buffer to the required alignment.
    uintptr_t base = reinterpret_cast<uintptr_t>(&self->data[0]);
    uintptr_t alignedBase = (base + self->requiredAlignment - 1)
                               & ~uintptr_t(self->requiredAlignment - 1);
    return alignedBase - base;
  };

  finishedPointer = &self->data[0];

  // If we need a non-zero leading offset, adjust the buffer.
  if (auto requiredPadding = leadingOffset()) {
    // Make sure we have space for the padding.
    self->data.reserve(self->data.size() + requiredPadding);

    // That might have reallocated the vector, in which case
    // the required padding may have changed.
    requiredPadding = leadingOffset();
    if (requiredPadding)
      self->data.insert(self->data.begin(), requiredPadding, '\0');

    assert(requiredPadding == leadingOffset() &&
           "insert() after reserve() changed leading offset");

    finishedPointer = &self->data[requiredPadding];
  }

  return finishedPointer;
}

inline void RootObjectBuilder::fillReference(const Reference &ref) {
  assert(ref.referent->hasAssignedOffset());
  ptrdiff_t difference = 
    (ptrdiff_t) (ref.referent->assignedOffset + ref.addend - ref.offset);
  auto truncated = (RelativeReferenceType) difference;
  assert(difference == truncated && "built object > 2GB?");
  assert(ref.offset + sizeof(truncated) <= self->data.size());
  std::memcpy(&self->data[ref.offset], &truncated, sizeof(truncated));
}

} // end namespace swift

#endif