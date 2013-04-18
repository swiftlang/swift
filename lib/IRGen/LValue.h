//===--- LValue.h - LValue Representation -----------------------*- C++ -*-===//
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
//
// A storage structure for holding l-values in Swift IR.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_LVALUE_H
#define SWIFT_IRGEN_LVALUE_H

#include "Address.h"
#include "IRGen.h"
#include "swift/Basic/DiverseList.h"

namespace llvm {
  template <class T> class SmallVectorImpl;
  class Value;
}

namespace swift {
namespace irgen {
  class IRGenFunction;
  class RValue;


class PathComponent {
  friend class LValue;
  unsigned AllocatedSize : 31;
  const unsigned IsPhysical : 1;

  // This anchor method serves three purposes: it aligns the class to
  // a pointer boundary, it makes the class a primary base so that
  // subclasses will be at offset zero, and it anchors the v-table
  // to a specific file.
  virtual void _anchor();

protected:
  PathComponent(bool isPhysical)
    : IsPhysical(isPhysical) {}

  virtual ~PathComponent() {}

public:
  /// Returns sizeof(the final type), plus any extra storage required.
  size_t allocated_size() const { return AllocatedSize; }

  /// Is this component physical or logical?  If physical, this will
  /// be a subclass of PhysicalPathComponent.  If logical, this will
  /// be a subclass of LogicalPathComponent.
  bool isPhysical() const { return IsPhysical; }
  bool isLogical() const { return !IsPhysical; }

 };

/// An l-value represents a reference to storage holding a value
/// of a type, as opposed to an r-value, which is an actual value
/// of the type.
class LValue {
  DiverseList<PathComponent, 128> Path;

public:
  LValue() = default;
  LValue(const LValue &other) = default;
  LValue(LValue &&other) = default;

  bool isValid() const { return !Path.empty(); }

  /// Is this l-value purely physical?
  bool isPhysical() const {
    assert(isValid());
    for (auto &component : Path)
      if (!component.isPhysical())
        return false;
    return true;
  }

  /// Add a new component at the end of the access path of this l-value.
  template <class T, class... A> T &add(A &&...args) {
    T &component = Path.add<T>(std::forward<A>(args)...);
    component.AllocatedSize = sizeof(T);
    assert(component.allocated_size() == sizeof(T));
    return component;
  }

  template <class T, class... A> T &addWithExtra(A &&...args) {
    size_t extraSize = T::extra_storage_size(std::forward<A>(args)...);
    T &component = Path.addWithExtra<T>(extraSize, std::forward<A>(args)...);
    component.AllocatedSize = sizeof(T) + extraSize;
    assert(component.allocated_size() == sizeof(T) + extraSize);
    return component;
  }

  typedef DiverseListImpl<PathComponent>::iterator iterator;
  typedef DiverseListImpl<PathComponent>::const_iterator const_iterator;

  iterator begin() { return Path.begin(); }
  iterator end() { return Path.end(); }
  const_iterator begin() const { return Path.begin(); }
  const_iterator end() const { return Path.end(); }
};

} // end namespace irgen
} // end namespace swift

#endif
