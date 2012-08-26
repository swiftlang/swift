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

class PhysicalPathComponent;
class LogicalPathComponent;

/// An enumeration indicating whether to "preserve" a value or "consume" it.
enum ShouldPreserveValues : bool {
  ConsumeValues = false,
  PreserveValues = true
};

/// An l-value path component represents a chunk of the access path to
/// an object.  Path components may be either "physical" or "logical".
/// A physical path involves elementary address manipulations; these
/// address manipulations may be in some way dynamic, but they are
/// ultimately just pointer arithmetic.  A logical path requires
/// getter/setter logic.
///
/// This divide between physical/logical is closely related to the
/// fragile/resilient split, with two primary differences:
///   - Any sort of implementation can be fragile.  For example, a
///     computed field can still be fragile, meaning that it is known
///     to be implemented with a getter/setter.  The known
///     implementation must be a direct offset in order to qualify as
///     physical.
///   - A path component's implementation can be resilient and yet
///     still qualify for physical access if we are in a privileged
///     component.
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

  // These are implemented inline after the respective class declarations.

  PhysicalPathComponent &asPhysical();
  const PhysicalPathComponent &asPhysical() const;

  LogicalPathComponent &asLogical();
  const LogicalPathComponent &asLogical() const;
};

/// An abstract class for "physical" path components, i.e. path
/// components that can be accessed as address manipulations.  See the
/// comment for PathComponent for more information.
class PhysicalPathComponent : public PathComponent {
  virtual void _anchor();

protected:
  PhysicalPathComponent() : PathComponent(true) {}

public:
  virtual OwnedAddress offset(IRGenFunction &IGF,
                              OwnedAddress base) const = 0;
};

inline PhysicalPathComponent &PathComponent::asPhysical() {
  assert(isPhysical());
  return static_cast<PhysicalPathComponent&>(*this);
}
inline const PhysicalPathComponent &PathComponent::asPhysical() const {
  assert(isPhysical());
  return static_cast<const PhysicalPathComponent&>(*this);
}

/// An abstract class for "logical" path components, i.e. path
/// components that require getter/setter methods to access.  See the
/// comment for PathComponent for more information.
class LogicalPathComponent : public PathComponent {
  virtual void _anchor();

protected:
  LogicalPathComponent() : PathComponent(false) {}

public:
  /// Perform a store operation with a value produced by the given
  /// expression.
  virtual void storeRValue(IRGenFunction &IGF, Expr *rvalue,
                           Address base,
                           ShouldPreserveValues preserve) const = 0;

  /// Perform a store operation with a value held in memory.
  virtual void storeMaterialized(IRGenFunction &IGF, Address temp,
                                 Address base,
                                 ShouldPreserveValues preserve) const = 0;

  /// Perform a store operation with a value from the given explosion.
  virtual void storeExplosion(IRGenFunction &IGF, Explosion &value,
                              Address base,
                              ShouldPreserveValues preserve) const = 0;

  /// Perform a load operation from this path into the given
  /// explosion.
  virtual void loadExplosion(IRGenFunction &IGF, Address base,
                             Explosion &exp,
                             ShouldPreserveValues preserve) const = 0;

  /// Perform a load operation from this path into memory
  /// at a given address.
  virtual void loadMaterialized(IRGenFunction &IGF, Address base,
                                Address temp,
                                ShouldPreserveValues preserve) const = 0;

  /// Perform a load operation from this path into temporary
  /// memory.
  virtual OwnedAddress loadAndMaterialize(IRGenFunction &IGF,
                                          OnHeap_t onHeap, Address base,
                                    ShouldPreserveValues preserve) const = 0;
};

inline LogicalPathComponent &PathComponent::asLogical() {
  assert(isLogical());
  return static_cast<LogicalPathComponent&>(*this);
}
inline const LogicalPathComponent &PathComponent::asLogical() const {
  assert(isLogical());
  return static_cast<const LogicalPathComponent&>(*this);
}

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
