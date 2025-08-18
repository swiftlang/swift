//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#ifndef SWIFTC_RUNTIME_ARC_H
#define SWIFTC_RUNTIME_ARC_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <atomic>
#include <memory>

namespace swiftc {
namespace runtime {

//===----------------------------------------------------------------------===//
// MARK: - Reference Counting Types
//===----------------------------------------------------------------------===//

/// Reference count type - atomic for thread safety
using RefCount = std::atomic<uint32_t>;

/// Weak reference count type
using WeakRefCount = std::atomic<uint32_t>;

/// Reference counting flags
enum class ReferenceFlags : uint32_t {
  None = 0,
  Weak = 1 << 0,           // Weak reference
  Unowned = 1 << 1,        // Unowned reference  
  Deinitializing = 1 << 2, // Object is being deinitialized
  Deallocating = 1 << 3,   // Object is being deallocated
  Immortal = 1 << 4        // Object should never be deallocated
};

//===----------------------------------------------------------------------===//
// MARK: - Reference Counted Object Base
//===----------------------------------------------------------------------===//

/// Base class for all reference-counted Swift objects
class RefCountedObject {
private:
  mutable RefCount strongRefCount;
  mutable WeakRefCount weakRefCount;
  mutable std::atomic<uint32_t> flags;

public:
  RefCountedObject() 
    : strongRefCount(1), weakRefCount(1), flags(0) {}

  virtual ~RefCountedObject() = default;

  // Non-copyable and non-movable
  RefCountedObject(const RefCountedObject&) = delete;
  RefCountedObject& operator=(const RefCountedObject&) = delete;
  RefCountedObject(RefCountedObject&&) = delete;
  RefCountedObject& operator=(RefCountedObject&&) = delete;

  /// Get current strong reference count
  uint32_t getStrongRefCount() const {
    return strongRefCount.load(std::memory_order_acquire);
  }

  /// Get current weak reference count
  uint32_t getWeakRefCount() const {
    return weakRefCount.load(std::memory_order_acquire);
  }

  /// Check if object has specific flag
  bool hasFlag(ReferenceFlags flag) const {
    return (flags.load(std::memory_order_acquire) & static_cast<uint32_t>(flag)) != 0;
  }

  /// Set flag atomically
  void setFlag(ReferenceFlags flag) {
    flags.fetch_or(static_cast<uint32_t>(flag), std::memory_order_acq_rel);
  }

  /// Clear flag atomically
  void clearFlag(ReferenceFlags flag) {
    flags.fetch_and(~static_cast<uint32_t>(flag), std::memory_order_acq_rel);
  }

  // ARC operations (implemented in ARC.cpp)
  void retain() const;
  void release() const;
  void retainWeak() const;
  void releaseWeak() const;
  bool tryRetain() const;
  
protected:
  /// Called when strong reference count reaches zero
  virtual void deinitialize() {}
  
  /// Called when weak reference count reaches zero
  virtual void deallocate();

private:
  friend class ARCRuntime;
  
  /// Internal retain implementation
  void retainSlow() const;
  
  /// Internal release implementation  
  void releaseSlow() const;
  
  /// Internal weak retain implementation
  void retainWeakSlow() const;
  
  /// Internal weak release implementation
  void releaseWeakSlow() const;
};

//===----------------------------------------------------------------------===//
// MARK: - Weak Reference Wrapper
//===----------------------------------------------------------------------===//

/// Weak reference wrapper for cycle breaking
template<typename T>
class WeakRef {
private:
  mutable T* object;
  
public:
  WeakRef() : object(nullptr) {}
  
  WeakRef(T* obj) : object(obj) {
    if (object) {
      static_cast<const RefCountedObject*>(object)->retainWeak();
    }
  }
  
  WeakRef(const WeakRef& other) : object(other.object) {
    if (object) {
      static_cast<const RefCountedObject*>(object)->retainWeak();
    }
  }
  
  WeakRef& operator=(const WeakRef& other) {
    if (this != &other) {
      T* oldObject = object;
      object = other.object;
      
      if (object) {
        static_cast<const RefCountedObject*>(object)->retainWeak();
      }
      
      if (oldObject) {
        static_cast<const RefCountedObject*>(oldObject)->releaseWeak();
      }
    }
    return *this;
  }
  
  ~WeakRef() {
    if (object) {
      static_cast<const RefCountedObject*>(object)->releaseWeak();
    }
  }
  
  /// Load the object if it's still alive
  T* load() const {
    if (!object) return nullptr;
    
    // Try to retain the object
    if (static_cast<const RefCountedObject*>(object)->tryRetain()) {
      return object;
    }
    
    // Object is being deallocated
    return nullptr;
  }
  
  /// Check if weak reference is nil
  bool isNil() const {
    return object == nullptr || 
           static_cast<const RefCountedObject*>(object)->hasFlag(ReferenceFlags::Deallocating);
  }
};

//===----------------------------------------------------------------------===//
// MARK: - Unowned Reference Wrapper  
//===----------------------------------------------------------------------===//

/// Unowned reference wrapper - crashes if accessed after deallocation
template<typename T>
class UnownedRef {
private:
  T* object;
  
public:
  UnownedRef() : object(nullptr) {}
  
  UnownedRef(T* obj) : object(obj) {
    // Unowned references don't affect reference count
  }
  
  UnownedRef(const UnownedRef& other) : object(other.object) {}
  
  UnownedRef& operator=(const UnownedRef& other) {
    object = other.object;
    return *this;
  }
  
  ~UnownedRef() = default;
  
  /// Access the object (crashes if deallocated)
  T* get() const {
    if (!object) return nullptr;
    
    // Check if object is being deallocated
    if (static_cast<const RefCountedObject*>(object)->hasFlag(ReferenceFlags::Deallocating)) {
      // In a real implementation, this would crash with a runtime error
      return nullptr;
    }
    
    return object;
  }
  
  /// Operator-> for convenient access
  T* operator->() const {
    return get();
  }
  
  /// Operator* for dereferencing
  T& operator*() const {
    T* ptr = get();
    return *ptr;
  }
  
  /// Check if unowned reference is valid
  bool isValid() const {
    return object != nullptr && 
           !static_cast<const RefCountedObject*>(object)->hasFlag(ReferenceFlags::Deallocating);
  }
};

//===----------------------------------------------------------------------===//
// MARK: - Strong Reference Wrapper
//===----------------------------------------------------------------------===//

/// Strong reference wrapper with automatic memory management
template<typename T>
class StrongRef {
private:
  T* object;
  
public:
  StrongRef() : object(nullptr) {}
  
  StrongRef(T* obj) : object(obj) {
    if (object) {
      static_cast<const RefCountedObject*>(object)->retain();
    }
  }
  
  StrongRef(const StrongRef& other) : object(other.object) {
    if (object) {
      static_cast<const RefCountedObject*>(object)->retain();
    }
  }
  
  StrongRef(StrongRef&& other) noexcept : object(other.object) {
    other.object = nullptr;
  }
  
  StrongRef& operator=(const StrongRef& other) {
    if (this != &other) {
      T* oldObject = object;
      object = other.object;
      
      if (object) {
        static_cast<const RefCountedObject*>(object)->retain();
      }
      
      if (oldObject) {
        static_cast<const RefCountedObject*>(oldObject)->release();
      }
    }
    return *this;
  }
  
  StrongRef& operator=(StrongRef&& other) noexcept {
    if (this != &other) {
      if (object) {
        static_cast<const RefCountedObject*>(object)->release();
      }
      
      object = other.object;
      other.object = nullptr;
    }
    return *this;
  }
  
  ~StrongRef() {
    if (object) {
      static_cast<const RefCountedObject*>(object)->release();
    }
  }
  
  /// Get the raw pointer
  T* get() const { return object; }
  
  /// Operator-> for convenient access
  T* operator->() const { return object; }
  
  /// Operator* for dereferencing
  T& operator*() const { return *object; }
  
  /// Check if reference is null
  bool isNull() const { return object == nullptr; }
  
  /// Explicit bool conversion
  explicit operator bool() const { return object != nullptr; }
  
  /// Reset to null
  void reset() {
    if (object) {
      static_cast<const RefCountedObject*>(object)->release();
      object = nullptr;
    }
  }
  
  /// Reset with new object
  void reset(T* newObject) {
    if (object) {
      static_cast<const RefCountedObject*>(object)->release();
    }
    
    object = newObject;
    
    if (object) {
      static_cast<const RefCountedObject*>(object)->retain();
    }
  }
};

//===----------------------------------------------------------------------===//
// MARK: - ARC Runtime Interface
//===----------------------------------------------------------------------===//

class ARCRuntime {
public:
  /// Initialize the ARC runtime system
  static void initialize();
  
  /// Shutdown the ARC runtime system
  static void shutdown();
  
  /// Allocate memory for a reference-counted object
  static void* allocate(size_t size, size_t alignment = alignof(std::max_align_t));
  
  /// Deallocate memory for a reference-counted object
  static void deallocate(void* ptr, size_t size);
  
  /// Get runtime statistics
  struct Statistics {
    uint64_t totalAllocations;
    uint64_t totalDeallocations;
    uint64_t currentAllocations;
    uint64_t peakAllocations;
    uint64_t totalRetains;
    uint64_t totalReleases;
    uint64_t totalWeakRetains;
    uint64_t totalWeakReleases;
  };
  
  static Statistics getStatistics();
  
  /// Enable/disable ARC debugging
  static void setDebugging(bool enabled);
  
  /// Check for reference cycles (debugging feature)
  static void checkForCycles();
  
private:
  static bool initialized;
  static Statistics stats;
  static bool debugging;
};

//===----------------------------------------------------------------------===//
// MARK: - Convenience Functions
//===----------------------------------------------------------------------===//

/// Create a strong reference to an object
template<typename T, typename... Args>
StrongRef<T> makeStrong(Args&&... args) {
  T* object = new T(std::forward<Args>(args)...);
  return StrongRef<T>(object);
}

/// Convert strong reference to weak reference
template<typename T>
WeakRef<T> makeWeak(const StrongRef<T>& strong) {
  return WeakRef<T>(strong.get());
}

/// Convert strong reference to unowned reference
template<typename T>
UnownedRef<T> makeUnowned(const StrongRef<T>& strong) {
  return UnownedRef<T>(strong.get());
}

} // namespace runtime
} // namespace swiftc

//===----------------------------------------------------------------------===//
// MARK: - C Interface for Swift Runtime
//===----------------------------------------------------------------------===//

extern "C" {

/// C interface for Swift runtime - these functions are called by generated code

/// Retain an object
void swift_retain(void* object);

/// Release an object  
void swift_release(void* object);

/// Weak retain an object
void swift_weakRetain(void* object);

/// Weak release an object
void swift_weakRelease(void* object);

/// Try to retain an object (for weak -> strong conversion)
bool swift_tryRetain(void* object);

/// Allocate Swift object
void* swift_allocObject(size_t size, size_t alignment);

/// Deallocate Swift object
void swift_deallocObject(void* object, size_t size);

/// Check if object is being deallocated
bool swift_isBeingDeallocated(void* object);

} // extern "C"

#endif // SWIFTC_RUNTIME_ARC_H