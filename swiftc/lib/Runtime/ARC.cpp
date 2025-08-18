//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/Runtime/ARC.h"
#include <iostream>
#include <cassert>
#include <mutex>
#include <unordered_set>
#include <thread>

namespace swiftc {
namespace runtime {

//===----------------------------------------------------------------------===//
// MARK: - ARCRuntime Static Members
//===----------------------------------------------------------------------===//

bool ARCRuntime::initialized = false;
ARCRuntime::Statistics ARCRuntime::stats = {};
bool ARCRuntime::debugging = false;

// Global mutex for thread safety
static std::mutex globalMutex;

// Set of all live objects (for debugging)
static std::unordered_set<const RefCountedObject*> liveObjects;

//===----------------------------------------------------------------------===//
// MARK: - RefCountedObject Implementation
//===----------------------------------------------------------------------===//

void RefCountedObject::retain() const {
  uint32_t oldCount = strongRefCount.fetch_add(1, std::memory_order_relaxed);
  
  // Update statistics
  ARCRuntime::stats.totalRetains++;
  
  if (ARCRuntime::debugging) {
    std::lock_guard<std::mutex> lock(globalMutex);
    std::cout << "[ARC] retain: " << this << " count: " << (oldCount + 1) << std::endl;
  }
  
  // Check for overflow
  if (oldCount >= UINT32_MAX - 1) {
    // In a real implementation, this would be a runtime error
    if (ARCRuntime::debugging) {
      std::cerr << "[ARC] ERROR: Reference count overflow for object " << this << std::endl;
    }
  }
}

void RefCountedObject::release() const {
  uint32_t oldCount = strongRefCount.fetch_sub(1, std::memory_order_acq_rel);
  
  // Update statistics
  ARCRuntime::stats.totalReleases++;
  
  if (ARCRuntime::debugging) {
    std::lock_guard<std::mutex> lock(globalMutex);
    std::cout << "[ARC] release: " << this << " count: " << (oldCount - 1) << std::endl;
  }
  
  assert(oldCount > 0 && "Release called on object with zero reference count");
  
  if (oldCount == 1) {
    // Strong reference count reached zero - deinitialize
    setFlag(ReferenceFlags::Deinitializing);
    
    if (ARCRuntime::debugging) {
      std::lock_guard<std::mutex> lock(globalMutex);
      std::cout << "[ARC] deinitializing: " << this << std::endl;
    }
    
    // Call virtual deinitializer
    const_cast<RefCountedObject*>(this)->deinitialize();
    
    clearFlag(ReferenceFlags::Deinitializing);
    setFlag(ReferenceFlags::Deallocating);
    
    // Release weak reference (for the strong->weak linkage)
    releaseWeak();
  }
}

void RefCountedObject::retainWeak() const {
  uint32_t oldCount = weakRefCount.fetch_add(1, std::memory_order_relaxed);
  
  // Update statistics
  ARCRuntime::stats.totalWeakRetains++;
  
  if (ARCRuntime::debugging) {
    std::lock_guard<std::mutex> lock(globalMutex);
    std::cout << "[ARC] weakRetain: " << this << " weakCount: " << (oldCount + 1) << std::endl;
  }
  
  // Check for overflow
  if (oldCount >= UINT32_MAX - 1) {
    if (ARCRuntime::debugging) {
      std::cerr << "[ARC] ERROR: Weak reference count overflow for object " << this << std::endl;
    }
  }
}

void RefCountedObject::releaseWeak() const {
  uint32_t oldCount = weakRefCount.fetch_sub(1, std::memory_order_acq_rel);
  
  // Update statistics
  ARCRuntime::stats.totalWeakReleases++;
  
  if (ARCRuntime::debugging) {
    std::lock_guard<std::mutex> lock(globalMutex);
    std::cout << "[ARC] weakRelease: " << this << " weakCount: " << (oldCount - 1) << std::endl;
  }
  
  assert(oldCount > 0 && "Weak release called on object with zero weak reference count");
  
  if (oldCount == 1) {
    // Weak reference count reached zero - deallocate
    if (ARCRuntime::debugging) {
      std::lock_guard<std::mutex> lock(globalMutex);
      std::cout << "[ARC] deallocating: " << this << std::endl;
      liveObjects.erase(this);
    }
    
    const_cast<RefCountedObject*>(this)->deallocate();
  }
}

bool RefCountedObject::tryRetain() const {
  uint32_t currentCount = strongRefCount.load(std::memory_order_acquire);
  
  while (currentCount > 0) {
    // Try to increment the reference count
    if (strongRefCount.compare_exchange_weak(currentCount, currentCount + 1, 
                                           std::memory_order_acq_rel, 
                                           std::memory_order_acquire)) {
      // Success
      ARCRuntime::stats.totalRetains++;
      
      if (ARCRuntime::debugging) {
        std::lock_guard<std::mutex> lock(globalMutex);
        std::cout << "[ARC] tryRetain SUCCESS: " << this << " count: " << (currentCount + 1) << std::endl;
      }
      
      return true;
    }
    // compare_exchange_weak updated currentCount, try again
  }
  
  // Object is being deallocated
  if (ARCRuntime::debugging) {
    std::lock_guard<std::mutex> lock(globalMutex);
    std::cout << "[ARC] tryRetain FAILED: " << this << " (object being deallocated)" << std::endl;
  }
  
  return false;
}

void RefCountedObject::deallocate() {
  // Update statistics
  ARCRuntime::stats.totalDeallocations++;
  ARCRuntime::stats.currentAllocations--;
  
  // Default implementation - delete the object
  delete this;
}

//===----------------------------------------------------------------------===//
// MARK: - ARCRuntime Implementation
//===----------------------------------------------------------------------===//

void ARCRuntime::initialize() {
  if (initialized) return;
  
  std::lock_guard<std::mutex> lock(globalMutex);
  if (initialized) return; // Double-checked locking
  
  // Initialize statistics
  stats = {};
  debugging = false;
  
  initialized = true;
  
  std::cout << "[ARC] Runtime initialized" << std::endl;
}

void ARCRuntime::shutdown() {
  if (!initialized) return;
  
  std::lock_guard<std::mutex> lock(globalMutex);
  
  if (debugging && !liveObjects.empty()) {
    std::cout << "[ARC] WARNING: " << liveObjects.size() << " objects still alive at shutdown:" << std::endl;
    for (const auto* obj : liveObjects) {
      std::cout << "  Object: " << obj 
                << " strongCount: " << obj->getStrongRefCount()
                << " weakCount: " << obj->getWeakRefCount() << std::endl;
    }
  }
  
  // Print final statistics
  std::cout << "[ARC] Final statistics:" << std::endl;
  std::cout << "  Total allocations: " << stats.totalAllocations << std::endl;
  std::cout << "  Total deallocations: " << stats.totalDeallocations << std::endl;
  std::cout << "  Current allocations: " << stats.currentAllocations << std::endl;
  std::cout << "  Peak allocations: " << stats.peakAllocations << std::endl;
  std::cout << "  Total retains: " << stats.totalRetains << std::endl;
  std::cout << "  Total releases: " << stats.totalReleases << std::endl;
  std::cout << "  Total weak retains: " << stats.totalWeakRetains << std::endl;
  std::cout << "  Total weak releases: " << stats.totalWeakReleases << std::endl;
  
  initialized = false;
  
  std::cout << "[ARC] Runtime shutdown" << std::endl;
}

void* ARCRuntime::allocate(size_t size, size_t alignment) {
  // Use aligned allocation
  void* ptr = std::aligned_alloc(alignment, size);
  
  if (!ptr) {
    throw std::bad_alloc();
  }
  
  // Update statistics
  stats.totalAllocations++;
  stats.currentAllocations++;
  if (stats.currentAllocations > stats.peakAllocations) {
    stats.peakAllocations = stats.currentAllocations;
  }
  
  if (debugging) {
    std::lock_guard<std::mutex> lock(globalMutex);
    std::cout << "[ARC] allocate: " << ptr << " size: " << size << std::endl;
  }
  
  return ptr;
}

void ARCRuntime::deallocate(void* ptr, size_t size) {
  if (!ptr) return;
  
  if (debugging) {
    std::lock_guard<std::mutex> lock(globalMutex);
    std::cout << "[ARC] deallocate: " << ptr << " size: " << size << std::endl;
  }
  
  std::free(ptr);
}

ARCRuntime::Statistics ARCRuntime::getStatistics() {
  return stats;
}

void ARCRuntime::setDebugging(bool enabled) {
  std::lock_guard<std::mutex> lock(globalMutex);
  debugging = enabled;
  
  if (enabled) {
    std::cout << "[ARC] Debugging enabled" << std::endl;
  } else {
    std::cout << "[ARC] Debugging disabled" << std::endl;
  }
}

void ARCRuntime::checkForCycles() {
  if (!debugging) {
    std::cout << "[ARC] Cycle detection requires debugging to be enabled" << std::endl;
    return;
  }
  
  std::lock_guard<std::mutex> lock(globalMutex);
  
  std::cout << "[ARC] Checking for reference cycles..." << std::endl;
  std::cout << "[ARC] Live objects: " << liveObjects.size() << std::endl;
  
  // Simple cycle detection - objects with reference count > 1 might be in cycles
  size_t suspiciousObjects = 0;
  for (const auto* obj : liveObjects) {
    if (obj->getStrongRefCount() > 1) {
      suspiciousObjects++;
      std::cout << "  Suspicious object: " << obj 
                << " strongCount: " << obj->getStrongRefCount() << std::endl;
    }
  }
  
  if (suspiciousObjects == 0) {
    std::cout << "[ARC] No suspicious objects found" << std::endl;
  } else {
    std::cout << "[ARC] Found " << suspiciousObjects << " objects that might be in cycles" << std::endl;
  }
}

} // namespace runtime
} // namespace swiftc

//===----------------------------------------------------------------------===//
// MARK: - C Interface Implementation
//===----------------------------------------------------------------------===//

extern "C" {

void swift_retain(void* object) {
  if (!object) return;
  
  auto* refCountedObject = static_cast<swiftc::runtime::RefCountedObject*>(object);
  refCountedObject->retain();
}

void swift_release(void* object) {
  if (!object) return;
  
  auto* refCountedObject = static_cast<swiftc::runtime::RefCountedObject*>(object);
  refCountedObject->release();
}

void swift_weakRetain(void* object) {
  if (!object) return;
  
  auto* refCountedObject = static_cast<swiftc::runtime::RefCountedObject*>(object);
  refCountedObject->retainWeak();
}

void swift_weakRelease(void* object) {
  if (!object) return;
  
  auto* refCountedObject = static_cast<swiftc::runtime::RefCountedObject*>(object);
  refCountedObject->releaseWeak();
}

bool swift_tryRetain(void* object) {
  if (!object) return false;
  
  auto* refCountedObject = static_cast<swiftc::runtime::RefCountedObject*>(object);
  return refCountedObject->tryRetain();
}

void* swift_allocObject(size_t size, size_t alignment) {
  return swiftc::runtime::ARCRuntime::allocate(size, alignment);
}

void swift_deallocObject(void* object, size_t size) {
  swiftc::runtime::ARCRuntime::deallocate(object, size);
}

bool swift_isBeingDeallocated(void* object) {
  if (!object) return true;
  
  auto* refCountedObject = static_cast<swiftc::runtime::RefCountedObject*>(object);
  return refCountedObject->hasFlag(swiftc::runtime::ReferenceFlags::Deallocating);
}

} // extern "C"