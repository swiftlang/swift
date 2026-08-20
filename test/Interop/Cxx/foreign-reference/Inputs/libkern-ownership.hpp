/// Test libkern's ownership semantics and attributes

#pragma once

#include <swift/bridging>

#define LIBKERN_RETURNS_RETAINED __attribute__((os_returns_retained))
#define LIBKERN_RETURNS_NOT_RETAINED __attribute__((os_returns_not_retained))

class ObjectManager {
  mutable int totalRetains = 0;
  mutable int totalReleases = 0;

  ObjectManager() = default;
  ObjectManager(const ObjectManager&) = delete;

  public:
  static ObjectManager &get() {
    static ObjectManager manager;
    return manager;
  }

  int getTotalRetains() const { return totalRetains; }
  int getTotalReleases() const { return totalReleases; }

  void recordRetain() const { ++totalRetains; }
  void recordRelease() const { ++totalReleases; }

  void reset() {
    totalRetains = 0;
    totalReleases = 0;
  }
} SWIFT_IMMORTAL_REFERENCE;

class BaseObject {
  mutable int retainCount;
  
protected:
  BaseObject() : retainCount(1) {}
  virtual ~BaseObject() = default;

public:
  virtual void retain() const { 
    ObjectManager::get().recordRetain();
    ++retainCount; 
  }
  virtual void release() const {
    ObjectManager::get().recordRelease();
    if (--retainCount == 0)
      delete this;
  }

  virtual int getRetainCount() const { return retainCount; }
} SWIFT_SHARED_REFERENCE(.retain, .release);

class RegistryEntry : public BaseObject {
  RegistryEntry *parent = nullptr;
  RegistryEntry *child = nullptr;

protected:
  RegistryEntry() = default;

  virtual bool attachToParent(RegistryEntry *_Nonnull newParent) {
    if (this == newParent)
      return false;

    newParent->retain();
    parent = newParent;

    retain();
    newParent->child = this;
    return true;
  }

  virtual void detachFromParent() {
    if (!parent)
      return;

    RegistryEntry *oldParent = parent;

    parent = nullptr;
    oldParent->child = nullptr;
    
    oldParent->release();
    release();
  }

  virtual RegistryEntry *_Nullable getParent() const {
    return parent;
  }

  virtual RegistryEntry *_Nullable getChild() const {
    return child;
  }
};

class Service : public RegistryEntry {
  int id = 0;
  
protected:
  explicit Service(int n) : id(n) {}

public:
  int getID() const { return id; }

  // Without an annotation Swift would infer +0, while libkern expects +1.
  static LIBKERN_RETURNS_RETAINED Service *_Nonnull withID(int id) {
    return new Service(id);
  }

  // Without an annotation Swift would infer +0, while libkern expects +1.
  virtual LIBKERN_RETURNS_NOT_RETAINED Service* probe(Service *provider, int *_Nonnull score) {
    *score += 1;
    return this;
  }

  // attachToParent retains both this service and the parent
  virtual bool attach(Service *_Nonnull provider) {
    return attachToParent(provider);
  }

  // detachFromParent releases both this service and the parent
  virtual void detach() {
    detachFromParent();
  }

  // Both Swift and libkern infer +0 (the method's name begins with "get")
  virtual Service *_Nullable getProvider(void) const {
    return static_cast<Service *>(getParent());
  }

  // Without an annotation Swift would infer +0, while libkern expects +1.
  virtual LIBKERN_RETURNS_RETAINED Service *_Nonnull copyService() const {
    return new Service(id);
  }

  // Without an annotation both Swift and libkern would infer +0 (the
  // name of the function starts with "get")
  static LIBKERN_RETURNS_RETAINED Service *_Nonnull getCopyOfService(Service *_Nonnull service) {
    return new Service(service->getID());
  }
};

class NastyService : public Service {
private:
  class SubService {
  public:
    void retain() {}
    void release() {}
  } SWIFT_SHARED_REFERENCE(.retain, .release);

protected:
  explicit NastyService(int id) : Service(id) {}

public:
  static LIBKERN_RETURNS_RETAINED NastyService *_Nonnull withID(int id) {
    return new NastyService(id);
  }

  static SWIFT_RETURNS_RETAINED LIBKERN_RETURNS_NOT_RETAINED NastyService *
  toRetainOrNotToRetain() { // expected-error {{'toRetainOrNotToRetain' cannot be annotated with both SWIFT_RETURNS_RETAINED and SWIFT_RETURNS_UNRETAINED}}
    return NastyService::withID(-1);
  }
};

// Swift infers +1 (free function whose name begins with "copy"), and so does
// libkern
inline Service *_Nonnull copyServiceFreeFunction(Service *_Nonnull service) {
  return Service::withID(service->getID());
}

inline bool checkEqual(Service *_Nullable serviceA, Service *_Nullable serviceB) {
  return serviceA == serviceB;
}
