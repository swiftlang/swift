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

class OSObject {
  mutable int retainCount;
  
protected:
  OSObject() : retainCount(1) {}
  virtual ~OSObject() = default;

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

class RegistryEntry : public OSObject {
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

  // Make sure that leading underscores are stripped from the method's name when
  // we apply libkern's ownership convention
  virtual Service *_Nullable __getProvider(void) const {
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

  // Swift infers +0, but libkern ownership semantics infer +1
  static Service *_Nonnull noAnnotationWithID(int id) {
    return new Service(id);
  }

  virtual Service *_Nonnull virtualNoAnnotationCopyService() const {
    return new Service(id);
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

class DerivedService : public Service {
protected:
  explicit DerivedService(int n) : Service(n) {}

public:
  static LIBKERN_RETURNS_RETAINED DerivedService *_Nonnull derivedWithID(
      int id) {
    return new DerivedService(id);
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

class NonOSService {
  mutable int retainCount = 1;
  int id = 0;

  NonOSService(int n) : id(n) {}

public:
  // Swift infers +0, and this is not a subclass of OSObject, so libkern's
  // ownership rules don't apply.
  static NonOSService *_Nonnull noAnnotationWithID(int id) {
    return new NonOSService(id);
  }

  int getID() const { return id; }

  void retain() const {
    ObjectManager::get().recordRetain();
    ++retainCount;
  }
  void release() const {
    ObjectManager::get().recordRelease();
    if (--retainCount == 0)
      delete this;
  }

} SWIFT_SHARED_REFERENCE(.retain, .release);

class OSIterator : public OSObject {
public:
  // According to libkern's ownership conventions, methods that start with "get"
  // return +0 by default. However, if such a method returns OSIterator or one of
  // its subclasses, then it still returns +1.
  static OSIterator *_Nonnull getIterator() { return new OSIterator(); }
};

class OSCollectionIterator : public OSIterator {
public:
  // The iterator exception applies to subclasses of OSIterator too.
  static OSCollectionIterator *_Nonnull getCollectionIterator() {
    return new OSCollectionIterator();
  }
};
