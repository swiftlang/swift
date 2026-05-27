/// "RefKit" is a small, complete, and self-contained header-only library for
/// intrusive reference counting, written for the purposes of testing Swift's
/// foreign reference type annotations.
///
/// To keep things simple, all memory-managed objects will store floats, chosen
/// because they are straightforwardly bridged (for stability), have many values
/// (for testing), and are not integers (to avoid ambiguity with ref counts).
///
/// TODO: weak ptrs
#pragma once

#if EXECUTABLE
// Only include this for executable tests, to keep non-executable tests fast
#include <stdio.h>
#define tracePrintf(s, ...)                                                    \
  printf("%s:%d: " s "\n", __FILE__, __LINE__, ##__VA_ARGS__)
#else
#define tracePrintf(...)
#endif

/// Vend various definitions from here rather than pull them in from std headers
/// and swift/bridging to eliminate external dependencies.
namespace util {

#define SWIFT_SHARED_REFERENCE(_retain, _release)                              \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:" #_retain)))                              \
  __attribute__((swift_attr("release:" #_release)))

#define SWIFT_RETURNED_AS_UNRETAINED_BY_DEFAULT                                \
  __attribute__((swift_attr("returned_as_unretained_by_default")))

#define SWIFT_RETURNS_RETAINED __attribute__((swift_attr("returns_retained")))
#define SWIFT_RETURNS_UNRETAINED                                               \
  __attribute__((swift_attr("returns_unretained")))

using nullptr_t = decltype(nullptr);

template <typename T>
struct remove_reference {
  using type = T;
};
template <typename T>
struct remove_reference<T &> {
  using type = T;
};
template <typename T>
struct remove_reference<T &&> {
  using type = T;
};

template <typename T>
using remove_reference_t = typename remove_reference<T>::type;

template <typename T>
T &&forward(remove_reference_t<T> &t) noexcept {
  return static_cast<T &&>(t);
}
template <typename T>
T &&forward(remove_reference_t<T> &&t) noexcept {
  return static_cast<T &&>(t);
}
} // namespace util

/// Indirection for a type's retain/release functions.
///
/// Can be used to adapt Ref to work with a type T whose retain/release
/// functions are named something other than T::ref() and T::deref().
template <typename T>
struct RetainReleaseTraits {
  static void retain(T *t) { t->ref(); }
  static void release(T *t) { t->deref(); }
};

#define MAKE_RETAIN_RELEASE_FUNCTIONS(Class)                                   \
  inline void ref##Class(Class *v) { v->ref(); }                               \
  inline void deref##Class(Class *v) { v->deref(); }

/// A container for/pointer to a ref-counted object.
///
/// Instead of constructors, this type provides static create*() functions that
/// are named according to whether they retain the constructed object. It also
/// provides explicitly named getter functions.
template <typename T, typename RetainRelease = RetainReleaseTraits<T>>
class Ref {
private:
  T *ptr;

  Ref(util::nullptr_t) : ptr{nullptr} {}
  Ref(T *t) : ptr{t} {}

  /// Assert that the boxed pointer is not null.
  void requireNonnull() const {
    if (!ptr) {
      tracePrintf("error: unexpected nullptr");
      __builtin_trap();
    }
  }
  /// Increment reference count for non-null pointer.
  void doRetain() const {
    if (ptr)
      RetainRelease::retain(ptr);
  }
  /// Decrement reference count for non-null pointer.
  void doRelease() const {
    if (ptr)
      RetainRelease::release(ptr);
  }

  /// Copy/move/destroy operations.
public:
  Ref(const Ref &o) : ptr{o.ptr} { doRetain(); }
  Ref(Ref &&o) : ptr{o.ptr} { o.ptr = nullptr; }
  Ref &operator=(const Ref &o) {
    doRelease();
    ptr = o.ptr;
    doRetain();
    return *this;
  }
  Ref &operator=(Ref &&o) {
    doRelease();
    ptr = o.ptr;
    o.ptr = nullptr;
    return *this;
  }
  ~Ref() { doRelease(); }

  /// Factory functions.
public:
  /// Construct a null ref
  static Ref createNull() { return Ref(nullptr); }

  /// Construct a ref without an additional retain operation.
  ///
  /// This is the right factory function to use for RefCounted classes since
  /// those have their reference count initialized at 1.
  static Ref create(float v) { return Ref(new T(v)); }

  /// Construct a ref with an additional retain operation.
  ///
  /// This is called "Leaked" because for RefCounted classes whose reference
  /// count starts at 1, the additional retain will leak the object (unless it
  /// is released).
  static Ref createLeaked(float v) {
    auto r = Ref(v);
    r.doRetain();
    return r;
  }

  /// Some helper methods
public:
  /// Whether this Ref contains a null pointer
  bool isNull() const { return ptr == nullptr; }

  /// A non-const method used to fake out the Swift optimizer and ensure the
  /// copy constructor is called.
  void fakeMutation() {}

  /// Getters of various sorts, with and without a retain operation, with and
  /// without the swift_attr.
  ///
  /// getPtr*() may return nullptr; getRef*() returns a non-null reference.
public:
  T *getPtrUnretained() const { return ptr; }
  T *getPtrRetained() const {
    doRetain();
    return ptr;
  }
  T &getRefUnretained() const {
    requireNonnull();
    return *ptr;
  }
  T &getRefRetained() const {
    requireNonnull();
    doRetain();
    return *ptr;
  }

  SWIFT_RETURNS_UNRETAINED T *getPtrUnretainedAnnotated() const { return ptr; }
  SWIFT_RETURNS_RETAINED T *getPtrRetainedAnnotated() const {
    doRetain();
    return ptr;
  }
  SWIFT_RETURNS_UNRETAINED T &getRefUnretainedAnnotated() const {
    requireNonnull();
    return *ptr;
  }
  SWIFT_RETURNS_RETAINED T &getRefRetainedAnnotated() const {
    requireNonnull();
    doRetain();
    return *ptr;
  }
};

class AbstractRefCounted {
public:
  virtual void ref() const = 0;
  virtual void deref() const = 0;

protected:
  virtual ~AbstractRefCounted() = default;
};

/// Global counter of RefCounted objects that can be used for assertions.
static int globalCount = 0;

class RefCountedBase {
  mutable unsigned x_canary_pre = 0xcafe;
  /// The ref count, surrounded by some canaries to guard against/make it more
  /// obvious when we hit any kind of memory corruption.
  mutable int m_count = 1;
  mutable unsigned x_canary_post = 0xbabe;

  void x_canary_check() const {
    if (x_canary_pre != 0xcafe) {
      tracePrintf("error: clobbered x_canary_pre: expected 0xcafe, got 0x%x",
                  x_canary_pre);
      __builtin_trap();
    }

    if (x_canary_post != 0xbabe) {
      tracePrintf("error: clobbered x_canary_post: expected 0xbabe, got 0x%x",
                  x_canary_post);
      __builtin_trap();
    }
  }

public:
  RefCountedBase() {
    ++globalCount;
    x_canary_check();
  }
  virtual ~RefCountedBase() {
    --globalCount;
    x_canary_check();
  }

  int refCount() const {
    x_canary_check();
    return m_count;
  }

  void ref() const {
    ++m_count;
    x_canary_check();
  }

  bool derefBase() const {
    --m_count;
    if (m_count < 0) {
      tracePrintf("error: reference count dropped below 0: %d", m_count);
      __builtin_trap();
    }
    x_canary_check();
    return m_count == 0;
  }
};

template <typename T>
class RefCounted : public RefCountedBase {
public:
  void deref() const {
    if (!derefBase())
      return;
    delete const_cast<T *>(static_cast<const T *>(this));
  }
} SWIFT_RETURNED_AS_UNRETAINED_BY_DEFAULT;

class Object : public RefCounted<Object> {
  // noncopyable
  Object(const Object &) = delete;
  Object &operator=(const Object &) = delete;

protected:
  Object() : RefCounted() { tracePrintf("Object  -> %p", this); };

public:
  enum class Type : unsigned char { SomeType };
  virtual ~Object() { tracePrintf("~Object <- %p", this); }
} SWIFT_SHARED_REFERENCE(refObject, derefObject);

MAKE_RETAIN_RELEASE_FUNCTIONS(Object)

using RefObject = Ref<Object>;

template <Object::Type ty>
class TypedObject : public Object {
protected:
  TypedObject() = default;
};

class NumberObj final : public TypedObject<Object::Type::SomeType>,
                        public AbstractRefCounted {
public:
  NumberObj(float v) : value{v} { tracePrintf("NumberObj  -> %p", this); }
  virtual ~NumberObj() { tracePrintf("~NumberObj <- %p", this); }

private:
  float value;

public:
  void ref() const final { TypedObject<Object::Type::SomeType>::ref(); }
  void deref() const final { TypedObject<Object::Type::SomeType>::deref(); }

  float get() const { return value; }
  void set(float v) { value = v; }
} SWIFT_SHARED_REFERENCE(refNumberObj, derefNumberObj);

MAKE_RETAIN_RELEASE_FUNCTIONS(NumberObj)

using RefNumberObj = Ref<NumberObj>;

template <typename T>
class Boxed : public RefCounted<Boxed<T>> {
public:
  template <typename... Arguments>
  Boxed(Arguments &&...arguments)
      : m_value(util::forward<Arguments>(arguments)...) {
    tracePrintf("Boxed<T>  -> %p", this);
  }
  Boxed(const Boxed &) = delete;
  Boxed &operator=(const Boxed &) = delete;
  ~Boxed() { tracePrintf("~Boxed<T> <- %p", this); }

  T &operator*() [[clang::lifetimebound]] { return m_value; }
  const T &operator*() const [[clang::lifetimebound]] { return m_value; }

  void ref() const { RefCounted<Boxed<T>>::ref(); }
  void deref() const { RefCounted<Boxed<T>>::deref(); }

private:
  T m_value;
} SWIFT_SHARED_REFERENCE(.ref, .deref);

using BoxedFloat = Boxed<float>;
using RefBoxedFloat = Ref<Boxed<float>>;
