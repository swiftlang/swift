#ifndef __CLOSURE__
#define __CLOSURE__

struct NonTrivial {
  NonTrivial() noexcept { p = new int(123); }
  ~NonTrivial() { delete p; }
  NonTrivial(const NonTrivial &other) noexcept {
    p = new int(*other.p);
  }
  int *p;
};

void cfunc(void (^ _Nonnull block)(NonTrivial)) noexcept {
  block(NonTrivial());
}

void cfunc2(void (*_Nonnull fp)(NonTrivial)) noexcept { (*fp)(NonTrivial()); }

NonTrivial cfunc3(NonTrivial, int, NonTrivial);

#if __OBJC__
struct ARCStrong {
  id a;
};

void cfuncARCStrong(void (*_Nonnull)(ARCStrong)) noexcept ;
#endif

void cfuncReturnNonTrivial(NonTrivial (^_Nonnull)()) noexcept;
void cfuncReturnNonTrivial2(NonTrivial (*_Nonnull)()) noexcept;

struct ARCWeak {
#if __OBJC__
  __weak _Nullable id m;
#endif
};

void cfuncARCWeak(void (^ _Nonnull block)(ARCWeak)) noexcept {
  block(ARCWeak());
}

void cfunc(NonTrivial) noexcept;
void cfuncARCWeak(ARCWeak) noexcept;

void (* _Nonnull getFnPtr() noexcept)(NonTrivial) noexcept;
void (* _Nonnull getFnPtr2() noexcept)(ARCWeak) noexcept;

class SharedRef {
public:
  static SharedRef *_Nonnull makeSharedRef() { return new SharedRef(); }
  int _refCount = 1;

private:
  SharedRef() = default;

  SharedRef(const SharedRef &other) = delete;
  SharedRef &operator=(const SharedRef &other) = delete;
  SharedRef(SharedRef &&other) = delete;
  SharedRef &operator=(SharedRef &&other) = delete;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainSharedRef")))
__attribute__((swift_attr("release:releaseSharedRef")));

inline void
cppGo(void (*_Nonnull takeConstSharedRef)(const SharedRef *_Nonnull x)) {
  SharedRef *ref = SharedRef::makeSharedRef();
  takeConstSharedRef(ref);
}

inline void retainSharedRef(SharedRef *_Nonnull x) { x->_refCount += 1; }
inline void releaseSharedRef(SharedRef *_Nonnull x) {
  x->_refCount -= 1;
  if (x->_refCount == 0) {
    delete x;
  }
}

#endif // __CLOSURE__
