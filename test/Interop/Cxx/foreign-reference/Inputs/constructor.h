#pragma once

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain")))
__attribute__((swift_attr("release:release"))) ImportWithCtor {
  int value = 0;
  int param1 = 0;
  int param2 = 0;

  __attribute__((swift_name("init()")))
  __attribute__((swift_attr("returns_retained")))
  static ImportWithCtor * _Nonnull create() {
    return new ImportWithCtor{1};
  }

  __attribute__((swift_name("init(_:)")))
  __attribute__((swift_attr("returns_retained")))
  static ImportWithCtor * _Nonnull create(int x) {
    return new ImportWithCtor{1, x};
  }

  __attribute__((swift_name("init(_:_:)")))
  __attribute__((swift_attr("returns_retained")))
  static ImportWithCtor * _Nonnull create(int x, int y) {
    return new ImportWithCtor{1, x, y};
  }

  __attribute__((swift_name("init(_:_:_:)")))
  __attribute__((swift_attr("returns_unretained")))
  static ImportWithCtor * _Nonnull create(int x, int y, int z) {
    return new ImportWithCtor{0, x, y};
  }
};

inline void retain(ImportWithCtor * _Nonnull x) {
  x->value++;
}

inline void release(ImportWithCtor * _Nonnull x) {
  if (!--x->value)
    delete x;
}

namespace DefaultCtor {
  struct 
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:retain")))
    __attribute__((swift_attr("release:release")))
  CxxRefTy{
    public:
    int val = 2;
    // TODO: resolve the conflict b/w swift_name("init()") and synthesized function
    // __attribute__((swift_name("init()")))
    // __attribute__((swift_attr("returns_retained")))
    // static CxxRefTy * _Nonnull returnsCxxRefTy() {
    //   return new CxxRefTy{};
    // }
  };
}

void retain(DefaultCtor::CxxRefTy * _Nonnull v) {};
void release(DefaultCtor::CxxRefTy * _Nonnull v) {};