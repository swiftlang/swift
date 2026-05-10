#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_UNEVALUATED_CONTEXT_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_UNEVALUATED_CONTEXT_H

template <typename _Tp>
_Tp __declval(long);

template <typename _Tp>
struct __declval_protector {
  static const bool __stop = false;
};

template <typename _Tp>
auto declval() noexcept -> decltype(__declval<_Tp>(0)) {
  static_assert(__declval_protector<_Tp>::__stop,
                "declval() must not be used!");
  return __declval<_Tp>(0);
}

inline void stillCalled() {
    static int x = 0;
}

template <class T>
class Vec {
public:
  void push_back(const T &__x) {
    if (!noexcept(declval<T *>()))
      ;
    stillCalled();
  }
};

inline void initVector() {
  Vec<int> vv;
  vv.push_back(0);
}

template <class T>
class UseDeclVal {
public:
    UseDeclVal() {}

    auto declTypeRet() const noexcept -> decltype(declval<T>().method()) {
        return T().method();
    }

    inline int callMethod() const {
        int x = declTypeRet();
        return x;
    }
};

struct StructWithMethod {
    inline int method() {
        return 42;
    }
};

using UseDeclValStruct = UseDeclVal<StructWithMethod>;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_UNEVALUATED_CONTEXT_H
