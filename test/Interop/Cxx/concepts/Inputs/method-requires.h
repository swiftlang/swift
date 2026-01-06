inline void shouldNotBeCalledOrEmitted(int) {}

inline void calledFromConceptBody(int x) { shouldNotBeCalledOrEmitted(x); }
inline void calledFromMethodBody(int x) {}

struct MyStruct {
  template <typename T>
  void foo(T x)
    requires requires(const T x) { calledFromConceptBody(x); }
  {
    calledFromMethodBody(x);
  }
};
