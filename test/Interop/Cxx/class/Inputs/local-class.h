struct DeducedReturnType {
  static constexpr auto returnsLocalClass(int c) noexcept {
    struct Local {
      char chars[2];
    };
    return Local{{(char)c, '\0'}};
  }
};

inline auto localClassInFreeFunction() {
  struct Local {
    int x;
  };
  return Local{42};
}

inline auto nestedLocalClass() {
  struct LocalOuter {
    struct LocalInner {
      int x;
    };
    LocalInner inner;
  };
  return LocalOuter{{42}};
}

inline auto localClassInLambda() {
  auto fn = []() {
    struct LocalInLambda {
      int x;
    };
    return LocalInLambda{42};
  };
  return fn();
}

// Records nested at non-local scope should still be imported.
struct Outer {
  struct Inner {
    int x;
  };
  Inner inner;
};
