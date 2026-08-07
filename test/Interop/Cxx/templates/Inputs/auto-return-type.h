template <typename T>
inline auto canNotDeduce(T a, T b) { return a + b; }

template <typename T>
struct HasMethodReturningAuto {
  T t;

  auto getT() const {
    return t;
  }
  auto getPtrT() const {
    return &t;
  }
  auto getConstant() const {
    return 42.42;
  }

  auto outOfLineDefinition() const;
};

template <typename T>
auto HasMethodReturningAuto<T>::outOfLineDefinition() const {
  return t;
}

using HasMethodReturningAutoInt = HasMethodReturningAuto<int>;
