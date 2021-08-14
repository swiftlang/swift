template <class T>
struct GetTypeValue {
  static constexpr const bool value = T::value;
};

using Invalid1 = GetTypeValue<int>;

template <class T>
struct GetTypeValueInline {
  inline static constexpr const bool value = T::value;
};

using Invalid2 = GetTypeValueInline<int>;
