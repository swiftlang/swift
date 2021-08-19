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

struct S {};
template <class T>
struct IsSubtypeSame {
  static constexpr const S value = T();
};

using Invalid3 = IsSubtypeSame<int>;
