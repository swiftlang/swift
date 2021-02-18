template <class T>
struct HasTypeWithSelfAsParam {
  using TT = HasTypeWithSelfAsParam<HasTypeWithSelfAsParam<T>>;
};

using WillBeInfinite = HasTypeWithSelfAsParam<int>;
