#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_VARIADIC_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_VARIADIC_H

template <class... Ts> struct Tuple {};

template <>
struct Tuple<> {
  void set() {}
};

template <class T, class... Ts>
struct Tuple<T, Ts...> : Tuple<Ts...> {
  Tuple(T t, Ts... ts) : Tuple<Ts...>(ts...), _t(t) {}

  void set(T t, Ts... ts) { _t = t; Tuple<Ts...>::set(ts...); }

  T first() { return _t; }
  Tuple<Ts...> rest() { return *this; }

  T _t;
};

struct IntWrapper {
  int value;
  int getValue() const { return value; }
};

typedef Tuple<IntWrapper> Single;
typedef Tuple<IntWrapper, IntWrapper> Pair;
typedef Tuple<IntWrapper, IntWrapper, IntWrapper> Triple;
typedef Tuple<Tuple<IntWrapper, IntWrapper>, Tuple<IntWrapper, IntWrapper>> Nested;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_VARIADIC_H
