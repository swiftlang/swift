#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_TRANSITIVE_FUNCTION_TEMPLATE_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_TRANSITIVE_FUNCTION_TEMPLATE_H

template<typename T>
inline T increment(T t) {
  return t + 1;
}

template<typename T>
inline T add2(T t) {
  return increment(increment(t));
}

template<typename T>
inline T add3(T t) {
  return increment(add2(t));
}

// template<typename T>
// struct Wrapper {
//   T wrapee;
//   Wrapper(T t) {
//     wrapee = myStdForward(t);
//   }
// };

// template<typename T>
// inline T myStdForward(T param) {
//   return static_cast<T>(param);
// }

// struct MyInt {
//   int value = 42;
// };

// inline int getMyIntValue() {
//   MyInt myInt;
//   return Wrapper<MyInt>(myInt).wrapee.value; }

inline int increment2(int t) {
  return t + 1;
}


struct Wrapper {
  int wrapee;
  Wrapper(int t) : wrapee(increment2(t)) {}
};

inline int getMyIntValue() {
  return Wrapper(41).wrapee;
}

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_TRANSITIVE_FUNCTION_TEMPLATE_H
