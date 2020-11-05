// Make sure that we can import a type that uses a "dependent type" after being
// specialized (i.e. "size_type" in "Lander").
template <class T> struct Lander;

template <>
struct Lander<void> {};

template <class T> struct Lander {
  typedef unsigned long size_type;
  // Make sure we don't crash here. Before being specialized, "size_type" is
  // technically a depedent type because it expands to "Lander<T>::size_type".
  void test(size_type) { }
};

using Surveyor = Lander<char>;
