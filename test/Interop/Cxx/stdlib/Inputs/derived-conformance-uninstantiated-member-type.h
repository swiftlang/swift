// The member type aliases name specializations of a class template that
// nothing in this header instantiates. Clang instantiates std::vector<int> for
// Holder's field, but that does not instantiate the __normal_iterator
// specializations the aliases refer to.
//
// Deliberately no member functions: an eagerly imported member (a constructor,
// an operator, or a member function template) that mentions const_iterator
// would instantiate it first and hide the problem.

namespace __gnu_cxx {
template <class Pointer, class Container>
struct __normal_iterator {
  Pointer current;
};
} // namespace __gnu_cxx

namespace std {
template <class T>
class vector {
public:
  typedef T value_type;
  typedef unsigned long size_type;
  typedef __gnu_cxx::__normal_iterator<T *, vector> iterator;
  typedef __gnu_cxx::__normal_iterator<const T *, vector> const_iterator;
};
} // namespace std

struct Holder {
  std::vector<int> v;
};
