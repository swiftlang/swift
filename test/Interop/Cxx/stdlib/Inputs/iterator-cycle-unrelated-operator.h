// Reproducer for a cycle warning during iterator conformance derivation.
// The cycle requires three structural properties:
// 1. A class-template iterator with a free-function operator==
// 2. An unrelated nested class with a member operator==
// 3. The nested class's parent has a field of a different specialization
//    of the same iterator template
//
// When all three are present, a module-wide operator== scan during
// conformance derivation for one specialization can re-enter a mid-import
// context for the other, triggering a cycle warning.

namespace std {
struct input_iterator_tag {};
} // namespace std

namespace llvm {

template <class T>
struct Iter {
  using iterator_category = std::input_iterator_tag;
  T operator*() const;
  Iter &operator++();
};
template <class T>
bool operator==(Iter<T>, Iter<T>);

struct R {
  Iter<char> Name;
};
struct MC {
  Iter<int> Cache;
  struct It {
    bool operator==(It) const;
  };
};

} // namespace llvm
