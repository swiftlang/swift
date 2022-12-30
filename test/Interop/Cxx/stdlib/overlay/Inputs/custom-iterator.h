#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_CUSTOM_ITERATOR_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_CUSTOM_ITERATOR_H

#include <cstddef>
#include <iterator>

// MARK: Valid iterator types

struct ConstIterator {
private:
  int value;

public:
  using iterator_category = std::input_iterator_tag;
  using value_type = int;
  using pointer = int *;
  using reference = const int &;
  using difference_type = int;

  ConstIterator(int value) : value(value) {}
  ConstIterator(const ConstIterator &other) = default;

  const int &operator*() const { return value; }

  ConstIterator &operator++() {
    value++;
    return *this;
  }
  ConstIterator operator++(int) {
    auto tmp = ConstIterator(value);
    value++;
    return tmp;
  }

  bool operator==(const ConstIterator &other) const {
    return value == other.value;
  }
  bool operator!=(const ConstIterator &other) const {
    return value != other.value;
  }
};

struct ConstRACIterator {
private:
  int value;

public:
  using iterator_category = std::random_access_iterator_tag;
  using value_type = int;
  using pointer = int *;
  using reference = const int &;
  using difference_type = int;

  ConstRACIterator(int value) : value(value) {}
  ConstRACIterator(const ConstRACIterator &other) = default;

  const int &operator*() const { return value; }

  ConstRACIterator &operator++() {
    value++;
    return *this;
  }
  ConstRACIterator operator++(int) {
    auto tmp = ConstRACIterator(value);
    value++;
    return tmp;
  }

  void operator+=(difference_type v) { value += v; }
  void operator-=(difference_type v) { value -= v; }
  ConstRACIterator operator+(difference_type v) const {
    return ConstRACIterator(value + v);
  }
  ConstRACIterator operator-(difference_type v) const {
    return ConstRACIterator(value - v);
  }
  friend ConstRACIterator operator+(difference_type v,
                                    const ConstRACIterator &it) {
    return it + v;
  }
  int operator-(const ConstRACIterator &other) const {
    return value - other.value;
  }

  bool operator<(const ConstRACIterator &other) const {
    return value < other.value;
  }

  bool operator==(const ConstRACIterator &other) const {
    return value == other.value;
  }
  bool operator!=(const ConstRACIterator &other) const {
    return value != other.value;
  }
};

// Same as ConstRACIterator, but operator+= returns a reference to this.
struct ConstRACIteratorRefPlusEq {
private:
  int value;

public:
  using iterator_category = std::random_access_iterator_tag;
  using value_type = int;
  using pointer = int *;
  using reference = const int &;
  using difference_type = int;

  ConstRACIteratorRefPlusEq(int value) : value(value) {}
  ConstRACIteratorRefPlusEq(const ConstRACIteratorRefPlusEq &other) = default;

  const int &operator*() const { return value; }

  ConstRACIteratorRefPlusEq &operator++() {
    value++;
    return *this;
  }
  ConstRACIteratorRefPlusEq operator++(int) {
    auto tmp = ConstRACIteratorRefPlusEq(value);
    value++;
    return tmp;
  }

  ConstRACIteratorRefPlusEq &operator+=(difference_type v) {
    value += v;
    return *this;
  }
  ConstRACIteratorRefPlusEq &operator-=(difference_type v) {
    value -= v;
    return *this;
  }
  ConstRACIteratorRefPlusEq operator+(difference_type v) const {
    return ConstRACIteratorRefPlusEq(value + v);
  }
  ConstRACIteratorRefPlusEq operator-(difference_type v) const {
    return ConstRACIteratorRefPlusEq(value - v);
  }
  friend ConstRACIteratorRefPlusEq
  operator+(difference_type v, const ConstRACIteratorRefPlusEq &it) {
    return it + v;
  }
  int operator-(const ConstRACIteratorRefPlusEq &other) const {
    return value - other.value;
  }

  bool operator<(const ConstRACIteratorRefPlusEq &other) const {
    return value < other.value;
  }

  bool operator==(const ConstRACIteratorRefPlusEq &other) const {
    return value == other.value;
  }
  bool operator!=(const ConstRACIteratorRefPlusEq &other) const {
    return value != other.value;
  }
};

/// Same as ConstIterator, but defines `operator==` as a non-member.
struct ConstIteratorOutOfLineEq {
  int value;

  using iterator_category = std::input_iterator_tag;
  using value_type = int;
  using pointer = int *;
  using reference = int &;
  using difference_type = int;

  ConstIteratorOutOfLineEq(int value) : value(value) {}
  ConstIteratorOutOfLineEq(const ConstIteratorOutOfLineEq &other) = default;

  const int &operator*() const { return value; }

  ConstIteratorOutOfLineEq &operator++() {
    value++;
    return *this;
  }
  ConstIteratorOutOfLineEq operator++(int) {
    auto tmp = ConstIteratorOutOfLineEq(value);
    value++;
    return tmp;
  }
};

bool operator==(const ConstIteratorOutOfLineEq &lhs,
                const ConstIteratorOutOfLineEq &rhs) {
  return lhs.value == rhs.value;
}

bool operator!=(const ConstIteratorOutOfLineEq &lhs,
                const ConstIteratorOutOfLineEq &rhs) {
  return lhs.value != rhs.value;
}

/// Only satisfies the minimal requirements for an iterator.
struct MinimalIterator {
  int value;
  using iterator_category = std::input_iterator_tag;
  const int &operator*() const { return value; }
  MinimalIterator &operator++() {
    value++;
    return *this;
  }
  bool operator==(const MinimalIterator &other) const {
    return value == other.value;
  }
};

struct ForwardIterator {
  int value;
  using iterator_category = std::forward_iterator_tag;
  using reference = const int &;

  const int &operator*() const { return value; }
  ForwardIterator &operator++() {
    value++;
    return *this;
  }
  ForwardIterator operator++(int) {
    ForwardIterator tmp = {value};
    value++;
    return tmp;
  }
  bool operator==(const ForwardIterator &other) const {
    return value == other.value;
  }
};

struct HasCustomIteratorTag {
  struct CustomTag : public std::input_iterator_tag {};

  int value;
  using iterator_category = CustomTag;
  const int &operator*() const { return value; }
  HasCustomIteratorTag &operator++() {
    value++;
    return *this;
  }
  bool operator==(const HasCustomIteratorTag &other) const {
    return value == other.value;
  }
};

struct HasCustomRACIteratorTag {
  struct CustomTag : public std::random_access_iterator_tag {};

  int value;
  using iterator_category = CustomTag;
  const int &operator*() const { return value; }
  HasCustomRACIteratorTag &operator++() {
    value++;
    return *this;
  }
  void operator+=(int x) { value += x; }
  int operator-(const HasCustomRACIteratorTag &x) const {
    return value - x.value;
  }
  bool operator==(const HasCustomRACIteratorTag &other) const {
    return value == other.value;
  }
};

struct HasCustomIteratorTagInline {
  struct iterator_category : public std::input_iterator_tag {};

  int value;
  const int &operator*() const { return value; }
  HasCustomIteratorTagInline &operator++() {
    value++;
    return *this;
  }
  bool operator==(const HasCustomIteratorTagInline &other) const {
    return value == other.value;
  }
};

struct HasTypedefIteratorTag {
  int value;
  typedef std::input_iterator_tag iterator_category;
  const int &operator*() const { return value; }
  HasTypedefIteratorTag &operator++() {
    value++;
    return *this;
  }
  bool operator==(const HasTypedefIteratorTag &other) const {
    return value == other.value;
  }
};

// MARK: Types that are not actually iterators

struct HasNoIteratorCategory {
  int value;
  const int &operator*() const { return value; }
  HasNoIteratorCategory &operator++() {
    value++;
    return *this;
  }
  bool operator==(const HasNoIteratorCategory &other) const {
    return value == other.value;
  }
};

struct HasInvalidIteratorCategory {
  struct NotAnIteratorTag {};

  int value;
  using iterator_category = NotAnIteratorTag;
  const int &operator*() const { return value; }
  HasInvalidIteratorCategory &operator++() {
    value++;
    return *this;
  }
  bool operator==(const HasInvalidIteratorCategory &other) const {
    return value == other.value;
  }
};

struct HasNoEqualEqual {
  int value;
  using iterator_category = std::input_iterator_tag;
  const int &operator*() const { return value; }
  HasNoEqualEqual &operator++() {
    value++;
    return *this;
  }
};

struct HasInvalidEqualEqual {
  int value;
  using iterator_category = std::input_iterator_tag;
  const int &operator*() const { return value; }
  HasInvalidEqualEqual &operator++() {
    value++;
    return *this;
  }
  bool operator==(const int &other) const { // wrong type
    return value == other;
  }
};

struct HasNoIncrementOperator {
  int value;
  using iterator_category = std::input_iterator_tag;
  const int &operator*() const { return value; }
  bool operator==(const HasNoIncrementOperator &other) const {
    return value == other.value;
  }
};

struct HasNoPreIncrementOperator {
  int value;
  using iterator_category = std::input_iterator_tag;
  const int &operator*() const { return value; }
  HasNoPreIncrementOperator operator++(int) { // post-increment
    HasNoPreIncrementOperator tmp = {value};
    value++;
    return tmp;
  }
  bool operator==(const HasNoPreIncrementOperator &other) const {
    return value == other.value;
  }
};

struct HasNoDereferenceOperator {
  int value;
  using iterator_category = std::input_iterator_tag;
  HasNoDereferenceOperator &operator++() {
    value++;
    return *this;
  }
  bool operator==(const HasNoDereferenceOperator &other) const {
    return value == other.value;
  }
};

// MARK: Types that claim to be random access iterators, but actually aren't.

struct HasNoMinusOperator {
  int value;

  using iterator_category = std::random_access_iterator_tag;
  using value_type = int;
  using pointer = int *;
  using reference = const int &;
  using difference_type = int;

  HasNoMinusOperator(int value) : value(value) {}
  HasNoMinusOperator(const HasNoMinusOperator &other) = default;

  const int &operator*() const { return value; }

  HasNoMinusOperator &operator++() {
    value++;
    return *this;
  }
  void operator+=(difference_type v) { value += v; }
  void operator-=(difference_type v) { value -= v; }
  HasNoMinusOperator operator+(difference_type v) const {
    return HasNoMinusOperator(value + v);
  }
  HasNoMinusOperator operator-(difference_type v) const {
    return HasNoMinusOperator(value - v);
  }
  friend HasNoMinusOperator operator+(difference_type v,
                                      const HasNoMinusOperator &it) {
    return it + v;
  }

  bool operator<(const HasNoMinusOperator &other) const {
    return value < other.value;
  }

  bool operator==(const HasNoMinusOperator &other) const {
    return value == other.value;
  }
  bool operator!=(const HasNoMinusOperator &other) const {
    return value != other.value;
  }
};

struct HasNoPlusEqualOperator {
  int value;

  using iterator_category = std::random_access_iterator_tag;
  using value_type = int;
  using pointer = int *;
  using reference = const int &;
  using difference_type = int;

  HasNoPlusEqualOperator(int value) : value(value) {}
  HasNoPlusEqualOperator(const HasNoPlusEqualOperator &other) = default;

  const int &operator*() const { return value; }

  HasNoPlusEqualOperator &operator++() {
    value++;
    return *this;
  }
  void operator-=(difference_type v) { value -= v; }
  HasNoPlusEqualOperator operator+(difference_type v) const {
    return HasNoPlusEqualOperator(value + v);
  }
  HasNoPlusEqualOperator operator-(difference_type v) const {
    return HasNoPlusEqualOperator(value - v);
  }
  friend HasNoPlusEqualOperator operator+(difference_type v,
                                          const HasNoPlusEqualOperator &it) {
    return it + v;
  }
  int operator-(const HasNoPlusEqualOperator &other) const {
    return value - other.value;
  }

  bool operator<(const HasNoPlusEqualOperator &other) const {
    return value < other.value;
  }

  bool operator==(const HasNoPlusEqualOperator &other) const {
    return value == other.value;
  }
  bool operator!=(const HasNoPlusEqualOperator &other) const {
    return value != other.value;
  }
};

// MARK: Templated iterators

template <typename T>
struct TemplatedIterator {
  T value;

  using iterator_category = std::input_iterator_tag;
  using value_type = T;
  using pointer = T *;
  using reference = const T &;
  using difference_type = int;

  TemplatedIterator(int value) : value(value) {}
  TemplatedIterator(const TemplatedIterator &other) = default;

  const int &operator*() const { return value; }

  TemplatedIterator &operator++() {
    value++;
    return *this;
  }
  TemplatedIterator operator++(int) {
    auto tmp = ConstIterator(value);
    value++;
    return tmp;
  }
  bool operator==(const TemplatedIterator &other) const {
    return value == other.value;
  }
};

using TemplatedIteratorInt = TemplatedIterator<int>;

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_CUSTOM_ITERATOR_H