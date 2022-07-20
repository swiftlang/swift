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

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_CUSTOM_ITERATOR_H