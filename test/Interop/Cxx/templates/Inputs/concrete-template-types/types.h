#pragma once

namespace concrete {
template <class T>
struct Result {
  T value;
};
template <>
struct Result<void> {
  int value;
};
template struct Result<void *>;
template struct Result<const void *>;
template struct Result<void **>;
template struct Result<int>;
template struct Result<const int *>;
template struct Result<Result<void *>>;

inline Result<void *> makePointer() { return {nullptr}; }
inline Result<void> makeVoid() { return {42}; }
inline Result<const void *> makeConstPointer() { return {nullptr}; }
inline Result<void **> makePointerPointer() { return {nullptr}; }
inline Result<int> makeInt() { return {23}; }
inline Result<const int *> makeIntPointer() { return {nullptr}; }
inline Result<Result<void *>> makeNested() { return {{nullptr}}; }

// A specialization first declared, then defined.
template <>
struct Result<short>;
template <>
struct Result<short> {
  int value;
};
inline Result<short> makeShort() { return {7}; }

// Aliases must retain the same identity as direct names and inferred types.
using IntResult = Result<int>;
typedef Result<int> OtherIntResult;

struct Token {
  int value;
};
template struct Result<Token>;
inline Result<Token> makeToken() { return {{9}}; }

// Naming a completed specialization must not instantiate unused members.
template <class T>
struct Lazy {
  int value;
  void unused() { static_assert(sizeof(T) == 0, "unused member instantiated"); }
};
extern template struct Lazy<int>;
inline Lazy<int> makeLazy() { return {11}; }

// These types are never complete. Looking them up must not instantiate them.
template <class T>
struct Poison {
  static_assert(sizeof(T) == 0, "primary template instantiated");
};
Poison<int> *incomplete();
template <>
struct Result<float>;

template <class T>
struct Partial;
template <class T>
struct Partial<T *> {
  int value;
};
template struct Partial<int *>;
inline Partial<int *> makePartial() { return {13}; }

template <class T, class U = int>
struct Pair { // expected-note {{generic struct 'Pair' declared here}}
  int value;
};
template struct Pair<void *, int>;

template <int N>
struct NonType {
  int value;
};
template struct NonType<3>;
template <class... T>
struct Pack {
  int value;
};
template struct Pack<int>;
} // namespace concrete

namespace other {
template <class T>
struct Result {
  int value;
};
template struct Result<void *>;
} // namespace other

namespace concrete {
template <class T>
struct Implicit {
  T value;
};
inline Implicit<int> makeImplicit() { return {17}; }
} // namespace concrete
