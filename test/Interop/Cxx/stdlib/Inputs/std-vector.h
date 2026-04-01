#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_VECTOR_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_VECTOR_H

#include <string>
#include <vector>

using Vector = std::vector<int>;
using VectorOfString = std::vector<std::string>;

inline Vector initVector() { return {}; }

inline std::string takesVectorOfString(const VectorOfString &v) {
  return v.front();
}

class VectorSubclass: public Vector {
public:
};

class VectorOfStringSubclass : public std::vector<std::string> {
public:
  using std::vector<std::string>::vector;
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) ImmortalRef {
  int value;
  static ImmortalRef *create(int value) { return new ImmortalRef({value}); }
};
using VectorOfImmortalRefPtr = std::vector<ImmortalRef *>;

struct NonCopyable {
  NonCopyable(int n) : number(n) {}
  NonCopyable(const NonCopyable &other) = delete;
  NonCopyable(NonCopyable &&other) = default;
  ~NonCopyable() {}
  int number;
};

using VectorOfNonCopyable = std::vector<NonCopyable>;
using VectorOfPointer = std::vector<NonCopyable *>;

VectorOfNonCopyable makeVectorOfNonCopyable() {
  VectorOfNonCopyable vec;
  vec.emplace_back(1);
  vec.emplace_back(2);
  vec.emplace_back(3);
  return vec;
}

struct HasVector {
  std::vector<NonCopyable> field;
};

struct BaseHasVector : HasVector {};

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_VECTOR_H
