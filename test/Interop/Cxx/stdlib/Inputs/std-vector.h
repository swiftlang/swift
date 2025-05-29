#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_VECTOR_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_VECTOR_H

#include <vector>
#include <string>

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

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_VECTOR_H
