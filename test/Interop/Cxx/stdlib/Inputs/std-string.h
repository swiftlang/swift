#include <string>

struct HasMethodThatReturnsString {
  int value = 111;
  std::string getString() const { return std::to_string(value); }
};

inline std::string takesStringWithDefaultArg(std::string s = "abc") { return s; }

struct StringBox {
  std::string value;

  friend bool operator==(const StringBox &lhs, const std::string &rhs) {
    return lhs.value == rhs;
  }

  friend bool operator==(const std::string &lhs, const StringBox &rhs) {
    return rhs == lhs;
  }
};
