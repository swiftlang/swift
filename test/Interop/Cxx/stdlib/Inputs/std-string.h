#include <string>

struct HasMethodThatReturnsString {
  int value = 111;
  std::string getString() const { return std::to_string(value); }
};

inline std::string takesStringWithDefaultArg(std::string s = "abc") { return s; }
inline std::string takesStringConstRefWithDefaultArg(const std::string& s = "def") { return s; }
