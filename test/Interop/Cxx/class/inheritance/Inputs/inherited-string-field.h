#include <string>

struct BaseWithVirtualAndString {
  std::string sauce = "";
  int64_t id = 0;
  std::string type = "";
  virtual ~BaseWithVirtualAndString() = default;
};

struct DerivedWithStringField : public BaseWithVirtualAndString {
  std::string data;
};

inline DerivedWithStringField makeDerivedWithLongStrings() {
  DerivedWithStringField x;
  x.data = "this has 23 characters.";
  x.sauce = "this has 23 characters!";
  x.id = 42;
  x.type = "this has 22 characters";
  return x;
}

inline DerivedWithStringField makeDerivedWithShortStrings() {
  DerivedWithStringField x;
  x.data = "short";
  x.sauce = "short!";
  x.id = 42;
  x.type = "s";
  return x;
}
