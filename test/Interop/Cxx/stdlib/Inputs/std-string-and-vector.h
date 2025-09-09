#include <string>
#include <vector>

struct Item {
  std::vector<std::string> keys;
  std::vector<std::string> values;
};

inline Item get_item() {
  return {};
}

std::vector<int> makeVecOfInt() { return {1, 2, 3}; }
std::vector<std::string> makeVecOfString() { return {"Hello", "World"}; }

struct S {
private:
  std::string privStr;
  std::vector<std::string> privVec;

public:
  std::string pubStr;
  std::vector<std::string> pubVec;

protected:
  std::string protStr;
  std::vector<std::string> protVec;

public:
  S() : privStr("private"), privVec({"private", "vector"}), 
        pubStr("public"), pubVec({"a", "public", "vector"}), 
        protStr("protected"), protVec({"protected", "vector"}) {}

  std::vector<std::string> getPrivVec() const { return privVec; }
  std::string getProtStr() const { return protStr; }
};
