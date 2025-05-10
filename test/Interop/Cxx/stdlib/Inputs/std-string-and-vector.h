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
