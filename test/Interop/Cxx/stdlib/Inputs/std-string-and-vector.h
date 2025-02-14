#include <string>
#include <vector>

struct Item {
  std::vector<std::string> keys;
  std::vector<std::string> values;
};

inline Item get_item() {
  return {};
}
