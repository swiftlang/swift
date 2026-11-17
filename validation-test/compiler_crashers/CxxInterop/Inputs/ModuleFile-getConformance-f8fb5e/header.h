#include <iterator>
namespace ns {
class ListProxy {
  template <class Owner>
  class Iterator {
  public:
    using value_type = int;
    using difference_type = long;
    using pointer = int *;
    using reference = int;
    using iterator_category = std::random_access_iterator_tag;
    Iterator(long i) : index(i) {}
    int operator*() const { return 0; }
    Iterator &operator++() { ++index; return *this; }
    Iterator &operator+=(long n) { index += n; return *this; }
    Iterator &operator-=(long n) { index -= n; return *this; }
    long operator-(const Iterator &o) const { return index - o.index; }
    bool operator==(const Iterator &o) const { return index == o.index; }
    bool operator<(const Iterator &o) const { return index < o.index; }
  private:
    long index;
  };
public:
  Iterator<int> begin() const { return Iterator<int>(0); }
  Iterator<int> end() const { return Iterator<int>(3); }
  int operator[](long i) const { return 0; }
};
using IntListProxy = ListProxy;
}
