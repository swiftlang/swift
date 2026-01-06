#include <memory>

// C++ types that use pointer-to-implementation idiom.

struct HasPIMPL {
private:
  struct I;
  I *ptr;
};

HasPIMPL createHasPIMPL();

struct HasSmartPIMPL {
private:
  struct I;
  std::unique_ptr<I> smart_ptr;

public:
  HasSmartPIMPL();
  HasSmartPIMPL(const HasSmartPIMPL &other);
  ~HasSmartPIMPL();
};

HasSmartPIMPL createHasSmartPIMPL();
