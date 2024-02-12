struct Base {
  enum class EnumClass : char { eca = 2, ecb = 3, ecc = 4 };
  enum Enum { ea, eb, ec };

  struct Struct {
    int sa;
    int sb;
  };

  struct Parent {
    struct Child {
      int pca;
    };
  };

  union Union {
    int ua;
    Struct ub;
  };
};

struct Derived : Base {};
