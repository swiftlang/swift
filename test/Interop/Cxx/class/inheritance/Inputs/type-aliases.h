struct Base {
  struct Struct {};

  using T = int;
  using U = Struct;
};

struct Derived : Base {};
