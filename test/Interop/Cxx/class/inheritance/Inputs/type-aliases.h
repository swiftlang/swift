template <class From, class To>
To __swift_interopStaticCast(From from) {
  return static_cast<To>(from);
}

struct Base {
  struct Struct {};

  using T = int;
  using U = Struct;
};

struct Derived : Base {};
