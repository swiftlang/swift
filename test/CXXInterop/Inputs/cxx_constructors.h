struct ExplicitDefaultConstructor {
  ExplicitDefaultConstructor() noexcept : x(42) {}
  int x;
};

struct ImplicitDefaultConstructor {
  int x = 42;
};

struct MemberOfClassType {
  ImplicitDefaultConstructor member;
};

struct DefaultConstructorDeleted {
  DefaultConstructorDeleted() = delete;
  int &a;
};

struct ConstructorWithParam {
  ConstructorWithParam(int val) noexcept : x(val) {}
  int x;
};

struct PotentiallyThrowingConstructor {
  PotentiallyThrowingConstructor() {}
};
