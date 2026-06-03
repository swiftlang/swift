#define FRT_IMMORTAL                                                           \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:immortal")))                               \
  __attribute__((swift_attr("release:immortal")))

struct FRT_IMMORTAL Base {
  int nonVirtualMethod() const { return 42; }

  static Base &create() {
    static Base instance;
    return instance;
  }
};

struct FRT_IMMORTAL Derived : Base {
  int nonVirtualMethod() const { return 99; }

  static Derived &create() {
    static Derived instance;
    return instance;
  }
};
