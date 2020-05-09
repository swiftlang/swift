struct Base {
  int dummy;
};

struct Nested {
  int dummy;
} __attribute((swift_name("Base.NestedFromClang")));

struct NestedAndShadowed {
  int dummy;
} __attribute((swift_name("Base.NestedAndShadowed")));

struct NestedAndShadowed getShadowedFromClang();
