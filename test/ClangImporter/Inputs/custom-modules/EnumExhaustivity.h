#define MY_ENUM(_name) \
  enum _name _name; \
  enum __attribute__((enum_extensibility(open))) _name
#define MY_EXHAUSTIVE_ENUM(_name) \
  enum _name _name; \
  enum __attribute__((enum_extensibility(closed))) _name

typedef MY_ENUM(RegularEnum) {
  RegularEnumA,
  RegularEnumB
};
typedef MY_EXHAUSTIVE_ENUM(ExhaustiveEnum) {
  ExhaustiveEnumA,
  ExhaustiveEnumB
};
