enum NonExhaustiveEnum {
  NonExhaustiveEnumA = 0,
  NonExhaustiveEnumB = 1,
  NonExhaustiveEnumC = 2,
} __attribute__((enum_extensibility(open)));

enum NonExhaustiveEnum getExpectedValue(void) {
  return NonExhaustiveEnumB;
}
enum NonExhaustiveEnum getUnexpectedValue(void) {
  return (enum NonExhaustiveEnum)3;
}

enum LyingExhaustiveEnum {
  LyingExhaustiveEnumA = 0,
  LyingExhaustiveEnumB = 1,
  LyingExhaustiveEnumC = 2,
} __attribute__((enum_extensibility(closed)));

enum LyingExhaustiveEnum getExpectedLiarValue(void) {
  return LyingExhaustiveEnumB;
}
enum LyingExhaustiveEnum getUnexpectedLiarValue(void) {
  return (enum LyingExhaustiveEnum)3;
}
