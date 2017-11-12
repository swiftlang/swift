#pragma clang assume_nonnull begin

enum {
  AnonymousEnumValue,
  AnonymousEnumRenamed __attribute__((swift_name("AnonymousEnumRenamedSwiftUnversioned")))
};

enum UnknownEnum {
  UnknownEnumValue,
  UnknownEnumRenamed __attribute__((swift_name("UnknownEnumRenamedSwiftUnversioned")))
};

enum __attribute__((enum_extensibility(open))) TrueEnum {
  TrueEnumValue,
  TrueEnumRenamed __attribute__((swift_name("renamedSwiftUnversioned"))),
  TrueEnumAliasRenamed __attribute__((swift_name("aliasRenamedSwiftUnversioned")))
};

enum __attribute__((flag_enum)) OptionyEnum {
  OptionyEnumValue = 1,
  OptionyEnumRenamed __attribute__((swift_name("renamedSwiftUnversioned"))) = 2
};

#pragma clang assume_nonnull end
