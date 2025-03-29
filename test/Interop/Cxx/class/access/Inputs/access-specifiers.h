#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_ACCESS_SPECIFIERS_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_ACCESS_SPECIFIERS_H

class PublicPrivate {
public:
  int PublicMemberVar;
  static int PublicStaticMemberVar;
  void publicMemberFunc();

  typedef int PublicTypedef;
  struct PublicStruct {};
  enum PublicEnum { PublicEnumValue1 };
  enum { PublicAnonymousEnumValue1 };
  enum PublicClosedEnum {
    PublicClosedEnumValue1
  } __attribute__((enum_extensibility(closed)));
  enum PublicOpenEnum {
    PublicOpenEnumValue1
  } __attribute__((enum_extensibility(open)));
  enum PublicFlagEnum {} __attribute__((flag_enum));

private:
  int PrivateMemberVar;
  static int PrivateStaticMemberVar;
  void privateMemberFunc() {}

  typedef int PrivateTypedef;
  struct PrivateStruct {};
  enum PrivateEnum { PrivateEnumValue1 };
  enum { PrivateAnonymousEnumValue1 };
  enum PrivateClosedEnum {
    PrivateClosedEnumValue1
  } __attribute__((enum_extensibility(closed)));
  enum PrivateOpenEnum {
    PrivateOpenEnumValue1
  } __attribute__((enum_extensibility(open)));
  enum PrivateFlagEnum {} __attribute__((flag_enum));
};

#endif
