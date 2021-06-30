#ifndef TEST_INTEROP_CXX_STATIC_INPUTS_STATIC_MEMBER_VAR_H
#define TEST_INTEROP_CXX_STATIC_INPUTS_STATIC_MEMBER_VAR_H

class WithStaticMember {
public:
  static int staticMember;
  static int *getStaticMemberAddress();
  static int getStaticMemberFromCxx();
  static void setStaticMemberFromCxx(int);
};

class WithIncompleteStaticMember {
public:
  static int arrayMember[];
  static WithIncompleteStaticMember selfMember;
  int id = 3;

  static WithIncompleteStaticMember *getStaticMemberFromCxx();
  static void setStaticMemberFromCxx(WithIncompleteStaticMember);
};

class WithConstStaticMember {
public:
  const static int notDefined = 24;
  const static int defined = 48;
  const static int definedOutOfLine;
};

constexpr float getFloatValue() { return 42; }
constexpr float getIntValue(int arg) { return 40 + arg; }

class WithConstexprStaticMember {
public:
  constexpr static int definedInline = 139;
  constexpr static int definedInlineWithArg = getIntValue(2);
  constexpr static float definedInlineFloat = 139;
  constexpr static float definedInlineFromMethod = getFloatValue();
};

class WithStaticAndInstanceMember {
public:
  int myInstance;
  static int myStatic;
};

class ClassA {
public:
  static int notUniqueName;
};

class ClassB {
public:
  static int notUniqueName;
};

#endif
