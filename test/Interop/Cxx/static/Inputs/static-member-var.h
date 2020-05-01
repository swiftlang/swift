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

class WithConstexprStaticMember {
public:
  constexpr static int definedInline = 139;
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
