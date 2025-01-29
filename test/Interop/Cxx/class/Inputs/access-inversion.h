#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_ACCESS_INVERSION_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_ACCESS_INVERSION_H

/// A record whose public members expose private members
struct Leaky {
public: Leaky() {} // Apparently necessary to ensure constructor is unambiguous in Swift

private:
  typedef bool PrivateAlias;

  struct PrivateRec { public: void privateRecMethod() const {} };

  enum PrivateEnum { privateEnumMember };

  enum class PrivateEnumClass { privateEnumClassMember };

  static const bool PrivateConst = true;

  // These are used as return values in functions that return private types
  static PrivateAlias privateAliasVal;
  static PrivateRec privateRecVal;
  static PrivateEnum privateEnumVal;
  static PrivateEnumClass privateEnumClassVal;

public:
  typedef PrivateAlias AliasToPrivateAlias;
  typedef PrivateRec AliasToPrivateRec;
  typedef PrivateEnum AliasToPrivateEnum;
  typedef PrivateEnumClass AliasToPrivateEnumClass;

  struct RecWithPrivateAlias {
    PrivateAlias mem;
  };
  struct RecWithPrivateRec {
    PrivateRec mem;
  };
  struct RecWithPrivateEnum {
    PrivateEnum mem;
  };
  struct RecWithPrivateEnumClass {
    PrivateEnumClass mem;
  };
  struct RecWithPrivateConst {
    const bool mem = PrivateConst;
  };

  static PrivateAlias staticReturningPrivateAlias() { return privateAliasVal; }
  static PrivateRec staticReturningPrivateRec()     { return privateRecVal; }
  static PrivateEnum staticReturningPrivateEnum()   { return privateEnumVal; }
  static PrivateEnumClass staticReturningPrivateEnumClass()   { return privateEnumClassVal; }

  static void staticTakingPrivateAlias(PrivateAlias p){}
  static void staticTakingPrivateRec(PrivateRec p)    {}
  static void staticTakingPrivateEnum(PrivateEnum p)  {}
  static void staticTakingPrivateEnumClass(PrivateEnumClass p)  {}

  PrivateAlias methodReturningPrivateAlias() const { return privateAliasVal; }
  PrivateRec methodReturningPrivateRec() const { return privateRecVal; }
  PrivateEnum methodReturningPrivateEnum() const { return privateEnumVal; }
  PrivateEnumClass methodReturningPrivateEnumClass() const { return privateEnumClassVal; }

  void methodTakingPrivateAlias(PrivateAlias p) const {};
  void methodTakingPrivateRec(PrivateRec p) const {} ;
  void methodTakingPrivateEnum(PrivateEnum p) const {};
  void methodTakingPrivateEnumClass(PrivateEnumClass p) const {};
};

#endif
