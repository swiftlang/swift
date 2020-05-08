#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_MEMBERWISE_INITIALIZER_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_MEMBERWISE_INITIALIZER_H

struct StructPrivateOnly {
private:
  int varPrivate;
};

struct StructPublicOnly {
  int varPublic;
};

struct StructEmptyPrivateSection {
  int varPublic;
private:
};

struct StructPublicAndPrivate {
  int varPublic;
private:
  int varPrivate;
};

struct StructWithUnimportedMemberFunction {
  int varPublic;
  int StructWithUnimportedMemberFunction::* unimportedMemberFunction();
};

class ClassPrivateOnly {
  int varPrivate;
};

class ClassPublicOnly {
public:
  int varPublic;
};

class ClassEmptyPublicSection {
  int varPrivate;
public:
};

class ClassPrivateAndPublic {
  int varPrivate;
public:
  int varPublic;
};

struct ClassWithUnimportedMemberFunction {
public:
  int varPublic;
  int ClassWithUnimportedMemberFunction::* unimportedMemberFunction();
};

#endif
