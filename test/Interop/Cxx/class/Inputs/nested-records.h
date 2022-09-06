#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_NESTED_RECORDS_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_NESTED_RECORDS_H

struct S1 {
  struct S2 {
    bool A : 1;
  };
};

struct S3 {
  struct S4 { };
};

union U1 {
    union U2 {};
};

union U3 {
    enum E1 {};
};

union U4 {
    struct S5 {};
};

struct S6 {
    enum E3 {};
};

struct S7 {
  union U5 {
      union U6 {};
  };
};

struct S8 {
  struct S9 {
      union U7 {};
  };
};

struct S10 {
  union U8 {
      enum E4 {};
  };
};

struct HasForwardDeclaredNestedType {
  struct ForwardDeclaredType;
  struct NormalSubType { };
  struct ForwardDeclaredType { };
};

struct HasForwardDeclaredTemplateChild {
  template <class T> struct ForwardDeclaredClassTemplate;
  
  struct DeclaresForwardDeclaredClassTemplateFriend {
    template <class T>
    friend struct HasForwardDeclaredTemplateChild::ForwardDeclaredClassTemplate;
  };
  
  template <class T> struct ForwardDeclaredClassTemplate { };
};


// TODO: Nested class templates (https://github.com/apple/swift/issues/56251).

namespace NestedDeclIsAFirstForwardDeclaration {

struct ForwardDeclaresFriend {
  friend struct ForwardDeclaredFriend;
  friend void takesFriend(struct ForwardDeclaredFriend f);
};

struct ForwardDeclaredFriend { };

inline void takesFriend(ForwardDeclaredFriend b) { }

struct HasNestedForwardDeclaration {
  struct IsNestedForwardDeclaration;
};

struct HasNestedForwardDeclaration::IsNestedForwardDeclaration {
  int a;
};

inline void takesHasNestedForwardDeclaration(HasNestedForwardDeclaration) { }

}

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_NESTED_RECORDS_H
