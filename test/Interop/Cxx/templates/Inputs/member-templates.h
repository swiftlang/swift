#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_MEMBER_TEMPLATES_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_MEMBER_TEMPLATES_H

struct HasMemberTemplates {
  template <class T> T addSameTypeParams(T a, T b) { return a + b; }

  template <class T, class U> T addMixedTypeParams(T a, U b) { return a + b; }

  template <class T, class U> int addAll(int a, T b, U c) { return a + b + c; }

  template <class T> T passThrough(T val) { return val; }

  template <class T> T passThroughConst(const T val) { return val; }

  template <class T> T passThroughOnConst(T val) const { return val; }

  template <class T> T passThroughConstOnConst(const T val) const {
    return val;
  }

  template <class T> void doNothingConstRef(const T &val) {}

  template <class T> void make42Ref(T &val) {}
};

template <class T> struct TemplateClassWithMemberTemplates {
  T value;

  template <class U> void setValue(U val) { value = val; }

  template<class U> TemplateClassWithMemberTemplates<U> toOtherSpec(const U& u) const {
    return {u};
  }

  TemplateClassWithMemberTemplates(T val) : value(val) {}
};

using IntWrapper = TemplateClassWithMemberTemplates<int>;

struct HasStaticMemberTemplates {
  template <class T> static T add(T a, T b) { return a + b; }
  template <class T, class U> static T addTwoTemplates(T a, U b) { return a + b; }
  template <class T> static T removeReference(T &a) { return a; }
};

template <typename T>
struct MyTemplatedStruct {};

struct HasTemplatedField {
  MyTemplatedStruct<int> x;
};

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_MEMBER_TEMPLATES_H
