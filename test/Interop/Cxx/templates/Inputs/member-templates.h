struct HasMemberTemplates {
  template <class T> T add(T a, T b) { return a + b; }

  template <class T, class U> T addTwoTemplates(T a, U b) { return a + b; }

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

  TemplateClassWithMemberTemplates(T val) : value(val) {}
};

using IntWrapper = TemplateClassWithMemberTemplates<int>;
