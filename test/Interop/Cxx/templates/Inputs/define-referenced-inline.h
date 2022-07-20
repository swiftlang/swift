#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_DEFINE_REFERENCED_INLINE_TYPES_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_DEFINE_REFERENCED_INLINE_TYPES_H

template <class T>
inline void inlineFn1(T) { }

template <class T>
inline void inlineFn2(T) { }

inline void inlineFn3() { }

template <class T>
struct __attribute__((swift_attr("import_unsafe"))) HasInlineDtor {
  inline ~HasInlineDtor() { inlineFn1(T()); }
};

template<class T>
struct CtorCallsInlineFn {
  CtorCallsInlineFn(T x) { inlineFn2(x); }
};

template<class T>
struct HasInlineStaticMember {
  inline static void member() { inlineFn3(); }
};

template<class T>
struct ChildWithInlineCtorDtor1 {
  inline ChildWithInlineCtorDtor1() { HasInlineStaticMember<T>::member(); }
  inline ~ChildWithInlineCtorDtor1() { HasInlineStaticMember<T>::member(); }
};

template <class T>
struct __attribute__((swift_attr("import_unsafe"))) ChildWithInlineCtorDtor2 {
  inline ChildWithInlineCtorDtor2() { HasInlineStaticMember<T>::member(); }
  inline ~ChildWithInlineCtorDtor2() { HasInlineStaticMember<T>::member(); }
};

template <class T>
struct __attribute__((swift_attr("import_unsafe")))
ParentWithChildWithInlineCtorDtor : ChildWithInlineCtorDtor1<T> {};

template<class T>
struct HolderWithChildWithInlineCtorDtor {
  ChildWithInlineCtorDtor2<T> x;
};

template <class T>
struct __attribute__((swift_attr("import_unsafe"))) DtorCallsInlineMethod {
  inline void unique_name() {}

  ~DtorCallsInlineMethod() {
    unique_name();
  }
};

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_DEFINE_REFERENCED_INLINE_TYPES_H
