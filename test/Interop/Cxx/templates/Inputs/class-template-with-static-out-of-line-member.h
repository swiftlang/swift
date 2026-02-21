#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_STATIC_OUT_OF_LINE_MEMBER_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_STATIC_OUT_OF_LINE_MEMBER_H

template <class T>
struct HasStaticOutOfLineMember {
  static int values[];
  
  static int getFirstValue() {
    return values[0];
  }
};

template <class T>
int HasStaticOutOfLineMember<T>::values[123];

typedef HasStaticOutOfLineMember<int> HasStaticOutOfLineMemberInt;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_STATIC_OUT_OF_LINE_MEMBER_H
