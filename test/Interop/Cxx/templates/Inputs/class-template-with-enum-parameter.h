#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_ENUM_PARAMETER_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_ENUM_PARAMETER_H

template <class T>
struct Wrapper {
  T t;
};

enum MyEnum { MyEnum_a, MyEnum_b };
enum class MyEnumClass { a, b };
typedef enum { MyTypedefEnum_a, MyTypedefEnum_b } MyTypedefEnum;

typedef Wrapper<MyEnum> WrappedEnum;
typedef Wrapper<MyEnumClass> WrappedEnumClass;
typedef Wrapper<MyTypedefEnum> WrappedTypedefEnum;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_ENUM_PARAMETER_H
