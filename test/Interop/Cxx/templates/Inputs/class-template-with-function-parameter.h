#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_FUNCTION_ARGUMENT_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_FUNCTION_ARGUMENT_H

template <typename Fn>
class function_wrapper;

template <typename Ret, typename... Params>
class function_wrapper<Ret(Params...)> {
  Ret (*fn)(Params... params) = nullptr;
};

typedef function_wrapper<bool(bool)> FuncBoolToBool;
typedef function_wrapper<bool()> FuncVoidToBool;
typedef function_wrapper<void(bool)> FuncBoolToVoid;
typedef function_wrapper<void()> FuncVoidToVoid;
typedef function_wrapper<int(int)> FuncIntToInt;
typedef function_wrapper<int(int, int)> FuncIntIntToInt;
typedef function_wrapper<void(int, int)> FuncIntIntToVoid;
typedef function_wrapper<void(int, int, bool)> FuncIntIntBoolToVoid;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_WITH_FUNCTION_ARGUMENT_H
