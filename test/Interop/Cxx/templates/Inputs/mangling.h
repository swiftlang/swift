#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_MANGLING_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_MANGLING_H

template<typename T>
struct MagicWrapper {};

typedef MagicWrapper<int> WrappedMagicInt;
typedef MagicWrapper<bool> WrappedMagicBool;


#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_MANGLING_H
