#ifndef TEST_INTEROP_CXX_EXTERN_VAR_INPUTS_EXTERN_VAR_H
#define TEST_INTEROP_CXX_EXTERN_VAR_INPUTS_EXTERN_VAR_H

extern int counter;

int getCounterFromCxx();
void setCounterFromCxx(int);

namespace Namespaced {
extern int counter;

int getCounterFromCxx();
void setCounterFromCxx(int);
} // namespace Namespaced

#endif
