#include "default-arguments.h"

ArgTy ArgTy::createZero() { return {0}; }

int HasStaticMethodWithDefaultArg::value = 0;
int HasStaticMethodWithDefaultArg::counter = 0;

bool HasStaticMethodWithDefaultArg::isNonZero(int v) { return v != 0; }
bool HasStaticMethodWithDefaultArg::isNonZeroCounter(int v) { return v != 0; }
bool HasStaticMethodWithDefaultArg::isNonZeroPrivateCounter(int v) { return v != 0; }
