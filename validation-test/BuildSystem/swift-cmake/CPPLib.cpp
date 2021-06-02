
#include <cstdio>
#include "CPPLib.h"

extern "C" {
  void CPPLib_log() {
    printf("I am in cpplib log!\n");
  }
}
