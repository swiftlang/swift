#include "variadicBool.h"

int numberOfTrues(int count, va_list arguments) {  
  int i, total;
  total = 0;
  
  for(i = 0; i < count; i++) {
    //we're passing int here because passing bool is actually incorrect since 
    //bool is actually promoted to int in C
    if(va_arg(arguments, int) == true) {
      total += 1;
    }
  }
  
  return total;
}