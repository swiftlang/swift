#include "variadicBool.h"

int numberOfTrues(int count, va_list arguments) {  
  int i, total;
  total = 0;
  
  for(i = 0; i < count; i++) {
    //passing bool to va_arg causes a warning and bool is promoted to int anyway
    if(va_arg(arguments, int) == true) {
      total += 1;
    }
  }
  
  return total;
}