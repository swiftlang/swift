#include "swift/stdlib/ctensorflow_init.h"

#include "tensorflow/core/platform/init_main.h"

extern "C" {

void InitTensorFlowRuntime() {
  // Synthesize argc and argv
  int my_argc = 1;
  char arg0[] = "programName";
  char* my_argv[] = {&arg0[0]};
  char** tmpArgv = my_argv;
  // This call is needed to initialize GPU device for Google3. It is a no-op in
  // non-Google3 platforms.
  tensorflow::port::InitMain("", &my_argc, &tmpArgv);
}

}  // extern "C"
