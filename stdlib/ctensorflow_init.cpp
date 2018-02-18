#include "swift/stdlib/ctensorflow_init.h"

#include "tensorflow/core/platform/init_main.h"

#include <vector>

extern "C" {

void InitTensorFlowRuntime(unsigned char enable_debug_logging) {
  // Synthesize argc and argv
  char arg0[] = "dummyProgramName";
  std::vector<char*> my_argv;
  my_argv.push_back(&arg0[0]);
  // This allows us to dump TF logging to the output of a swift binary.
  // We can only dump to stderr, since there is no flag alsologtostdout.
  char arg1[] = "--alsologtostderr";
  if (enable_debug_logging > 0) {
    my_argv.push_back(&arg1[0]);
  }
  int my_argc = my_argv.size();
  char** tmpArgv = my_argv.data();
  // This call is needed to initialize GPU device for Google3. It is a no-op in
  // non-Google3 platforms.
  tensorflow::port::InitMain(/*usage=*/nullptr, &my_argc, &tmpArgv);
}

}  // extern "C"
