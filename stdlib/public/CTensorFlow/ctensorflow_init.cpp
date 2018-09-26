#include "ctensorflow_init.h"

#include "tensorflow/c/c_api.h"
#include "tensorflow/c/c_api_experimental.h"
#include "tensorflow/c/eager/c_api.h"
#include "tensorflow/core/platform/init_main.h"

#include <assert.h>
#include <vector>

extern "C" {

void InitTensorFlowRuntime(unsigned char enable_debug_logging,
                           int verbose_level) {
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
  char arg2[] = "--v=?";
  if (verbose_level > 0) {
    assert(verbose_level <= 4);
    arg2[4] = verbose_level + '0';
    my_argv.push_back(&arg2[0]);
  }
  int my_argc = my_argv.size();
  char** tmpArgv = my_argv.data();
  // Initialize GPU devices.
  tensorflow::port::InitMain(/*usage=*/nullptr, &my_argc, &tmpArgv);
}

void *swift_tfc_CreateScalarFloatTensor(int32_t val) {
  auto *tensor =
      TF_AllocateTensor(TF_FLOAT, /*shape.data()*/ nullptr, /*shape.size()*/ 0,
                        TF_DataTypeSize(TF_FLOAT) * 1);
  auto *ptr = reinterpret_cast<char *>(TF_TensorData(tensor));
  *reinterpret_cast<float *>(ptr) = static_cast<float>(val);
  return tensor;
}

void swift_tfc_TFE_Execute(void *op, void **retvals, int32_t *num_retvals,
                           void *status) {
  int int_num_retvals = *num_retvals;
  TFE_Execute(reinterpret_cast<TFE_Op *>(op),
              reinterpret_cast<TFE_TensorHandle **>(retvals), &int_num_retvals,
              reinterpret_cast<TF_Status *>(status));
  *num_retvals = int_num_retvals;
}

}  // extern "C"
