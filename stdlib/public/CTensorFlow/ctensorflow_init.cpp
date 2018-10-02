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

void *swift_tfc_CreateScalarIntTensor(int64_t val, int32_t dtype,
                                      TF_Status *status) {
  auto tfDtype = (TF_DataType)dtype;
  auto *tensor =
      TF_AllocateTensor(tfDtype, /*shape.data()*/ nullptr, /*shape.size()*/ 0,
                        TF_DataTypeSize(tfDtype) * 1);
  auto *ptr = reinterpret_cast<char *>(TF_TensorData(tensor));

  switch (tfDtype) {
  case TF_INT8:
    *reinterpret_cast<int8_t *>(ptr) = static_cast<int8_t>(val);
    break;
  case TF_UINT8:
    *reinterpret_cast<uint8_t *>(ptr) = static_cast<uint8_t>(val);
    break;
  case TF_INT16:
    *reinterpret_cast<int16_t *>(ptr) = static_cast<int16_t>(val);
    break;
  case TF_UINT16:
    *reinterpret_cast<uint16_t *>(ptr) = static_cast<uint16_t>(val);
    break;
  case TF_INT32:
    *reinterpret_cast<int32_t *>(ptr) = static_cast<int32_t>(val);
    break;
  case TF_UINT32:
    *reinterpret_cast<uint32_t *>(ptr) = static_cast<uint32_t>(val);
    break;
  case TF_INT64:
    *reinterpret_cast<int64_t *>(ptr) = static_cast<int64_t>(val);
    break;
  case TF_UINT64:
    *reinterpret_cast<uint64_t *>(ptr) = static_cast<uint64_t>(val);
    break;
  default:
    TF_MakeInternalErrorStatus(status, "Unsupported data type");
    return nullptr;
  }
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
