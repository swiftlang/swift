#ifndef SWIFT_SRC_SWIFT_STDLIB_CTENSORFLOW_INIT_H_
#define SWIFT_SRC_SWIFT_STDLIB_CTENSORFLOW_INIT_H_

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Call this API exactly once before any TensorFlow backend/runtime calls.
//
// It sets up device context for any GPU based computation.  When
// `enable_debug_logging` is true, it also dumps TF logging for debugging
// purposes. In that case, when `verbose_level` is positive (must be <= 4), it
// also dumps verbose logs at that level.
extern void InitTensorFlowRuntime(unsigned char enable_debug_logging,
                                  int verbose_level);

//===----------------------------------------------------------------------===//
// - MARK: Runtime functions to be called via IRGen.
//===----------------------------------------------------------------------===//

// Caller owns the returned tensor.
// TODO: Generalize to create tensors from other shapes and dtypes.
void *swift_tfc_CreateScalarFloatTensor(int32_t val);

struct TF_Status;
void *swift_tfc_CreateScalarIntTensor(int64_t val, int32_t dtype,
                                      TF_Status *status);

void swift_tfc_TFE_Execute(void *op, void **retvals, int32_t *num_retvals,
                           void *status);

#ifdef __cplusplus
} /* end extern "C" */
#endif

#endif  // SWIFT_SRC_SWIFT_STDLIB_CTENSORFLOW_INIT_H_
