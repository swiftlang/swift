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

struct TF_Status;

// Given the tensor shape specified by `num_dims` and `dims`, create an int
// tensor with element values taken from `vals`, where the # elements must be
// equal to the product of the dimension sizes. Each element in `vals` is
// converted to the C data type based on `dtype`. e.g. For dtype = TF_INT8,
// int8_t will be used.
//
// Caller owns the returned tensor.
void *swift_tfc_CreateIntTensor(int32_t num_dims, int64_t *dims, int64_t *vals,
                                int32_t dtype, TF_Status *status);

// Similar to the above API, but creates a float tensor, and no conversion is
// involved on the elements of `vals`.
void *swift_tfc_CreateFloatTensor(int32_t num_dims, int64_t *dims, float *vals,
                                  TF_Status *status);

// Similar to the above API, but creates a scalar string tensor.
void *swift_tfc_CreateScalarStringTensor(char *val, int32_t valLen,
                                         TF_Status *status);

// We removed a redundant argument in the C shape inference API.
// This interface function lets us migrate swift-apis without breakages.
// (This is a temporary function and will be removed.)
void TFE_InferShapes_Transition(
  TFE_Op* op, TF_ShapeAndTypeList* input_shapes,
  TF_Tensor** input_tensors,
  TF_ShapeAndTypeList* input_tensor_as_shapes,
  TF_ShapeAndTypeList** input_resource_shapes_and_types,
  TF_ShapeAndTypeList** output_shapes,
  TF_ShapeAndTypeList*** output_resource_shapes_and_types, TF_Status* status);

#ifdef __cplusplus
} /* end extern "C" */
#endif

#endif  // SWIFT_SRC_SWIFT_STDLIB_CTENSORFLOW_INIT_H_
