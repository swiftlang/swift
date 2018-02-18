#ifndef SWIFT_STDLIB_CTENSORFLOW_INIT_H_
#define SWIFT_STDLIB_CTENSORFLOW_INIT_H_

#ifdef __cplusplus
extern "C" {
#endif

// Call this API exactly once before any TensorFlow backend/runtime calls.
//
// It sets up device context for any GPU based computation.  When
// `enable_debug_logging` is true, it also dumps TF logging for debugging
// purposes.
extern void InitTensorFlowRuntime(unsigned char enable_debug_logging);

#ifdef __cplusplus
} /* end extern "C" */
#endif

#endif  // SWIFT_STDLIB_CTENSORFLOW_INIT_H_
