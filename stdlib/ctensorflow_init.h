#ifndef SWIFT_STDLIB_CTENSORFLOW_INIT_H_
#define SWIFT_STDLIB_CTENSORFLOW_INIT_H_

#ifdef __cplusplus
extern "C" {
#endif

// Call this API exactly once, before starting any GPU based computation on
// TensorFlow.
extern void InitTensorFlowRuntime();

#ifdef __cplusplus
} /* end extern "C" */
#endif

#endif  // SWIFT_STDLIB_CTENSORFLOW_INIT_H_
