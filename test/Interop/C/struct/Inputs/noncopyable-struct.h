// RUN: %target-typecheck-verify-swift -I %S/Inputs/

typedef struct __attribute__((swift_attr("~Copyable"))) NonCopyable {
  float x, y;
} NonCopyable;

typedef struct __attribute__((swift_attr("~Copyable"))) __attribute__((swift_attr("destroy:freeNonCopyableWithDeinit"))) NonCopyableWithDeinit {
  void *storage;
} NonCopyableWithDeinit;

#ifdef __cplusplus
extern "C" {
#endif

void freeNonCopyableWithDeinit(NonCopyableWithDeinit ncd);

#ifdef __cplusplus
}
#endif
