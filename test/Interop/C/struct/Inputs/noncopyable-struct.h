#include "swift/bridging"

typedef struct __attribute__((swift_attr("~Copyable"))) NonCopyable {
  float x, y;
} NonCopyable;

typedef struct SWIFT_NONCOPYABLE_WITH_DESTROY(freeNonCopyableWithDeinit) NonCopyableWithDeinit {
  void *storage;
} NonCopyableWithDeinit;

typedef struct __attribute__((swift_attr("destroy:freeCopyableType"))) CopyableType {
  void *storage;
} CopyableType;

typedef struct __attribute__((swift_attr("~Copyable"))) __attribute__((swift_attr("destroy:freeMultiNonCopyable1"))) __attribute__((swift_attr("destroy:freeMultiNonCopyable2"))) MultiNonCopyableType {
  void *storage;
} MultiNonCopyableType;

typedef struct __attribute__((swift_attr("~Copyable"))) __attribute__((swift_attr("destroy:badDestroy1"))) BadDestroyNonCopyableType {
  void *storage;
} BadDestroyNonCopyableType;

typedef struct __attribute__((swift_attr("~Copyable"))) __attribute__((swift_attr("destroy:badDestroy2"))) BadDestroyNonCopyableType2 {
  void *storage;
} BadDestroyNonCopyableType2;

#ifdef __cplusplus
extern "C" {
#endif

void freeNonCopyableWithDeinit(NonCopyableWithDeinit ncd);
void freeCopyableType(CopyableType ct);


void freeMultiNonCopyable1(MultiNonCopyableType ct);
void freeMultiNonCopyable2(MultiNonCopyableType ct);

void badDestroy1(void);
void badDestroy2(BadDestroyNonCopyableType2 *ptr);

#ifdef __cplusplus

struct __attribute__((swift_attr("~Copyable"))) __attribute__((swift_attr("destroy:extraDestroy"))) ExtraDestroy {
  void *storage;

  ~ExtraDestroy() { }
};

void extraDestroy(ExtraDestroy);
}
#endif
