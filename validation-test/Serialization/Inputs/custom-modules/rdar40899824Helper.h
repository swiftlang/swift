#ifndef BAD
typedef struct {
  int value;
} SoonToBeMissing;
#endif

@interface Impl
#ifndef BAD
- (void)use:(SoonToBeMissing)value;
#endif

- (void)unrelated;
@end
