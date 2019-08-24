@import Foundation;

static inline NSArray *_Nullable getNullable() { return nil; }
static inline NSArray *_Nonnull getNonnull() {
  return (NSArray * _Nonnull) nil;
}
