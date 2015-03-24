@import Foundation;

static inline NSArray * __nullable getNullable() {
  return nil;
}
static inline NSArray * __nonnull getNonnull() {
  return (NSArray * __nonnull) nil;
}
