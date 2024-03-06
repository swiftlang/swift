#include <stdint.h>

void *swift_retain_n(void *, uint32_t);
void swift_release_n(void *, uint32_t);
void *swift_nonatomic_retain_n(void *, uint32_t);
void swift_nonatomic_release_n(void *, uint32_t);

void *swift_unownedRetain_n(void *, uint32_t);
void swift_unownedRelease_n(void *, uint32_t);
void *swift_nonatomic_unownedRetain_n(void *, uint32_t);
void swift_nonatomic_unownedRelease_n(void *, uint32_t);

// Wrappers so we can call these from Swift without upsetting the ARC optimizer.
void *wrapper_swift_retain_n(void *obj, uint32_t n) {
  return swift_retain_n(obj, n);
}

void wrapper_swift_release_n(void *obj, uint32_t n) {
  swift_release_n(obj, n);
}

void *wrapper_swift_nonatomic_retain_n(void *obj, uint32_t n) {
  return swift_nonatomic_retain_n(obj, n);
}

void wrapper_swift_nonatomic_release_n(void *obj, uint32_t n) {
  swift_nonatomic_release_n(obj, n);
}

void *wrapper_swift_unownedRetain_n(void *obj, uint32_t n) {
  return swift_unownedRetain_n(obj, n);
}

void wrapper_swift_unownedRelease_n(void *obj, uint32_t n) {
  swift_unownedRelease_n(obj, n);
}

void *wrapper_swift_nonatomic_unownedRetain_n(void *obj, uint32_t n) {
  return swift_nonatomic_unownedRetain_n(obj, n);
}

void wrapper_swift_nonatomic_unownedRelease_n(void *obj, uint32_t n) {
  swift_nonatomic_unownedRelease_n(obj, n);
}


