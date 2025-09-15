extern "C" {

inline void inlineFn();

void cacheMiss() { }
void incorrectCacheHit() {
  inlineFn();
}

static void caller() {
  cacheMiss();
  incorrectCacheHit();
}

inline void inlineFn() { }

}

