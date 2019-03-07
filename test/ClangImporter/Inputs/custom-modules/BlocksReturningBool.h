typedef unsigned long size_t;
typedef _Bool (^predicate_t)(size_t);
typedef struct {
  void (*takePredicate)(predicate_t);
} Aggregate;
