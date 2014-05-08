@import ObjectiveC;

#import "used-by-both-headers.h"

typedef struct {
  struct Point start, end;
} Line;

void doSomethingElse(Line line);
