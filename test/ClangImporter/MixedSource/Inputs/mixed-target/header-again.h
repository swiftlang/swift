@import ObjectiveC;

#import "used-by-both-headers.h"

typedef struct {
  struct Point2D start, end;
} Line;

void doSomethingElse(Line line);
