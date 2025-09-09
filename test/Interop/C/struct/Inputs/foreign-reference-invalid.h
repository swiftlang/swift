#include <stdlib.h>

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:nonexistent")))
__attribute__((swift_attr("release:nonexistent"))) NonExistent {
  int value;
};

struct __attribute__((swift_attr("import_reference"))) NoRetainRelease {
  int value;
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:badRetain")))
__attribute__((swift_attr("release:badRelease"))) BadRetainRelease {
  int value;
};

float badRetain(struct BadRetainRelease *v);
void badRelease(struct BadRetainRelease *v, int i);
