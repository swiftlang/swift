
#ifndef OVERLAID_H
#define OVERLAID_H

struct __attribute__((swift_name("Overlaid"))) OVOverlaid {
  double x, y, z;
};

double OVOverlaidInOriginalFunc(struct OVOverlaid s) __attribute__((swift_name("Overlaid.inOriginalFunc(self:)")));

struct OVOverlaid createOverlaid();

#endif
