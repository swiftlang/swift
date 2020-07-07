
#ifndef OVERLAYED_H
#define OVERLAYED_H

struct __attribute__((swift_name("Overlayed"))) OVOverlayed {
  double x, y, z;
};

double OVOverlayedInOriginalFunc(struct OVOverlayed s) __attribute__((swift_name("Overlayed.inOriginalFunc(self:)")));

struct OVOverlayed createOverlayed();

#endif
