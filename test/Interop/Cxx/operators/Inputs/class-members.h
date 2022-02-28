#ifndef SWIFT_CLASS_MEMBERS_H
#define SWIFT_CLASS_MEMBERS_H

struct ClassMembers {
  int value;
  ClassMembers operator-(ClassMembers rhs) {
    return ClassMembers{.value = value - rhs.value};
  }

  ClassMembers operator+(ClassMembers rhs) {
    return ClassMembers{.value = value + rhs.value};
  }

  int operator()() {
    return value;
  }
  int operator()(int x) {
    return value + x;
  }
  int operator()(int x, int y) {
    return value + x * y;
  }
};

#endif // SWIFT_CLASS_MEMBERS_H
