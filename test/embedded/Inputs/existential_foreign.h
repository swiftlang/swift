#pragma once

typedef struct {
  unsigned long long f1;
  unsigned long long f2;
} SomeCStruct;

static inline SomeCStruct createSomeCStruct() {
    SomeCStruct s;
    s.f1 = 1;
    s.f2 = 2;
    return s;
}

typedef enum {
  caseA,
  caseB,
  caseC
} SomeCEnum;

static inline SomeCEnum createSomeCEnum() {
  return caseA;
}

#define SWIFT_ENUM(_type, _name) enum _name : _type

typedef SWIFT_ENUM(unsigned short, SomeNSEnum) {
  someCaseA,
  someCaseB,
} SomeNSEnum;

static inline SomeNSEnum createSomeNSEnum() {
  return someCaseB;
}
