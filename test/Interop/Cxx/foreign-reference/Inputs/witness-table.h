#ifndef TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_WITNESS_TABLE_H
#define TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_WITNESS_TABLE_H

#include <stdlib.h>
#if defined(_WIN32)
inline void *operator new(size_t, void *p) { return p; }
#else
#include <new>
#endif

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) CxxLinkedList {
  int value = 3;

  CxxLinkedList * _Nullable next() {
    if (value == 3)
      return nullptr;

    return this + 1;
  }
};

CxxLinkedList * _Nonnull makeLinkedList() {
  CxxLinkedList *buff = (CxxLinkedList *)malloc(sizeof(CxxLinkedList) * 4);
  buff[0].value = 0;
  buff[1].value = 1;
  buff[2].value = 2;
  buff[3].value = 3;
  return buff;
}

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) MyCxxSequence {
  CxxLinkedList * _Nullable list = nullptr;

  CxxLinkedList * _Nullable next() {
    if (list->value == 3)
      return nullptr;

    auto * _Nullable tmp = list;
    list = tmp + 1;
    return tmp;
  }
};

MyCxxSequence * _Nonnull makeSequence() {
  CxxLinkedList *buff = (CxxLinkedList *)malloc(sizeof(CxxLinkedList) * 4);
  buff[0].value = 0;
  buff[1].value = 1;
  buff[2].value = 2;
  buff[3].value = 3;

  MyCxxSequence *seq = (MyCxxSequence *)malloc(sizeof(MyCxxSequence));
  seq->list = buff;
  return seq;
}

#endif // TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_WITNESS_TABLE_H
