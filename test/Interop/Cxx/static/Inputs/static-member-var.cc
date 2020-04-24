#include "static-member-var.h"

int WithStaticMember::staticMember = 42;

int *WithStaticMember::getStaticMemberAddress() { return &staticMember; }

int WithStaticMember::getStaticMemberFromCxx() { return staticMember; }

void WithStaticMember::setStaticMemberFromCxx(int newVal) {
  staticMember = newVal;
}

int WithIncompleteStaticMember::arrayMember[3] = {18, 19, 20};

WithIncompleteStaticMember WithIncompleteStaticMember::selfMember =
    WithIncompleteStaticMember();

WithIncompleteStaticMember *
WithIncompleteStaticMember::getStaticMemberFromCxx() {
  return &selfMember;
}

void WithIncompleteStaticMember::setStaticMemberFromCxx(
    WithIncompleteStaticMember newVal) {
  selfMember = newVal;
}

const int WithConstStaticMember::defined;
const int WithConstStaticMember::definedOutOfLine = 96;

int ClassA::notUniqueName = 144;
int ClassB::notUniqueName = 169;
