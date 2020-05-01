#include "inline-static-member-var.h"

int *WithInlineStaticMember::getStaticMemberAddress() { return &staticMember; }

int WithInlineStaticMember::getStaticMemberFromCxx() { return staticMember; }

void WithInlineStaticMember::setStaticMemberFromCxx(int newVal) {
  staticMember = newVal;
}
