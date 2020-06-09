#include "static-member-func.h"

int WithStaticMemberFunc::staticMemberFunc() { return 1234; }
WithStaticMemberFunc::Func WithStaticMemberFunc::getStaticMemberFuncAddress() {
  return &WithStaticMemberFunc::staticMemberFunc;
}
