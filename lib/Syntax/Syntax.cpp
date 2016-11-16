
#include "swift/Syntax/Syntax.h"

bool swift::syntax::isWhitespace(char c) {
  switch (c) {
    case ' ':
    case '\t':
    case '\f':
    case '\v':
    case '\n':
    case '\r':
      return true;
    default:
      return false;
  }
}

bool swift::syntax::isNewline(char c) {
  return c == '\n' || c == '\r';
}
