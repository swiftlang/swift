#ifndef FIELD
#error "FIELD must be defined before including this file"
#endif

// FIELD(TYPE, NAME)

FIELD(int, number)
FIELD(const char *, name)

#undef FIELD
