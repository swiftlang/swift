struct EmbeddedFilesTy {
#define FIELD(TYPE, NAME) TYPE field_##NAME;
#include "textual.inc"
};
