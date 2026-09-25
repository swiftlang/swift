typedef void (^VoidBlock)(void);

// A managed (block) field followed by a one-byte field: sizeof 16 with 7 bytes
// of tail padding, non-trivially-destroyable.
struct NestedPadded {
  VoidBlock b;
  char tag;
};

// The padded, non-trivially-destroyable aggregate is followed by another
// managed (block) field, which must be released from offset 16, not 9.
struct HasBlockAfterPadding {
  struct NestedPadded nested;
  VoidBlock b2;
};
