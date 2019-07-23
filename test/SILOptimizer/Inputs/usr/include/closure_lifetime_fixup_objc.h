
#define SWIFT_NOESCAPE __attribute__((__noescape__))

typedef void (^block_t)(void);

block_t block_create_noescape(block_t SWIFT_NOESCAPE block);
