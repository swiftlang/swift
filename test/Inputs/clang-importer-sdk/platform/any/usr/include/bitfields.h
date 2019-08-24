
typedef struct ModRM {
  unsigned rm: 3;
  unsigned reg: 3;
  unsigned mod: 2;
  unsigned opcode;
} ModRM;

struct AnonStructWithBitfields {
  struct {
    int first: 2;
    int second: 2;
  } left;
  struct {
    int first: 2;
    int second: 2;
  } right;
};
