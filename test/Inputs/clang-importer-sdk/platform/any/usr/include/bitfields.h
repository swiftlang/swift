
typedef struct ModRM {
  unsigned rm: 3;
  unsigned reg: 3;
  unsigned mod: 2;
  unsigned opcode;
} ModRM;

