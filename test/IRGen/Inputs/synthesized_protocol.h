#ifndef SYNTHESIZED_PROTOCOL_H_
#define SYNTHESIZED_PROTOCOL_H_

typedef enum __attribute__((flag_enum,enum_extensibility(open))) {
  One = 0x0001,
  Two = 0x0002,
  Four = 0x0004,
  Eight = 0x0008,
  Sixteen = 0x0010
} Flags;

#endif /* SYNTHESIZED_PROTOCOL_H_ */
