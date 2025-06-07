typedef unsigned int __uint32_t;
typedef unsigned int u_int32_t;
typedef __uint32_t in_addr_t;

struct in_addr {
  in_addr_t s_addr;
};

#define INADDR_ANY (u_int32_t)0x00000000
