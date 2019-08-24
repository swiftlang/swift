#ifndef _MACH_O_DYLD_H_
#define _MACH_O_DYLD_H_

#if __cplusplus
extern "C" {
#endif 

#ifndef ENUM_DYLD_BOOL
#define ENUM_DYLD_BOOL
  #undef FALSE
  #undef TRUE
  enum DYLD_BOOL { FALSE, TRUE };
#endif /* ENUM_DYLD_BOOL */

#if __cplusplus
}
#endif 

#endif /* _MACH_O_DYLD_H_ */
