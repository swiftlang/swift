#ifndef __OBJC_SUPERCLASS_HACK__
#define __OBJC_SUPERCLASS_HACK__
__attribute__((objc_root_class))
@interface _ObjCSuperClassHack
{
  void *isa;
}
@end
#endif
