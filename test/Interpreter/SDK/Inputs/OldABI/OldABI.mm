#include "OldABI.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <sys/mman.h>
#include <malloc/malloc.h>
#include <objc/runtime.h>
#include <Foundation/Foundation.h>

// Implementation of ObjC classes
// with bits set to mimic the pre-stable Swift ABI
// and additional memory protection to detect mis-use

#if __has_include(<objc/objc-internal.h>)
#include <objc/objc-internal.h>
#else
extern "C" Class objc_initializeClassPair(Class superclass, const char *name, Class cls, Class metacls);
extern "C" id _objc_rootRetain(id);
extern "C" void _objc_rootRelease(id);
extern "C" id _objc_rootAutorelease(id);
extern "C" uintptr_t _objc_rootRetainCount(id);
extern "C" bool _objc_rootTryRetain(id);
extern "C" bool _objc_rootIsDeallocating(id);
#endif

// This class stands in for the pre-stable ABI's SwiftObject.
// Stable Swift's class is named Swift._SwiftObject (but mangled).

__attribute__((objc_root_class))
@interface SwiftObject { id isa; } @end

@implementation SwiftObject
+(void)initialize { }
+(id)allocWithZone:(struct _malloc_zone_t *)zone {
  return class_createInstance(self, 0);
}
+(id)alloc { return [self allocWithZone:nil]; }
+(id)class { return self; }
-(id)class { return object_getClass(self); }
+(id)superclass { return class_getSuperclass(self); }
-(id)superclass { return class_getSuperclass([self class]); }
+(BOOL)isMemberOfClass:(Class)cls { return object_getClass(self) == cls; }
-(BOOL)isMemberOfClass:(Class)cls { return [self class] == cls; }
-(id)self { return self; }
-(BOOL)isProxy { return NO; }
-(struct _malloc_zone_t *)zone { return malloc_default_zone(); }
-(void)doesNotRecognizeSelector:(SEL)sel {
  Class cls = [self class];
  fprintf(stderr, "unrecognized selector %c[%s %s]\n",
          class_isMetaClass(cls) ? '+' : '-',
          class_getName(cls), sel_getName(sel));
  abort();
}

+(id)retain { return self; }
+(void)release { }
+(id)autorelease { return self; }
+(uintptr_t)retainCount { return ~(uintptr_t)0; }
+(BOOL)_tryRetain { return YES; }
+(BOOL)_isDeallocating { return NO; }

-(id)retain { return _objc_rootRetain(self); }
-(void)release { _objc_rootRelease(self); }
-(id)autorelease { return _objc_rootAutorelease(self); }
-(uintptr_t)retainCount { return _objc_rootRetainCount(self); }
-(BOOL)_tryRetain { return _objc_rootTryRetain(self); }
-(BOOL)_isDeallocating { return _objc_rootIsDeallocating(self); }
-(void)dealloc { object_dispose(self); }

-(BOOL)isKindOfClass:(Class)other {
  for (Class cls = object_getClass(self); cls; cls = class_getSuperclass(cls))
    if (cls == other) return YES;
  return NO;
}
+(BOOL)isSubclassOfClass:(Class)other {
  for (Class cls = self; cls; cls = class_getSuperclass(cls))
    if (cls == other) return YES;
  return NO;
}
-(BOOL)respondsToSelector:(SEL)sel {
  if (!sel) return NO;
  return class_respondsToSelector(object_getClass(self), sel);
}
+(BOOL)instancesRespondToSelector:(SEL)sel {
  if (!sel) return NO;
  return class_respondsToSelector(self, sel);
}

-(uintptr_t)hash { return (uintptr_t)self; }
-(BOOL)isEqual:(id)other { return self == other; }
+(NSString *)description { return @"FakeSwiftObject class"; }
-(NSString *)description { return @"FakeSwiftObject instance"; }
-(NSString *)debugDescription { return [self description]; }

- (BOOL)isNSArray__      { return NO; }
- (BOOL)isNSCFConstantString__  { return NO; }
- (BOOL)isNSData__       { return NO; }
- (BOOL)isNSDate__       { return NO; }
- (BOOL)isNSDictionary__ { return NO; }
- (BOOL)isNSObject__     { return NO; }
- (BOOL)isNSOrderedSet__ { return NO; }
- (BOOL)isNSNumber__     { return NO; }
- (BOOL)isNSSet__        { return NO; }
- (BOOL)isNSString__     { return NO; }
- (BOOL)isNSTimeZone__   { return NO; }
- (BOOL)isNSValue__      { return NO; }

@end


static char *AllocTailGuardedPointer(size_t size) {
  // Round up to page boundary.
  size_t writeableSize = (size + PAGE_MAX_SIZE - 1) & ~(PAGE_MAX_SIZE - 1);

  // Allocate writeable memory plus one guard page.
  char *writeableBuffer = (char *)mmap(0, writeableSize + PAGE_MAX_SIZE,
                                       PROT_READ | PROT_WRITE,
                                       MAP_ANON | MAP_PRIVATE, -1, 0);
  if (writeableBuffer == MAP_FAILED) abort();

  // Mark the guard page inaccessible.
  mprotect(writeableBuffer + writeableSize, PAGE_MAX_SIZE, 0);

  // Scribble on the prefix.
  memset(writeableBuffer, 0x55, writeableSize - size);

  // Return the address just before the guard page.
  // FIXME: this doesn't handle alignment properly,
  // but we don't need to for this test's usage.
  return writeableBuffer + writeableSize - size;
}

static Class CreateOldABISubclass(Class supercls, const char *name) {
  // Allocate class and metaclass in tail-guarded memory.
  // If the Swift runtime incorrectly tries to read Swift
  // metadata from this class then it'll crash.
  char *clsbuf  = AllocTailGuardedPointer(5*sizeof(uintptr_t));
  char *metabuf = AllocTailGuardedPointer(5*sizeof(uintptr_t));
  Class result = objc_initializeClassPair(supercls, name,
                                          (Class)clsbuf, (Class)metabuf);

  // Set the old is-Swift bit in the class.
  uintptr_t *words = (uintptr_t *)clsbuf;
  words[4] |= 1;

  return result;
}

static Class FakeOldABIClass;

__attribute__((constructor))
static void initialize(void) {
  FakeOldABIClass = CreateOldABISubclass([SwiftObject class],
                                         "_TtC6OldABI8Subclass");
}

bool CanTestOldABI() {
  // These tests don't work until the stable ABI is using its designed bit.
  // This check can be removed after SWIFT_CLASS_IS_SWIFT_MASK is made
  // static everywhere.
  Class cls = objc_getClass("_TtCs19__EmptyArrayStorage");
  if (!cls) abort();
  uintptr_t *words = (uintptr_t *)cls;
  if ((words[4] & 3) != 2) return false;  // wrong stable is-Swift bit
  return true;
}

id AllocOldABIObject() {
  return [FakeOldABIClass alloc];
}
