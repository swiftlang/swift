#import <string.h>
#import "NSSlowTaggedLocalizedString.h"
#import <dlfcn.h>

@implementation NSSlowTaggedLocalizedString

uintptr_t *obfuscator;
void (*_objc_registerTaggedPointerClass)(uint16_t tag, Class cls);

+ (instancetype) createTestString {
#if __LP64__
  if (obfuscator == NULL) {
    obfuscator = dlsym(RTLD_DEFAULT, "objc_debug_taggedpointer_obfuscator");
    *obfuscator = 0;
    _objc_registerTaggedPointerClass = dlsym(RTLD_DEFAULT, "_objc_registerTaggedPointerClass");
    (*_objc_registerTaggedPointerClass)(0, self); //0 would be unsafe if we loaded Foundation, but we aren't doing that
  }
#if __x86_64__
  return (id)(void *)(uintptr_t)1; //x86_64 uses the LSB as the tag bit, and we want tag 0
#else
  return (id)(void *)(uintptr_t)(1llu << 63); //MSB everywhere else
#endif
#else
  return nil;
#endif
}

static const char *contents = NULL;

+ (void) setContents: (const char *)newContents {
  const char *oldContents = contents;
  contents = strdup(newContents);
  free((void *)oldContents);
}

- (const char *)_fastCStringContents:(BOOL)nullTerminationRequired {
  return contents;
}

- (uint64_t)length {
  return strlen(contents);
}

- (id)copyWithZone:(id)unused {
  return self;
}

- (uint16_t)characterAtIndex:(NSUInteger)index {
  if (index >= [self length]) {
    //throw the appropriate exception
    abort();
  }
  return (uint16_t)contents[index];
}

- (void *) _fastCharacterContents {
  return nil;
}

@end
