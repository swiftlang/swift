#import <string.h>
#import "NSSlowTaggedLocalizedString.h"
#import <dlfcn.h>
#import <objc/runtime.h>
#import <Foundation/Foundation.h>

/*
 This horrific mess is simulating the new-in-macOS-Ventura tagged pointer strings,
 which can have lengths >15 characters, which can cause problems in SmallString,
 which used to assume that would never happen. Once CI is running on Ventura or
 later, this can be rewritten to use a regular NSLocalizedString.
 */

@implementation NSSlowTaggedLocalizedString

+ (instancetype) createTestString {
#if __LP64__
  static dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    [[[NSString alloc] init] release]; //Make sure NSString is initialized
    Class tagClass = objc_lookUpClass("NSTaggedPointerString");
    Class ourClass = [NSSlowTaggedLocalizedString class];
    
    Method fastCString = class_getInstanceMethod(ourClass, @selector(_fastCStringContents:));
    class_replaceMethod(tagClass, @selector(_fastCStringContents:), method_getImplementation(fastCString), method_getTypeEncoding(fastCString));
    
    Method length = class_getInstanceMethod(ourClass, @selector(length));
    class_replaceMethod(tagClass, @selector(length), method_getImplementation(length), method_getTypeEncoding(length));
    
    Method charIndex = class_getInstanceMethod(ourClass, @selector(characterAtIndex:));
    class_replaceMethod(tagClass, @selector(characterAtIndex:), method_getImplementation(charIndex), method_getTypeEncoding(charIndex));
    
    Method fastChars = class_getInstanceMethod(ourClass, @selector(_fastCharacterContents));
    class_replaceMethod(tagClass, @selector(_fastCharacterContents), method_getImplementation(fastChars), method_getTypeEncoding(fastChars));
    
    Method retain = class_getInstanceMethod(ourClass, @selector(retain));
    class_replaceMethod(tagClass, @selector(retain), method_getImplementation(retain), method_getTypeEncoding(retain));
    
    Method release = class_getInstanceMethod(ourClass, @selector(release));
    class_replaceMethod(tagClass, @selector(release), method_getImplementation(release), method_getTypeEncoding(release));
    
    Method typeID = class_getInstanceMethod(ourClass, @selector(_cfTypeID));
    class_replaceMethod(tagClass, @selector(_cfTypeID), method_getImplementation(typeID), method_getTypeEncoding(typeID));
    
    Method description = class_getInstanceMethod(ourClass, @selector(description));
    class_replaceMethod(tagClass, @selector(description), method_getImplementation(description), method_getTypeEncoding(description));
    
    Method getBytes = class_getInstanceMethod(ourClass, @selector(getBytes:maxLength:usedLength:encoding:options:range:remainingRange:));
    class_replaceMethod(tagClass, @selector(getBytes:maxLength:usedLength:encoding:options:range:remainingRange:), method_getImplementation(getBytes), method_getTypeEncoding(getBytes));
  });
  return (NSSlowTaggedLocalizedString *)(void *)CFStringCreateWithCString(NULL, "a", kCFStringEncodingASCII); //make a tagged pointer string
#else
  return nil;
#endif
}

static const char *contents = NULL;

+ (void) setContents: (const char *)newContents {
  const char *oldContents = contents;
  if (newContents) {
    contents = strdup(newContents);
  } else {
    contents = NULL;
  }
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
    abort();
  }
  return (uint16_t)contents[index];
}

- (void *) _fastCharacterContents {
  return nil;
}

- (id) retain { return self; }
- (oneway void) release {}

- (uint64_t)_cfTypeID {
  return 7; //CFString
}

- (id) description {
  return self;
}

- (BOOL)getBytes:(void *)buffer maxLength:(uint64_t)max usedLength:(uint64_t *)used encoding:(uint64_t)encoding options:(uint64_t)options range:(NSRange)range remainingRange:(NSRange *)leftover {
  assert(encoding == 1 /* ASCII */ || encoding == 4 /* UTF8 */);
  strncpy(buffer, contents, max);
  if (strlen(contents) > max) {
    leftover->location = max;
    leftover->length = strlen(contents) - max;
    return false;
  }
  leftover->location = 0;
  leftover->length = 0;
  return true;
}

@end
