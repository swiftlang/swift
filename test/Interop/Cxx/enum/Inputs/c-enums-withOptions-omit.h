// Enum usage that is bitwise-able and assignable in C++, aka how CF_OPTIONS
// does things.
typedef int __attribute__((availability(swift, unavailable))) NSEnumerationOptions;
enum : NSEnumerationOptions { NSEnumerationConcurrent, NSEnumerationReverse };

@interface NSSet
- (void)enumerateObjectsWithOptions:(NSEnumerationOptions)opts ;
@end
