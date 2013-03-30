@interface NSObject
+ (NSObject*) alloc;
- (NSObject*) init;
+ (NSObject*) new;
+ (void) load;
@end

@interface Ansible : NSObject
- (Ansible*)initWithBellsOn:(id)theBells;
@end
