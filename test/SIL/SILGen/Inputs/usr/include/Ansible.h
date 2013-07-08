@interface NSObject
+ (NSObject*) alloc;
- (NSObject*) init;
+ (NSObject*) new;
+ (void) load;
@end

@interface Ansible : NSObject
- (Ansible*)initWithBellsOn:(id)theBells;
@end

Ansible *NSAnse(Ansible *x) {
  return x;
}
