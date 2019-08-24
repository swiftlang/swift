#import "ObjectiveC.h"

typedef void (^AnseCompletion)();
typedef void (^AnseCallback)(AnseCompletion completionBlock);

@interface Ansible : NSObject
- (Ansible*)initWithBellsOn:(id)theBells;
+ (void)anseAsync:(AnseCallback)block;
@end

static Ansible *NSAnse(Ansible *x) {
  return x;
}

Ansible *NSAnseExternal(Ansible *x) {
  return x;
}

void hasNoPrototype();

static void staticForwardDeclaration(void);
