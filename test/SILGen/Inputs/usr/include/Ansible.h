#import "ObjectiveC.h"

@interface Ansible : NSObject
- (Ansible*)initWithBellsOn:(id)theBells;
@end

static Ansible *NSAnse(Ansible *x) {
  return x;
}

Ansible *NSAnseExternal(Ansible *x) {
  return x;
}

void hasNoPrototype();

static void staticForwardDeclaration(void);
