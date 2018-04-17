#import "BlockGlobals.h"

NSString *(^mutableBlockGlobal)(NSString *) = ^ NSString *(NSString *arg) {
  return [@"default mutable block: " stringByAppendingString: arg];
};
NSString *(^ const constBlockGlobal)(NSString *) = ^ NSString *(NSString *arg) {
  return [@"default const block: " stringByAppendingString: arg];
};

static NSString *appendToDefault(NSString *arg) {
  return [@"default mutable FP: " stringByAppendingString: arg];
}

NSString *(*mutableFPGlobal)(NSString *) = &appendToDefault;


static NSString *appendToDefaultConst(NSString *arg) {
  return [@"default const FP: " stringByAppendingString: arg];
}
NSString *(* const constFPGlobal)(NSString *) = &appendToDefaultConst;
