#import <CoreFoundation/CoreFoundation.h>

typedef bool (^boolBlock)(void);

struct objc_bool_block {
  __unsafe_unretained boolBlock block;
};
