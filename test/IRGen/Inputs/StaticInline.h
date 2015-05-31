#import <Foundation/Foundation.h>

static inline NSString *staticInlineFun() {
	return [[NSLocale currentLocale] localeIdentifier];
}
