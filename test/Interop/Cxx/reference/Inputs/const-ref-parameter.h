#import <Foundation/Foundation.h>

struct OptionsStruct {
  int intOption;
  float floatOption;
};

@interface OptionsConsumerObjC : NSObject

- (nonnull instancetype)initWithOptions:(const OptionsStruct &)options;
+ (nonnull instancetype)consumerWithOptions:(const OptionsStruct &)options;
+ (int)doThingWithOptions:(const OptionsStruct &)options;
- (float)doOtherThingWithOptions:(const OptionsStruct &)options;

@end

struct OptionsConsumerCxx {
  OptionsConsumerCxx(const OptionsStruct &options);
  static OptionsConsumerCxx build(const OptionsStruct &options);
  static int doThing(const OptionsStruct &options);
  float doOtherThing(const OptionsStruct &options);
};
