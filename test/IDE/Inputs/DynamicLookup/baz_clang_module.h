@class NSString;

@interface Baz_Class {
  int baz_Ivar1;
}

- (void)baz_Class_InstanceFunc1;
+ (void)baz_Class_ClassFunc1;

- (id)objectAtIndexedSubscript:(int)idx;

@property NSString *baz_Class_Property1;

@end

@protocol Baz_Protocol

- (void)baz_Protocol_InstanceFunc1;
+ (void)baz_Protocol_ClassFunc1;

- (id)objectForKeyedSubscript:(id)key;

@property NSString *baz_Protocol_Property1;

@end

