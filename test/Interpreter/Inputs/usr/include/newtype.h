
@import ObjectiveC;

@interface NSMyObject : NSObject

-(NSMyObject *)init;
-(void)print;

@property(retain) id lifetimeTracked;

@end

typedef NSMyObject *MyObject __attribute((swift_newtype(struct))) __attribute((swift_name("MyObject")));
