@interface Record : NSObject {
    NSString* First;
    NSString* Last;
}

- (NSComparisonResult)compare:(Record *)otherObject;
- (void) setFirst: (NSString*)input;
- (void) setLast: (NSString*)input;
- (NSString*) Firstname;
- (NSString*) Lastname;
@end

