@interface Record : NSObject {
    NSString* FullName;
}

- (NSComparisonResult)compare:(Record *)otherObject;
- (void) setFullName: (NSString*)input;
- (NSString*) FullName;
@end

