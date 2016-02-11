@import Foundation;

@interface OmitNeedlessWords : NSObject
-(void)jumpToUrl:(nonnull NSURL *)url;
-(BOOL)objectIsCompatibleWithObject:(nonnull id)other;
-(void)insetByX:(NSInteger)x y:(NSInteger)y;
-(void)setIndirectlyToValue:(nonnull id)object;
-(void)jumpToTop:(nonnull id)sender;
-(void)removeWithNoRemorse:(nonnull id)object;
-(void)bookmarkWithURLs:(nonnull NSArray<NSURL *> *)urls;
-(void)saveToURL:(nonnull NSURL *)url forSaveOperation:(NSInteger)operation;
-(void)indexWithItemNamed:(nonnull NSString *)name;
-(void)methodAndReturnError:(NSError **)error;
-(nullable Class)typeOfString:(nonnull NSString *)string;
-(nullable Class)typeOfNamedString:(nonnull NSString *)string;
-(void)appendWithContentsOfString:(nonnull NSString *)string;
@end
