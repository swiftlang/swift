
@import Foundation;

@interface NSEntityDescription : NSObject
@end

@interface NSManagedObjectContext : NSObject
@end

@interface NSManagedObject : NSObject
- (__kindof NSManagedObject *)initWithEntity:(NSEntityDescription *)entity
              insertIntoManagedObjectContext:(NSManagedObjectContext *)context;
@property(nonatomic, readonly, strong) NSEntityDescription *entity;
+ (NSEntityDescription *)entity;
@end

