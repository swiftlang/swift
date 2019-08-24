__attribute__((objc_root_class))
@interface MyObject
@end

__attribute__((swift_name("MyObject.init(id:)")))
MyObject *_Nonnull my_object_create(int id);
