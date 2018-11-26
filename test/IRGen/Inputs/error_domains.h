#define MY_ERROR_ENUM(_type, _name, _domain)                                   \
  enum _name : _type _name;                                                    \
  enum __attribute__((ns_error_domain(_domain))) _name : _type

@class NSString;

extern NSString * const TagDomain1;
typedef MY_ERROR_ENUM(int, TagError1, TagDomain1) {
  Badness
};

extern NSString * const TagDomain2;
typedef MY_ERROR_ENUM(int, TagError2, TagDomain2) {
  Sickness
};

extern NSString * const TypedefDomain1;
typedef enum __attribute__((ns_error_domain(TypedefDomain1))) {
  Wrongness
} TypedefError1;

extern NSString *TypedefDomain2;
typedef enum __attribute__((ns_error_domain(TypedefDomain2))) {
  Illness
} TypedefError2;
