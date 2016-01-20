typedef const struct __attribute__((objc_bridge(id))) __MyProblematicObject *MyProblematicObjectRef;

@interface MyProblematicObject
@end

typedef float MyProblematicAlias;
typedef MyProblematicObjectRef MyProblematicAliasRef;
