typedef const struct __attribute__((objc_bridge(id))) __MyProblematicObject *MyProblematicObjectRef;

@interface MyProblematicObject
@end

typedef float MyProblematicAlias;
typedef MyProblematicObjectRef MyProblematicAliasRef;

// For a CF type, it's okay to have the underlying struct pointer have
// the same name, since it won't get imported anyway.
typedef struct __attribute__((objc_bridge(id))) NotAProblem *NotAProblemRef;
