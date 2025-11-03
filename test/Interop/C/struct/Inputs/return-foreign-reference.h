#ifndef __RETURN_FOREIGN_REFERENCE_H
#define __RETURN_FOREIGN_REFERENCE_H

struct
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:FRTRetain")))
__attribute__((swift_attr("release:FRTRelease")))
FRT {
  int value;
};

struct Value { int value; };

void FRTRetain(struct FRT *x);
void FRTRelease(struct FRT *x);

struct FRT *getFRTNoAnnotations(void); // expected-note {{annotate}}
struct FRT *getFRTRetained() __attribute__((swift_attr("returns_retained")));
struct FRT *getFRTUnretained() __attribute__((swift_attr("returns_unretained")));

struct FRT *getFRTUnretained() __attribute__((swift_attr("returns_unretained")));

struct FRT *getFRTConflictingAnnotations()
    __attribute__((swift_attr("returns_retained")))
    __attribute__((swift_attr("returns_unretained")));
// expected-error@-3 {{cannot be annotated with both}}

struct Value *getValueRetained() __attribute__((swift_attr("returns_retained")));
// expected-warning@-1 {{should not be annotated}}
struct Value *getValueUnretained() __attribute__((swift_attr("returns_unretained")));
// expected-warning@-1 {{should not be annotated}}

#endif /* __RETURN_FOREIGN_REFERENCE_H */
