#pragma once

#define IMMORTAL_FRT                                                           \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:immortal")))                               \
  __attribute__((swift_attr("release:immortal")))

namespace NoAnnotations {

struct RefCountedType {
  int value;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainRefCounted")))
__attribute__((swift_attr("release:releaseRefCounted")));

RefCountedType& getRefCountedByRef(); // expected-note {{annotate 'getRefCountedByRef()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
RefCountedType& createRefCountedByRef(); // expected-note {{annotate 'createRefCountedByRef()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
RefCountedType& copyRefCountedByRef(); // expected-note {{annotate 'copyRefCountedByRef()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}

} // namespace NoAnnotations

void retainRefCounted(NoAnnotations::RefCountedType* obj);
void releaseRefCounted(NoAnnotations::RefCountedType* obj);

namespace APIAnnotations {

struct RefCountedType {
  int value;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainAPIRefCounted")))
__attribute__((swift_attr("release:releaseAPIRefCounted")));

RefCountedType& createByRef() __attribute__((swift_attr("returns_retained")));
RefCountedType& getByRef() __attribute__((swift_attr("returns_unretained")));


} // namespace APIAnnotations

void retainAPIRefCounted(APIAnnotations::RefCountedType* obj);
void releaseAPIRefCounted(APIAnnotations::RefCountedType* obj);

namespace TypeAnnotation {

struct RefCountedType {
  int value;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainTypeRefCounted")))
__attribute__((swift_attr("release:releaseTypeRefCounted")))
__attribute__((swift_attr("returned_as_unretained_by_default")));

RefCountedType& getByRef();
RefCountedType& createByRef();
RefCountedType& copyByRef();

} // namespace TypeAnnotation

void retainTypeRefCounted(TypeAnnotation::RefCountedType* obj);
void releaseTypeRefCounted(TypeAnnotation::RefCountedType* obj);

namespace BothAnnotations {

struct RefCountedType {
  int value;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainBothRefCounted")))
__attribute__((swift_attr("release:releaseBothRefCounted")))
__attribute__((swift_attr("returned_as_unretained_by_default")));

// Note: Type default at type level is unretained, but API annotation overrides
RefCountedType& createByRef() __attribute__((swift_attr("returns_retained")));
RefCountedType& getByRef();

} // namespace BothAnnotations

void retainBothRefCounted(BothAnnotations::RefCountedType* obj);
void releaseBothRefCounted(BothAnnotations::RefCountedType* obj);

struct ImmortalIntBox {
  int value;

  ImmortalIntBox(const ImmortalIntBox &) = delete;

  ImmortalIntBox &builderPattern() { return *this; }
} IMMORTAL_FRT;

static ImmortalIntBox globalImmortalIntBox = {123};
ImmortalIntBox &createImmortalIntBox() { return globalImmortalIntBox; }

struct DerivedImmortalIntBox : ImmortalIntBox {};

static DerivedImmortalIntBox globalDerivedImmortalIntBox = {456};
DerivedImmortalIntBox &createDerivedImmortalIntBox() { return globalDerivedImmortalIntBox; }
