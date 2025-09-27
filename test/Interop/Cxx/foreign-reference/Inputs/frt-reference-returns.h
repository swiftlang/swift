#pragma once

namespace NoAnnotations {

struct RefCountedType {
  int value;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainRefCounted")))
__attribute__((swift_attr("release:releaseRefCounted")));

RefCountedType& getRefCountedByRef(); // expected-note {{'getRefCountedByRef()' is defined here}}
RefCountedType& createRefCountedByRef(); // expected-note {{'createRefCountedByRef()' is defined here}}
RefCountedType& copyRefCountedByRef(); // expected-note {{'copyRefCountedByRef()' is defined here}}   

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
