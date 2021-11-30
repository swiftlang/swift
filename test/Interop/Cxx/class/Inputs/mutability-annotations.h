#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_MUTABILITY_ANNOTATIONS_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_MUTABILITY_ANNOTATIONS_H

struct HasConstMethodAnnotatedAsMutating {
  int a;

  int annotatedMutating() const __attribute__((__swift_attr__("mutating"))) {
    const_cast<HasConstMethodAnnotatedAsMutating *>(this)->a++;
    return a;
  }

  int annotatedMutatingWithOtherAttrs() const __attribute__((__swift_attr__("public"))) __attribute__((__swift_attr__("mutating"))) {
    const_cast<HasConstMethodAnnotatedAsMutating *>(this)->a++;
    return a;
  }
};

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_MUTABILITY_ANNOTATIONS_H
