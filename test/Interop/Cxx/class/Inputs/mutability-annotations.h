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

struct HasMutableProperty {
  mutable int a;
  int b;

  int annotatedNonMutating() const __attribute__((__swift_attr__("nonmutating"))) {
    return b;
  }

  int noAnnotation() const { return b; }

  // expected-warning@+1 {{attribute 'mutating' is ignored when combined with attribute 'nonmutating'}}
  int contradictingAnnotations() const __attribute__((__swift_attr__("nonmutating"))) __attribute__((__swift_attr__("mutating"))) {
    return b;
  }

  int duplicateAnnotations() const __attribute__((__swift_attr__("nonmutating"))) __attribute__((__swift_attr__("nonmutating"))) {
    return b;
  }
};

struct NoMutableProperty {
  int a;

  // expected-warning@+1 {{attribute 'nonmutating' has no effect without any mutable fields}}
  int isConst() const __attribute__((__swift_attr__("nonmutating"))) {
    return a;
  }

  // expected-warning@+2 {{attribute 'nonmutating' has no effect without any mutable fields}}
  // expected-warning@+1 {{attribute 'nonmutating' has no effect on non-const method}}
  int nonConst() __attribute__((__swift_attr__("nonmutating"))) { return a; }
};

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_MUTABILITY_ANNOTATIONS_H
