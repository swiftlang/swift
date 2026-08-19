#ifndef TEST_INTEROP_CXX_CXX_IMPL_FOREIGN_REFERENCE_H
#define TEST_INTEROP_CXX_CXX_IMPL_FOREIGN_REFERENCE_H

// A foreign reference type is a class in Swift and a pointer to the C++ class
// in C++.

struct Node;
void retainNode(Node *_Nonnull);
void releaseNode(Node *_Nonnull);

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainNode")))
__attribute__((swift_attr("release:releaseNode"))) Node {
  int value;

  static Node *_Nonnull passThrough(Node *_Nonnull n)
      __attribute__((swift_attr("returns_retained")));
};

// Parameters

int takesNode(Node *_Nonnull n);
int takesNullableNode(Node *_Nullable n);

// Results. Only a result returned retained (+1) can be implemented in Swift.

Node *_Nonnull returnsRetainedNode(Node *_Nonnull n)
    __attribute__((swift_attr("returns_retained")));
Node *_Nullable returnsNullableRetainedNode(Node *_Nonnull n, int null)
    __attribute__((swift_attr("returns_retained")));
Node *_Nonnull returnsUnretainedNode(Node *_Nonnull n)
    __attribute__((swift_attr("returns_unretained")));
Node *_Nonnull returnsUnannotatedNode(Node *_Nonnull n);

// A foreign reference type returned unretained by default

struct Leaf;
void retainLeaf(Leaf *_Nonnull);
void releaseLeaf(Leaf *_Nonnull);

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainLeaf")))
__attribute__((swift_attr("release:releaseLeaf")))
__attribute__((swift_attr("returned_as_unretained_by_default"))) Leaf {
  int value;
};

Leaf *_Nonnull returnsLeaf(Leaf *_Nonnull l);
Leaf *_Nonnull returnsRetainedLeaf(Leaf *_Nonnull l)
    __attribute__((swift_attr("returns_retained")));

// An immortal foreign reference type is never retained or released

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) Singleton {
  int value;
};

Singleton *_Nonnull returnsSingleton(Singleton *_Nonnull s);

#endif // !TEST_INTEROP_CXX_CXX_IMPL_FOREIGN_REFERENCE_H
