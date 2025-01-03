#pragma once

// A wrapper around C++'s static_cast(), which allows Swift to get around interop's current lack of support for inheritance.
template <class I, class O> O cxxCast(I i) { return static_cast<O>(i); }

// A minimal foreign reference type.
struct
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
BaseT {
public:
    bool isBase;
    BaseT() { isBase = true; }
    BaseT(const BaseT &) = delete;
    static BaseT &getBaseT() { static BaseT singleton; return singleton; }
};

// A foreign reference type that is a subclass of BaseT.
struct
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
SubT : BaseT {
public:
    SubT() { isBase = false; }
    SubT(const SubT &) = delete;
    static SubT &getSubT() { static SubT singleton; return singleton; }
};

struct
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
BaseWithVirtualDestructor {
    int baseField = 123;

    BaseWithVirtualDestructor() {}
    virtual ~BaseWithVirtualDestructor() {}
};

struct
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
DerivedWithVirtualDestructor : public BaseWithVirtualDestructor {
    int derivedField = 456;

    DerivedWithVirtualDestructor() : BaseWithVirtualDestructor() {}
    ~DerivedWithVirtualDestructor() override {}
};

struct
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
DerivedOutOfOrder : public BaseT, public DerivedWithVirtualDestructor {
    // DerivedWithVirtualDestructor is the primary base class despite being the
    // second one the list.

    int leafField = 789; 

    DerivedOutOfOrder() = default;
    ~DerivedOutOfOrder() override {}

    static DerivedOutOfOrder& getInstance() {
      static DerivedOutOfOrder singleton;
      return singleton;
    }
};
