#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_DESTRUCTORS_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_DESTRUCTORS_H

struct
    __attribute__((swift_attr("conforms_to:Testable")))
    HasTest {
  void test() const;
};

struct
    __attribute__((swift_attr("conforms_to:Playable")))
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:immortal")))
    __attribute__((swift_attr("release:immortal")))
    HasPlay {
  void play() const;
};


#endif // TEST_INTEROP_CXX_CLASS_INPUTS_DESTRUCTORS_H
