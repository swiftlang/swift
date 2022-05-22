public protocol MyProto {}
struct StructA : MyProto, Hashable {
    var x: Int
}

struct foo {
    struct bar {
        struct baz {
            struct qux {
                struct quux {
                    struct corge {
                        struct grault {
                            struct garply {
                                struct waldo {
                                    struct fred {
                                        struct plugh {
                                            struct xyzzy {
                                                struct thud {
                                                    struct SomeConformingType : MyProto {
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

extension foo : MyProto {}
