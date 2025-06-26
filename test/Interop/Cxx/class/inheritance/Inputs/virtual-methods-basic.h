// struct Base {
//   virtual void foo() = 0;
//   virtual void virtualRename() const;
// };

#define IMMORTAL_FRT                                                           \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:immortal")))                               \
  __attribute__((swift_attr("release:immortal")))

struct IMMORTAL_FRT Immortal {
public:
  virtual void virtualRename() const
      __attribute__((swift_name("swiftVirtualRename()")));
};

// struct Child : Immortal {
//   public: 
//     void virtualRename() const override __attribute__((swift_name("swiftVirtualRename()"))) {

//     }
// };
