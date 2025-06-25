#define SWIFT_NAME(name) __attribute__((swift_name(name)))

namespace MyNS {
struct NestedStruct {
  int value = 123;
};
}

int nestedStruct_method(MyNS::NestedStruct p) SWIFT_NAME("MyNS.NestedStruct.method(self:)") { return p.value; }
int nestedStruct_methodConstRef(const MyNS::NestedStruct &p) SWIFT_NAME("MyNS.NestedStruct.methodConstRef(self:)") { return p.value + 1; }

namespace MyNS {
namespace MyDeepNS {
struct DeepNestedStruct {
  int value = 456;
};
}
}

int deepNestedStruct_method(MyNS::MyDeepNS::DeepNestedStruct p) SWIFT_NAME("MyNS.MyDeepNS.DeepNestedStruct.method(self:)") { return p.value; }
int deepNestedStruct_methodConstRef(const MyNS::MyDeepNS::DeepNestedStruct &p) SWIFT_NAME("MyNS.MyDeepNS.DeepNestedStruct.methodConstRef(self:)") { return p.value + 2; }

typedef MyNS::MyDeepNS::DeepNestedStruct DeepNestedStructTypedef;

int deepNestedStructTypedef_method(DeepNestedStructTypedef p) SWIFT_NAME("DeepNestedStructTypedef.methodTypedef(self:)") { return p.value + 3; }
int deepNestedStructTypedef_methodQualName(MyNS::MyDeepNS::DeepNestedStruct p) SWIFT_NAME("DeepNestedStructTypedef.methodTypedefQualName(self:)") { return p.value + 4; }
