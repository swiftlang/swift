#ifndef TEST_INTEROP_CXX_NAMESPACE_INPUTS_TEMPLATES_H
#define TEST_INTEROP_CXX_NAMESPACE_INPUTS_TEMPLATES_H

namespace TemplatesNS1 {
template <class T>
const char *basicFunctionTemplate(T)
    __attribute__((swift_attr("import_unsafe"))) {
  return "TemplatesNS1::basicFunctionTemplate";
}

template <class> struct BasicClassTemplate {
  const char *basicMember() __attribute__((swift_attr("import_unsafe"))) {
    return "TemplatesNS1::BasicClassTemplate::basicMember";
  }
};

using BasicClassTemplateChar = BasicClassTemplate<char>;
} // namespace TemplatesNS1

namespace TemplatesNS1 {
namespace TemplatesNS2 {
template <class T> const char *forwardDeclaredFunctionTemplate(T);
template <class> struct ForwardDeclaredClassTemplate;

template <class T> const char *forwardDeclaredFunctionTemplateOutOfLine(T);
template <class> struct ForwardDeclaredClassTemplateOutOfLine;
} // namespace TemplatesNS2
} // namespace TemplatesNS1

namespace TemplatesNS1 {
template <class T>
const char *TemplatesNS2::forwardDeclaredFunctionTemplate(T)
    __attribute__((swift_attr("import_unsafe"))) {
  return "TemplatesNS1::TemplatesNS2::forwardDeclaredFunctionTemplate";
}

template <class> struct TemplatesNS2::ForwardDeclaredClassTemplate {
  const char *basicMember() __attribute__((swift_attr("import_unsafe"))) {
    return "TemplatesNS1::TemplatesNS2::ForwardDeclaredClassTemplate::basicMember";
  }
};

using ForwardDeclaredClassTemplateChar =
    TemplatesNS2::ForwardDeclaredClassTemplate<char>;
} // namespace TemplatesNS1

template <class T>
const char *
TemplatesNS1::TemplatesNS2::forwardDeclaredFunctionTemplateOutOfLine(T) {
  return "TemplatesNS1::TemplatesNS2::forwardDeclaredFunctionTemplateOutOfLine";
}

template <class>
struct TemplatesNS1::TemplatesNS2::ForwardDeclaredClassTemplateOutOfLine {
  const char *basicMember() __attribute__((swift_attr("import_unsafe"))) {
    return "TemplatesNS1::TemplatesNS2::ForwardDeclaredClassTemplateOutOfLine::"
           "basicMember";
  }
};

using ForwardDeclaredClassTemplateOutOfLineChar =
    TemplatesNS1::TemplatesNS2::ForwardDeclaredClassTemplateOutOfLine<char>;

namespace TemplatesNS1 {
namespace TemplatesNS3 {
template <class> struct BasicClassTemplate {};
} // namespace TemplatesNS3
} // namespace TemplatesNS1

namespace TemplatesNS1 {
namespace TemplatesNS2 {
using BasicClassTemplateChar = TemplatesNS3::BasicClassTemplate<char>;
inline const char *takesClassTemplateFromSibling(BasicClassTemplateChar) {
  return "TemplatesNS1::TemplatesNS2::takesClassTemplateFromSibling";
}
} // namespace TemplatesNS2
} // namespace TemplatesNS1

namespace TemplatesNS4 {
template <class> struct HasSpecialization {};

template <> struct HasSpecialization<int> {};
} // namespace TemplatesNS4

namespace TemplatesNS1 {
using UseTemplate = TemplatesNS4::HasSpecialization<char>;
using UseSpecialized = TemplatesNS4::HasSpecialization<int>;
} // namespace TemplatesNS1

namespace TemplatesNS1 {
template <class T> const char *basicFunctionTemplateDefinedInDefs(T);
template <class> struct BasicClassTemplateDefinedInDefs;
} // namespace TemplatesNS1

#endif // TEST_INTEROP_CXX_NAMESPACE_INPUTS_TEMPLATES_H
