#ifndef TEST_INTEROP_CXX_NAMESPACE_INPUTS_TEMPLATES_SECOND_HEADER_H
#define TEST_INTEROP_CXX_NAMESPACE_INPUTS_TEMPLATES_SECOND_HEADER_H

template <class T>
const char *TemplatesNS1::basicFunctionTemplateDefinedInDefs(T) {
  return "TemplatesNS1::basicFunctionTemplateDefinedInDefs";
}

template <class> struct TemplatesNS1::BasicClassTemplateDefinedInDefs {
  const char *basicMember() {
    return "TemplatesNS1::BasicClassTemplateDefinedInDefs::basicMember";
  }
};

using BasicClassTemplateDefinedInDefsChar =
    TemplatesNS1::BasicClassTemplateDefinedInDefs<char>;

#endif // TEST_INTEROP_CXX_NAMESPACE_INPUTS_TEMPLATES_SECOND_HEADER_H
