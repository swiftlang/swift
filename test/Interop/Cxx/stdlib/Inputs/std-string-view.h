#include <string_view>

static std::string_view staticStringView{"abc210"};
static std::string_view staticEmptyStringView{""};
static std::string_view staticNonASCIIStringView{"тест"};

// UTF-16
static std::u16string_view staticU16StringView{u"abc210"};
static std::u16string_view staticU16EmptyStringView{u""};
static std::u16string_view staticU16NonASCIIStringView{u"тест"};

// UTF-32
static std::u32string_view staticU32StringView{U"abc210"};
static std::u32string_view staticU32EmptyStringView{U""};
static std::u32string_view staticU32NonASCIIStringView{U"тест"};
