#pragma once

template<class T>
struct MagicWrapper {};

class __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) Foo {};

using MagicWrapperFrt = MagicWrapper<Foo *>;
using MagicWrapperConstFrt = MagicWrapper<const Foo *>;
using MagicWrapperVolatileFrt = MagicWrapper<volatile Foo *>;
using MagicWrapperVolatileFrtRef = MagicWrapper<volatile Foo &>;
