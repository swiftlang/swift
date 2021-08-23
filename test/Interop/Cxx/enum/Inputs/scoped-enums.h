enum class ScopedEnumDefined { x = 0, y = 2 };

enum class ScopedEnumBasic { x, y, z };

enum class ScopedEnumCharDefined : char { x = 0, y = 2 };

enum class ScopedEnumUnsignedDefined : unsigned int { x = 0, y = 2 };

enum class ScopedEnumUnsignedLongDefined : unsigned long { x = 0, y = 2 };

enum class ScopedEnumChar : char { x, y, z };

enum class ScopedEnumUnsigned : unsigned int { x, y, z };

enum class ScopedEnumUnsignedLong : unsigned long { x, y, z };

enum class ScopedEnumInt : int { x, y, z };

enum class ScopedEnumNegativeElement : int { x = -1, y = 0, z = 2 };

enum class MiddleDefinedScopedEnum { x, y = 42, z };
