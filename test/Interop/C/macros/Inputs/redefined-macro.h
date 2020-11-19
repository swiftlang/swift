#define FOO 0
#define TMP_FOO FOO
#undef FOO
#define FOO TMP_FOO

#define ONE TWO
#define TWO THREE
#define THREE ONE

#define THE_MAX THREE
#undef THREE
#define THREE THE_MAX

#define BAR 0
#define BAZ BAR