#pragma once

#if defined(UNICODE)
#define F FW
#define V VW
#else
#define F FA
#define V VA
#endif

extern const unsigned int VA;
extern const unsigned long long VW;

void FA(unsigned int);
void FW(unsigned long long);

#define InvalidCall DoesNotExist

extern float UIA;
extern double UIW;

#if defined(UNICODE)
#define UI UIW
#else
#define UI UIA
#endif
