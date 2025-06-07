#pragma once

void f(int a) __attribute__((enable_if(a > 0, ""))) __attribute__((enable_if(true, "")));
