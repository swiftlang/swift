* %

* optional: DDDD$     selects the argument index, 1-based

  argument index increases by 1 otherwise

* optional flags from "#0- +'"

* optional separator from ",;:_"

* optional min width from "0123456789"

* optional precision "." or ".<digit-string>"

* optional length modifier from hh,h,l,ll,j,t,z,q

         Modifier          d, i           o, u, x, X            n
         hh                signed char    unsigned char         signed char *
         h                 short          unsigned short        short *
         l (ell)           long           unsigned long         long *
         ll (ell ell)      long long      unsigned long long    long long *
         j                 intmax_t       uintmax_t             intmax_t *
         t                 ptrdiff_t      (see note)            ptrdiff_t *
         z                 (see note)     size_t                (see note)
         q (deprecated)    quad_t         u_quad_t              quad_t *


         The following length modifier is valid for the a, A, e, E, f, F, g, or G
         conversion:

         Modifier    a, A, e, E, f, F, g, G
         l (ell)     double (ignored, same behavior as without it)
         L           long double

         The following length modifier is valid for the c or s conversion:

         Modifier    c         s
         l (ell)     wint_t    wchar_t *

* conversion type
  diouxX => int
  DOU => long int (deprecated)
  eE => double
  fF => double
  gG => double
  aA => double
  C => wint_t
  c => int
  S => wchar_t*
  p => void*
  n -> int*
  % no conversion

va_double(block: void ^(va_list), )
