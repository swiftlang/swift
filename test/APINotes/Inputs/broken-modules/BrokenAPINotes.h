typedef struct ZXSpectrum { unsigned char A, B, C, D, H, L; } ZXSpectrum;

unsigned char ZXSpectrumGetAccumulator(const ZXSpectrum *self);
void ZXSpectrumSetAccumulator(ZXSpectrum *self, unsigned char newValue);

unsigned char ZXSpectrumGetRegister(const ZXSpectrum *self, int which);
void ZXSpectrumSetRegister(ZXSpectrum *self, int which, unsigned char newValue);

unsigned char ZXSpectrumGetMisnamedRegister(const ZXSpectrum *self, int which);
void ZXSpectrumSetMisnamedRegister(ZXSpectrum *self, int which, unsigned char newValue);

void ZXSpectrumHelperReset(void);
