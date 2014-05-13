
class RC4:
  def __init__(self):
    self.state = [0] * 256
    self.I = 0
    self.J = 0

  def init(self, key):
    for i in xrange(256):
      self.state[i] = i

    j = 0
    for i in xrange(256):
      K = ord(key[i % len(key)])
      S = self.state[i]
      j = (j + S + K) % 256
      self.swapByIndex(i, j)

  def swapByIndex(self, i, j):
    self.state[i], self.state[j] = self.state[j], self.state[i]

  def next(self):
    self.I = (self.I + 1) % 256
    self.J = (self.J + self.state[self.I]) % 256
    self.swapByIndex(self.I, self.J)
    return self.state[(self.state[self.I] + self.state[self.J]) % 256]

  def encrypt(self, data):
    for i in xrange(len(data)):
      data[i] = data[i] ^ self.next()


def benchRC4_internal(messageLen, iterations):
  Secret = "This is my secret message"
  Key    = "This is my key"
  LongData = [0] * messageLen

  for i in xrange(messageLen):
    LongData[i] = ord(Secret[i % len(Secret)])

  Enc = RC4()
  Enc.init(Key)

  for i in xrange(iterations):
    Enc.encrypt(LongData)

benchRC4_internal(5000, 100000)
