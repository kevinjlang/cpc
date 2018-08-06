// Copyright 2018, Kevin Lang, Oath Research

/* do not use ROTATEMACRO with shift = 0 or shift = 64 */
#define ROTATEMACRO(val,shift) (((val) >> (shift)) | ((val) << (64 - (shift))))

/* A prime between 2^63 and 2^64 */
static const u_int64_t k2 = 0x9ae16a3b2f90404fULL;

/* This is a manually inlined version of the computation done by CityHash64() on an 8-byte input */

static inline u_int64_t MyCity64(const u_int64_t b)
{
  u_int64_t mul = k2 + 16;
  u_int64_t a = b + k2;
  u_int64_t c =  ROTATEMACRO(b, 37) * mul + a;
  u_int64_t d = (ROTATEMACRO(a, 25) + b) * mul;
  u_int64_t aa, bb;
  aa = (c ^ d) * mul;
  aa ^= (aa >> 47);
  bb = (d ^ aa) * mul;
  bb ^= (bb >> 47);
  bb *= mul;
  return bb;
}
