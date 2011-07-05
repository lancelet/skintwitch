/** Shader patterns. */

#ifndef PATTERNS_H
#define PATTERHS_H


/* filteredpulsetrain function from ARMan. */
float filteredpulsetrain(float edge, period, x, dx)
{
  /* First normalize so period == 1 and our domain of interest is > 0 */
  float w = dx / period;
  float x0 = x / period - w / 2;
  float x1 = x0 + w;
  float nedge = edge / period;
  
  /* Definite integral of normalised pulsetrain from 0 to t */
  float integral(float t) {
    extern float nedge;
    return ((1-nedge)*floor(t) + max(0, t-floor(t)-nedge));
  }
  
  /* Now we want to integrate the normalised pulsetrain over [x0,x1] */
  return (integral(x1) - integral(x0)) / w;
}


/* filteredpulse function from ARMan. */
float filteredpulse(float edge0, edge1, x, dx)
{
  float x0 = x - dx / 2;
  float x1 = x0 + dx;
  return max(0, (min(x1, edge1) - max(x0, edge0)) / dx);
}


#endif
