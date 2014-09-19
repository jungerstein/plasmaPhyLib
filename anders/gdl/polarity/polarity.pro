; polarity.pro
; The Interface for User's Calculation of polarity relations. 

pro polarity, cs, va, theta, rhoZero
  B = calbzero(va, rhoZero)

  print, '****** FAST WAVE ******'
  c = wavespeed(cs, va, theta, /fast)
  delta = calcpolarity(c, va, theta, rhoZero, /fast)
  druck = pressure(cs, B, delta)
  outputpolar, delta
  outputpressure, druck

  print, '****** SLOW WAVE ******'
  c = wavespeed(cs, va, theta, /slow)
  delta = calcpolarity(c, va, theta, rhoZero, /slow)
  druck = pressure(cs, B, delta)
  outputpolar, delta
  outputpressure, druck
end
